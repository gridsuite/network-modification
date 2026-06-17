# network-modification

[![Actions Status](https://github.com/gridsuite/network-modification/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/gridsuite/network-modification/actions)
[![Coverage Status](https://sonarcloud.io/api/project_badges/measure?project=org.gridsuite%3Anetwork-modification&metric=coverage)](https://sonarcloud.io/component_measures?id=org.gridsuite%3Anetwork-modification&metric=coverage)
[![MPL-2.0 License](https://img.shields.io/badge/license-MPL_2.0-blue.svg)](https://www.mozilla.org/en-US/MPL/2.0/)

Library for applying network modifications on a network.

## Architecture

This library is organized around two distinct responsibilities that must not be confused:

- **Describing** a modification — the serializable input data exchanged over REST and persisted by the
  server (the `dto` package, suffixed `*Infos`).
- **Applying** a modification — the executable logic that mutates a PowSyBl `Network` (the `modifications`
  package).

Keeping these two concerns separated is the central design goal: it lets the *execution* code stand on its
own, ultimately as a standalone library that does not need the server-oriented DTO layer.

### Package layout

```
org.gridsuite.modification
├── (root)            Cross-cutting contracts & enums: NetworkModificationException,
│                     ModificationType, IFilterService, ILoadFlowService, VariationMode, …
├── modifications     The execution layer. One class per modification (e.g. LoadCreation),
│   │                 each extending AbstractModification and implementing apply(Network, …).
│   ├── data          Plain data holders that belong to the execution layer but are not yet
│   │                 moved out of dto (see "Shared data objects" below).
│   ├── byfilter      "By filter" / "by formula" modification execution.
│   ├── tabular       Tabular (bulk) modification execution — special case, see below.
│   └── olg           Operational-limits-group execution helpers.
├── dto               The description layer. One *Infos class per modification, all extending
│   │                 ModificationInfos. REST contract (Swagger) + JPA/serialization model.
│   ├── annotation    DTO annotations (@ModificationErrorTypeName, …).
│   ├── byfilter      DTO for by-filter/by-formula modifications.
│   └── tabular       DTO for tabular modifications.
├── utils             Stateless helpers operating on the network (ModificationUtils, …).
└── report            PowSyBl report resource bundle.
```

### The two layers and the bridge between them

Every modification exists as a pair:

| Layer        | Example          | Base class            | Role                                                        |
|--------------|------------------|-----------------------|------------------------------------------------------------|
| `dto`        | `LoadCreationInfos` | `ModificationInfos` | Serializable description: REST contract, persistence, Swagger. |
| `modifications` | `LoadCreation`   | `AbstractModification` | Executable logic: `check(network)` then `apply(network, …)`. |

`ModificationInfos` is the polymorphic root of the DTO layer (`@JsonTypeInfo` + `@JsonSubTypes`); it carries
the server-specific fields (`uuid`, `date`, `stashed`, `activated`, `messageType`, `messageValues`,
`description`) and the Swagger annotations.

The single bridge from description to execution is `ModificationInfos#toModification()`. Each leaf `*Infos`
builds its matching modification from its own fields:

```java
// LoadCreationInfos
public AbstractModification toModification() {
    return LoadCreation.builder()
        .equipmentId(getEquipmentId())
        .p0(p0)
        .q0(q0)
        // … copy every field …
        .build();
}
```

This is why the modification classes **redeclare their business fields** instead of inheriting them from the
`*Infos` DTO: it severs the dependency from `modifications` to the server-oriented `ModificationInfos`
hierarchy. The modification is a self-contained, builder-constructed object; the DTO merely knows how to
populate it.

### Class hierarchy

Both layers mirror the same equipment taxonomy, but in two independent trees.

#### Execution layer (`modifications`)

Every applied modification descends from `AbstractModification` (itself a PowSyBl
`AbstractNetworkModification`). The abstract bases factor out the shared `check`/`apply`/reporting logic:

```
AbstractNetworkModification (powsybl)
└── AbstractModification
    ├── AbstractEquipmentBase
    │   ├── AbstractEquipmentCreation
    │   │   ├── AbstractInjectionCreation ........ implements InjectionCreation
    │   │   │   ├── LoadCreation
    │   │   │   ├── GeneratorCreation ............ implements ReactiveLimitsHolderInfos
    │   │   │   ├── BatteryCreation .............. implements ReactiveLimitsHolderInfos
    │   │   │   ├── ShuntCompensatorCreation
    │   │   │   └── StaticVarCompensatorCreation
    │   │   ├── AbstractBranchCreation
    │   │   │   ├── LineCreation
    │   │   │   └── TwoWindingsTransformerCreation
    │   │   ├── SubstationCreation
    │   │   ├── VoltageLevelCreation
    │   │   ├── LccCreation
    │   │   └── VscCreation
    │   ├── AbstractEquipmentModification
    │   │   ├── AbstractInjectionModification .... implements InjectionModification
    │   │   │   ├── LoadModification
    │   │   │   ├── GeneratorModification
    │   │   │   ├── BatteryModification
    │   │   │   └── ShuntCompensatorModification
    │   │   ├── AbstractBranchModification
    │   │   │   ├── LineModification
    │   │   │   └── TwoWindingsTransformerModification
    │   │   ├── SubstationModification
    │   │   ├── VoltageLevelModification
    │   │   ├── LccModification
    │   │   └── VscModification
    │   ├── EquipmentAttributeModification
    │   ├── EquipmentDeletion
    │   ├── OperatingStatusModification
    │   └── VoltageLevelTopologyModification
    ├── AbstractScaling
    │   ├── GeneratorScaling
    │   └── LoadScaling
    └── (standalone) CompositeModification, ModificationReference, GroovyScript,
        GenerationDispatch, ByFilterDeletion, VoltageInitModification, … 
```

#### Shared contracts: `InjectionCreation` / `InjectionModification`

The `InjectionCreation` and `InjectionModification` **interfaces** (in `modifications`) declare the common
injection field accessors (`equipmentId`, `voltageLevelId`, properties, connection info, …). They are
implemented by **two independent abstract bases**, which is the reason the contract is an interface rather
than a shared superclass:

- `modifications.AbstractInjectionCreation` — the base for *top-level* injection modifications (above).
- `modifications.data.AbstractInjectionCreation` — a base for *injection data components* that are **not**
  modifications themselves. `LccConverterStationCreation` and `VscConverterStationCreation` extend it; they
  are sub-objects embedded inside `LccCreation` / `VscCreation` rather than standalone modifications, which
  is why they live in `modifications.data` and only implement the interface instead of joining the
  `AbstractModification` chain.

`ReactiveLimitsHolderInfos` is another shared interface (currently in `dto`) implemented by the
reactive-limits-bearing creations (`GeneratorCreation`, `BatteryCreation`, `VscConverterStationCreation`).

#### Description layer (`dto`)

The DTO tree parallels the execution tree, rooted at the polymorphic `ModificationInfos`:

```
ModificationInfos
├── EquipmentModificationInfos
│   ├── EquipmentCreationInfos
│   │   ├── InjectionCreationInfos
│   │   │   ├── LoadCreationInfos
│   │   │   ├── GeneratorCreationInfos ........... implements ReactiveLimitsHolderInfos
│   │   │   ├── BatteryCreationInfos ............. implements ReactiveLimitsHolderInfos
│   │   │   ├── ShuntCompensatorCreationInfos
│   │   │   ├── StaticVarCompensatorCreationInfos
│   │   │   ├── LccConverterStationCreationInfos
│   │   │   └── ConverterStationCreationInfos .... implements ReactiveLimitsHolderInfos
│   │   ├── BranchCreationInfos
│   │   │   ├── LineCreationInfos
│   │   │   └── TwoWindingsTransformerCreationInfos
│   │   ├── SubstationCreationInfos
│   │   ├── VoltageLevelCreationInfos
│   │   └── LccCreationInfos
│   ├── BasicEquipmentModificationInfos
│   │   ├── InjectionModificationInfos
│   │   │   ├── LoadModificationInfos
│   │   │   ├── GeneratorModificationInfos
│   │   │   └── … (Battery, ShuntCompensator, ConverterStation, Lcc, …)
│   │   ├── BranchModificationInfos
│   │   │   ├── LineModificationInfos
│   │   │   └── TwoWindingsTransformerModificationInfos
│   │   └── SubstationModificationInfos
│   ├── EquipmentAttributeModificationInfos
│   ├── EquipmentDeletionInfos
│   └── OperatingStatusModificationInfos
├── ScalingInfos
│   ├── GeneratorScalingInfos
│   └── LoadScalingInfos
└── (direct) CompositeModificationInfos, ModificationReferenceInfos, GroovyScriptInfos,
    TabularModificationInfos, ByFilterDeletionInfos, VoltageInitModificationInfos, … 
```

Note the two trees are **not** linked by inheritance: a `LoadCreation` is *not* a subclass of
`LoadCreationInfos`. They are connected only through `LoadCreationInfos.toModification()`, which copies the
DTO's fields into a freshly built `LoadCreation`.

### Dependency rules between packages

The dependency direction is deliberate and one-way:

```
        dto  ───────────────►  modifications  ───►  utils  ───►  (root contracts)
   (description)               (execution)
```

- **`dto` depends on `modifications`** — every `*Infos.toModification()` constructs a concrete modification.
  This is expected and correct.
- **`modifications` must NOT depend on `ModificationInfos` subclasses.** The execution layer should never
  reference a server-oriented DTO. Today, after redeclaring the business fields in the modifications, this
  rule holds for the whole creation/modification family.
- The only `dto` types the `modifications` package is still allowed to reference are **plain data objects**
  that happen to live in `dto` for now — see below.

#### Shared data objects (the `dto` types modifications may still use)

A handful of `dto` classes are *not* modifications: they are simple, reusable value/data objects that do
**not** extend `ModificationInfos`. The execution layer references them legitimately, for example:

- `FreePropertyInfos`, `AttributeModification<T>`, `ReactiveCapabilityCurvePointsInfos`,
  `IdentifiableAttributes`, `ScalingVariationInfos`, `FilterInfos`, `OperationalLimitsGroupInfos`, …

These currently sit in the `dto` package only for historical reasons. Conceptually they belong to the
execution model and are **intended to move into the `modifications` package** (the `modifications.data`
subpackage already hosts some of them, e.g. the injection creation/modification data holders and the LCC/VSC
converter-station data). Until that move is complete, treat any `dto` type that does **not** extend
`ModificationInfos` as part of the execution model, not as a DTO.

The same applies to **value types nested inside an `*Infos` class**: a modification that imports an `*Infos`
purely to reach a nested enum or record is *not* depending on a `ModificationInfos` subclass — it is using a
plain data object that simply lives in the wrong place. For example `OperatingStatusModification` references
only `OperatingStatusModificationInfos.ActionType`, a plain enum. Such nested types should be promoted to
top-level data objects and moved alongside the execution layer (`modifications.data`).

### Special cases: composite, tabular & reference modifications

A few families break the "modifications never depend on `ModificationInfos`" rule by nature, and are
explicitly **excluded** from it:

- **`CompositeModification`** aggregates an ordered list of child modifications.
- **Tabular modifications** (`modifications.tabular`) apply the same modification to many equipments in bulk.
- **`ModificationReference`** wraps a single other modification, holding it as a `ModificationInfos` and
  delegating to its `toModification()` at apply time.

All of these are *containers of modifications*, so they intrinsically manipulate the polymorphic
`ModificationInfos` type (and the tabular `*Infos`). They should be understood as a layer *above* the
ordinary modifications rather than peers of them, and they are not expected to satisfy the
"no `ModificationInfos` dependency" constraint that applies to every other modification.

### Adding a new modification

1. Create the executable `XxxModification` (or `XxxCreation`) in `modifications`, extending
   `AbstractModification`. Declare its own business fields, implement `check(Network)` and
   `apply(Network, ReportNode)`, expose a Lombok `@Builder`.
2. Create the `XxxInfos` DTO in `dto`, extending the appropriate `ModificationInfos` subtype, carrying the
   Swagger annotations and any persistence concerns.
3. Implement `XxxInfos#toModification()` to build the modification from the DTO's fields.
4. Register the DTO in the `@JsonSubTypes` list of `ModificationInfos` and add its `@JsonTypeName` /
   `@ModificationErrorTypeName` annotations.
5. Reuse the shared data objects (`FreePropertyInfos`, `AttributeModification`, …) for common fields; do not
   let the modification reference any `ModificationInfos` subclass.

