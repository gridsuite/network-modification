# Architecture Documentation — `gridsuite-network-modification`

## Overview

`gridsuite-network-modification` is a Java library designed to apply modifications to electrical power networks. It is part of the [GridSuite](http://www.gridsuite.org/) platform and is built on top of [PowSyBl](https://www.powsybl.org/), an open-source framework for power-system simulation.

The library provides a clean, extensible separation between:

- **Data Transfer Objects (DTOs)** — plain Java objects describing *what* a modification should do (the inputs/configuration).
- **Modification implementations** — the business logic that actually mutates a `Network` object.

It is intended to be consumed by other services or applications that need to programmatically modify network topologies, equipment properties, or operating status.

---

## Technology Stack

| Concern | Technology |
|---|---|
| Language | Java 17+ |
| Build tool | Maven |
| Core framework | PowSyBl (IIDM, Load-Flow API, Balances Adjustment) |
| DTO / serialisation | Jackson, Swagger/OpenAPI annotations |
| Boilerplate reduction | Lombok |
| Scripting support | Apache Groovy |
| Validation | Jakarta Validation API |
| Reporting | PowSyBl `ReportNode` |
| Unit testing | JUnit 5, Spring Boot Test |

---

## High-Level Module Layout

```
org.gridsuite.modification
├── dto/                         # Data Transfer Objects (modification descriptors)
│   ├── annotation/              # Custom annotations (e.g. @ModificationErrorTypeName)
│   ├── byfilter/                # DTOs for filter-based modifications
│   │   ├── assignment/          # Assignment descriptors
│   │   ├── equipmentfield/      # Field resolution per equipment type
│   │   └── formula/             # Formula-based modification descriptors
│   └── tabular/                 # DTOs for tabular / bulk modifications
├── modifications/               # Concrete modification implementations
│   ├── byfilter/                # Filter-based modification logic
│   └── tabular/                 # Tabular / bulk modification logic
├── report/                      # i18n report message bundles
├── utils/                       # Internal utility helpers
├── IFilterService.java          # Service interface: filter resolution
├── ILoadFlowService.java        # Service interface: load-flow parameters
├── ModificationType.java        # Enum of all supported modification types
└── NetworkModificationException.java  # Unified exception type
```

---

## Core Abstractions

### 1. `AbstractModification` (modifications layer)

```
com.powsybl.iidm.modification.AbstractNetworkModification
    └── AbstractModification  (org.gridsuite.modification.modifications)
            └── <all concrete modifications>
```

Every modification inherits from `AbstractModification`, which extends PowSyBl's `AbstractNetworkModification`. It defines three lifecycle hooks:

| Method | Purpose |
|---|---|
| `check(Network)` | Validates inputs before applying. Throws `NetworkModificationException` on error. |
| `initApplicationContext(IFilterService, ILoadFlowService)` | Injects external services required by some modifications (e.g. filter resolution, load-flow parameters). |
| `apply(Network, ReportNode)` | Applies the modification to the network and records events in the report tree. |
| `apply(Network, NamingStrategy, ReportNode)` | Variant that accepts an explicit naming strategy. |
| `getName()` | Returns a stable string identifier for the modification type. |

### 2. `ModificationInfos` (DTO layer)

`ModificationInfos` is the root DTO base class. It is serialised/deserialised with Jackson using polymorphic typing (`@JsonTypeInfo` / `@JsonSubTypes`). All concrete DTO classes annotate themselves with `@JsonTypeName("<MODIFICATION_TYPE>")`.

Key fields:

| Field | Type | Description |
|---|---|---|
| `uuid` | `UUID` | Unique identifier of the modification |
| `type` | `ModificationType` | Enum value identifying the modification kind |
| `date` | `Instant` | Creation timestamp |
| `stashed` | `Boolean` | When `true`, the modification is deactivated |
| `activated` | `Boolean` | Whether the modification is active |
| `description` | `String` | Free-text user description |
| `messageType` | `String` | i18n message key |
| `messageValues` | `String` | Serialised message interpolation values |

Key methods:

| Method | Description |
|---|---|
| `toModification()` | Factory — creates the corresponding `AbstractModification` implementation |
| `createSubReportNode(ReportNode)` | Creates a child reporting node for this modification |
| `check()` | Validates the DTO itself before converting to a modification |
| `getErrorType()` | Returns the `NetworkModificationException.Type` associated with this DTO |

### 3. DTO Inheritance Hierarchy

```
ModificationInfos
├── EquipmentModificationInfos         (equipmentId + free properties)
│   ├── EquipmentCreationInfos         (+ equipmentName)
│   │   ├── InjectionCreationInfos     (voltageLevel, bus/busbar section)
│   │   │   ├── LoadCreationInfos
│   │   │   ├── GeneratorCreationInfos
│   │   │   ├── BatteryCreationInfos
│   │   │   ├── ShuntCompensatorCreationInfos
│   │   │   └── StaticVarCompensatorCreationInfos
│   │   ├── BranchCreationInfos        (voltage levels, terminals, limits)
│   │   │   ├── LineCreationInfos
│   │   │   └── TwoWindingsTransformerCreationInfos
│   │   ├── SubstationCreationInfos
│   │   ├── VoltageLevelCreationInfos
│   │   ├── VscCreationInfos / LccCreationInfos
│   │   └── ...
│   ├── InjectionModificationInfos     (attribute-level changes via AttributeModification<T>)
│   │   ├── LoadModificationInfos
│   │   ├── GeneratorModificationInfos
│   │   ├── BatteryModificationInfos
│   │   └── ...
│   ├── BranchModificationInfos
│   │   ├── LineModificationInfos
│   │   └── TwoWindingsTransformerModificationInfos
│   └── BasicEquipmentModificationInfos
├── CompositeModificationInfos          (list of nested ModificationInfos)
├── ModificationReferenceInfos          (reference to another modification by UUID)
├── GeneratorScalingInfos / LoadScalingInfos
├── GenerationDispatchInfos
├── BalancesAdjustmentModificationInfos
├── VoltageInitModificationInfos
├── GroovyScriptInfos
├── OperatingStatusModificationInfos
├── EquipmentAttributeModificationInfos
├── TabularModificationInfos / TabularCreationInfos
├── ByFormulaModificationInfos / ModificationByAssignmentInfos
├── ByFilterDeletionInfos
└── Topology modifications (LineSplit, LineAttach, CreateVoltageLevel, ...)
```

---

## Modification Categories

### Equipment CRUD

Direct creation or modification of individual network equipment:

| Category | Modification Types |
|---|---|
| **Loads** | `LOAD_CREATION`, `LOAD_MODIFICATION` |
| **Generators** | `GENERATOR_CREATION`, `GENERATOR_MODIFICATION` |
| **Batteries** | `BATTERY_CREATION`, `BATTERY_MODIFICATION` |
| **Shunt compensators** | `SHUNT_COMPENSATOR_CREATION`, `SHUNT_COMPENSATOR_MODIFICATION` |
| **Static var compensators** | `STATIC_VAR_COMPENSATOR_CREATION` |
| **Lines** | `LINE_CREATION`, `LINE_MODIFICATION` |
| **Two-windings transformers** | `TWO_WINDINGS_TRANSFORMER_CREATION`, `TWO_WINDINGS_TRANSFORMER_MODIFICATION` |
| **Substations** | `SUBSTATION_CREATION`, `SUBSTATION_MODIFICATION` |
| **Voltage levels** | `VOLTAGE_LEVEL_CREATION`, `VOLTAGE_LEVEL_MODIFICATION` |
| **VSC HVDC** | `VSC_CREATION`, `VSC_MODIFICATION`, `CONVERTER_STATION_CREATION`, `CONVERTER_STATION_MODIFICATION` |
| **LCC HVDC** | `LCC_CREATION`, `LCC_MODIFICATION`, `LCC_CONVERTER_STATION_CREATION`, `LCC_CONVERTER_STATION_MODIFICATION` |

### Equipment Deletion

| Type | Description |
|---|---|
| `EQUIPMENT_DELETION` | Delete a single equipment by ID and type |
| `BY_FILTER_DELETION` | Delete all equipment matching a set of filters |

### Topology Modifications

Complex structural operations on the network graph:

| Type | Description |
|---|---|
| `LINE_SPLIT_WITH_VOLTAGE_LEVEL` | Split a line by inserting a new voltage level |
| `LINE_ATTACH_TO_VOLTAGE_LEVEL` | Attach an end of a line to a different voltage level |
| `LINES_ATTACH_TO_SPLIT_LINES` | Restructure lines around a split |
| `DELETE_VOLTAGE_LEVEL_ON_LINE` | Remove a voltage level inserted on a line |
| `DELETE_ATTACHING_LINE` | Remove a line and rewire the network |
| `VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION` | Modify busbar sections within a voltage level |
| `CREATE_COUPLING_DEVICE` | Add a coupling device (breaker/disconnector) |
| `CREATE_VOLTAGE_LEVEL_TOPOLOGY` | Create an entire voltage level topology |
| `CREATE_VOLTAGE_LEVEL_SECTION` | Add a busbar section to an existing voltage level |
| `MOVE_VOLTAGE_LEVEL_FEEDER_BAYS` | Move feeder bays between busbar sections |

### Bulk / Programmatic Modifications

| Type | Description |
|---|---|
| `TABULAR_MODIFICATION` | Apply the same attribute changes to many equipment of the same type |
| `TABULAR_CREATION` | Create many equipment of the same type from a table |
| `LIMIT_SETS_TABULAR_MODIFICATION` | Bulk modification of operational limit sets |
| `BY_FORMULA_MODIFICATION` | Apply a formula-based transformation to equipment attributes |
| `MODIFICATION_BY_ASSIGNMENT` | Assign values to equipment attributes based on rules |
| `EQUIPMENT_ATTRIBUTE_MODIFICATION` | Change a single named attribute on one piece of equipment |
| `GROOVY_SCRIPT` | Execute an arbitrary Groovy script against the network |

### Operational Modifications

| Type | Description |
|---|---|
| `OPERATING_STATUS_MODIFICATION` | Trip/energize/lockout a line or equipment |
| `GENERATOR_SCALING` | Scale generator active power production |
| `LOAD_SCALING` | Scale load consumption |
| `GENERATION_DISPATCH` | Dispatch generation to meet a target |
| `BALANCES_ADJUSTMENT_MODIFICATION` | Run a balances-adjustment algorithm on areas |
| `VOLTAGE_INIT_MODIFICATION` | Apply voltage initialisation results |

### Composition / Orchestration

| Type | Description |
|---|---|
| `COMPOSITE_MODIFICATION` | Run a named, ordered list of sub-modifications sequentially; errors in sub-modifications are logged but do not interrupt the sequence |
| `MODIFICATION_REFERENCE` | Delegate to another modification identified by UUID |

---

## Key Design Patterns

### DTO ↔ Implementation Factory

Each concrete `ModificationInfos` subclass overrides `toModification()` to instantiate the matching `AbstractModification` subclass. This keeps the DTO and implementation tightly coupled while retaining clean separation of concerns:

```java
// DTO:
public AbstractModification toModification() {
    return new GeneratorCreation(this);
}

// Caller:
AbstractModification mod = infos.toModification();
mod.check(network);
mod.apply(network, reportNode);
```

### Attribute Patching with `AttributeModification<T>`

For partial updates, fields in modification DTOs are wrapped in `AttributeModification<T>` rather than using raw values. This allows consumers to distinguish between:

- **Not provided** (field is `null`) — attribute is left unchanged.
- **SET** (`OperationType.SET`) — attribute is changed to the given value.
- **UNSET** (`OperationType.UNSET`) — attribute is reset to its default/null.

### Polymorphic JSON Serialisation

`ModificationInfos` uses Jackson's `@JsonTypeInfo` / `@JsonSubTypes` so that a list of modifications can be deserialised from JSON without extra type handling by the consumer. The `type` field acts as the discriminator.

### Reporting via `ReportNode`

All modifications report their actions and errors through PowSyBl's hierarchical `ReportNode` API. Each modification creates a typed child node before performing its work, enabling structured audit logs and i18n message rendering.

### External Service Injection

Some modifications depend on external services:

- `IFilterService` — resolves UUIDs to `AbstractFilter` objects and exports matching equipment sets from the network.
- `ILoadFlowService` — retrieves `LoadFlowParametersInfos` by UUID for modifications that run an internal load flow.

These are injected via `initApplicationContext()` and are intentionally defined as interfaces so that host applications can provide their own implementations.

---

## Error Handling

`NetworkModificationException` (extending PowSyBl's `PowsyblException`) is the single exception type thrown by this library. It carries a strongly typed `Type` enum value (e.g. `GENERATOR_NOT_FOUND`, `CREATE_LOAD_ERROR`) paired with an HTTP status code, making it easy for REST controllers in consumer services to map errors to proper HTTP responses.

The `@ModificationErrorTypeName` annotation on each DTO class statically links the DTO to its error type — accessible at runtime via `ModificationInfos.getErrorType()`.

---

## Data Flow

```
Consumer application
        │
        │  1. Build / deserialise ModificationInfos DTO
        ▼
ModificationInfos.check()          ← validate DTO inputs
        │
        │  2. Convert to executable modification
        ▼
ModificationInfos.toModification() → AbstractModification
        │
        │  3. Inject services (optional)
        ▼
AbstractModification.initApplicationContext(filterService, loadFlowService)
        │
        │  4. Validate against the live network
        ▼
AbstractModification.check(network)
        │
        │  5. Apply and record results
        ▼
AbstractModification.apply(network, reportNode)
        │
        ▼
   Mutated Network + populated ReportNode tree
```

---

## Package Summary

| Package | Responsibility |
|---|---|
| `org.gridsuite.modification` | Root: enums, interfaces, exception |
| `org.gridsuite.modification.dto` | DTO classes for all modification types |
| `org.gridsuite.modification.dto.annotation` | Custom annotations used by the DTO layer |
| `org.gridsuite.modification.dto.byfilter` | DTOs for filter-based modifications, formulas, and assignments |
| `org.gridsuite.modification.dto.tabular` | DTOs for tabular/bulk modifications |
| `org.gridsuite.modification.modifications` | Concrete modification implementations |
| `org.gridsuite.modification.modifications.byfilter` | Implementations of filter-based modifications |
| `org.gridsuite.modification.modifications.tabular` | Implementations of tabular modifications |
| `org.gridsuite.modification.report` | `ResourceBundle` registration for i18n report messages |
| `org.gridsuite.modification.utils` | Shared utility classes (limits, measurements, properties, load-flow params) |
