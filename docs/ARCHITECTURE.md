# Architecture Documentation — `gridsuite-network-modification`

## Overview

`gridsuite-network-modification` is a Java library designed to apply structural, operational, and topological modifications to electrical power networks. It is part of the [GridSuite](http://www.gridsuite.org/) platform and is built on top of [PowSyBl](https://www.powsybl.org/) (Power System Blocks), an open-source framework for power-system simulation and analysis.

The library provides a clean, extensible architectural separation between:

- **Data Transfer Objects (DTOs)** — plain Java models describing *what* network modifications to perform (inputs, configurations, metadata).
- **Modification Implementations** — business logic execution classes that validate and apply mutations to a live PowSyBl `Network` instance.
- **Reporting and Auditing** — integrated internationalized reporting via PowSyBl `ReportNode`.
- **Error Handling** — a unified exception hierarchy mapping power-system modification errors.

It is intended to be consumed as a library by backend services (such as GridSuite microservices) or standalone Java applications that require programmatic network manipulation.

---

## Technology Stack

| Concern | Technology | Notes |
|---|---|---|
| Language | Java 25 | Configured in `pom.xml` |
| Build tool | Apache Maven | Parent POM: `powsybl-parent` |
| Core Framework | PowSyBl (`powsybl-iidm-api`, `powsybl-iidm-modification`, `powsybl-loadflow-api`, `powsybl-open-loadflow`, `powsybl-balances-adjustment`) | Network modeling and simulation |
| Filter Framework | `gridsuite-filter` | Equipment filtering engine |
| Serialization & Schema | Jackson (`jackson-databind`, `jackson-datatype-jsr310`), Swagger/OpenAPI v3 annotations | Polymorphic JSON serialization |
| Boilerplate Reduction | Project Lombok | Builder, Getter, Setter, EqualsAndHashCode |
| Scripting Engine | Apache Groovy | Dynamic script-based network modifications |
| Validation | Jakarta Validation API (`jakarta.validation-api`) | Constraint validation |
| Reporting & i18n | PowSyBl `ReportNode`, Java `ResourceBundle` (`AutoService`) | Multi-language execution reports |
| Testing | JUnit 5, Spring Boot Test, PowSyBl Config Test, JaCoCo | Unit and integration testing |

---

## High-Level Module Layout

```
org.gridsuite.modification
├── context/                     # ModificationContext and the loaders it resolves dependencies with
│   └── dto/                     # Context value objects
├── dto/                         # Data Transfer Objects (deserialization, models)
│   ├── byfilter/                # DTOs for filter-based modifications
│   │   ├── assignment/          # Assignment descriptors (String, Double, Boolean, Enum)
│   │   ├── equipmentfield/      # Field resolution enums per equipment type
│   │   └── formula/             # Formula-based modification descriptors and operators
│   └── tabular/                 # DTOs for tabular / bulk modifications
├── error/                       # Unified exception hierarchy and exception types
├── modifications/               # Concrete modification business logic implementations
│   ├── byfilter/                # Filter-based modification execution logic
│   ├── data/                    # Execution data handed to modifications
│   │   ├── assignment/          # Assignment data (one class per value type)
│   │   └── scaling/             # Scaling variation data
│   ├── olg/                     # Operational-limits-group bulk modification logic
│   └── tabular/                 # Tabular and limit-set bulk modification execution logic
├── report/                      # i18n report resource bundle registration (SPI)
├── utils/                       # Utility classes (limits, measurements, properties, load-flow)
├── ModificationType.java        # Enumeration of all supported modification types
├── ReactiveVariationMode.java   # Scaling reactive variation mode enum
├── TapChangerType.java          # Tap changer types enum
├── VariationMode.java           # Scaling active variation mode enum
└── VariationType.java           # Scaling variation type (delta / target) enum
```

---

## Core Abstractions

### 1. `AbstractModification` (Modifications Layer)

```
com.powsybl.iidm.modification.AbstractNetworkModification
    └── AbstractModification  (org.gridsuite.modification.modifications)
            └── <concrete modification implementations>
```

Every modification implementation inherits from `AbstractModification`, which extends PowSyBl's `AbstractNetworkModification`. It defines the core execution lifecycle:

| Method | Purpose |
|---|---|
| `void check(Network network)` | Pre-execution validation against a live `Network` instance. Throws `NetworkModificationException` on validation failure. Default implementation performs no check. |
| `final void initApplicationContext(String rootNetworkTag)` | Sets only the root network tag. External resources are resolved through the construction context, not this method. The tag must be restored after executable deserialization. |
| `void apply(Network network, ReportNode subReportNode)` | Executes the modification logic, mutating the `Network` and writing structured events to the provided `ReportNode`. Must be implemented by subclasses. |
| `void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode)` | Variant accepting an explicit `NamingStrategy`. Delegates by default to `apply(network, subReportNode)`. |
| `String getName()` | Returns a stable, human-readable name identifying the modification type. |

### 2. `ModificationInfos` (DTO Layer)

`ModificationInfos` is the abstract base class for all modification Data Transfer Objects. It supports polymorphic JSON serialization and deserialization using Jackson annotations (`@JsonTypeInfo` and `@JsonSubTypes`) with the `type` property as discriminator.

Key properties:

| Property | Type | Description |
|---|---|---|
| `uuid` | `UUID` | Unique identifier of the modification instance |
| `type` | `ModificationType` | Automatically derived from the class `@JsonTypeName` or explicitly set |
| `date` | `Instant` | Timestamp of creation or modification |
| `stashed` | `Boolean` | Staging flag; stashed modifications (`true`) are skipped during execution |
| `activated` | `Boolean` | Activation flag; deactivated modifications are skipped during execution |
| `description` | `String` | Optional user description or scenario notes |
| `messageType` | `String` | i18n message key template |
| `messageValues` | `String` | Serialized message interpolation parameters |

Key methods:

| Method | Description |
|---|---|
| `AbstractModification toModification()` | Converts a DTO without supplied loaders; suitable for modifications without external dependencies. |
| `AbstractModification toModification(ModificationContext context)` | Converts a DTO using the required loaders. Composite/reference children are converted during application. |
| `ReportNode createSubReportNode(ReportNode reportNode)` | Creates and attaches a child `ReportNode` with the appropriate message template for this modification. |
| `void check()` | Validates internal DTO fields before converting to an executable modification. |
| `ModificationType getType()` | Returns the resolved `ModificationType` enum value. |
| `Map<String, String> getMapMessageValues()` | Returns interpolation parameters as key-value pairs for reporting. |

### 3. DTO Inheritance Hierarchy

```
ModificationInfos
├── EquipmentModificationInfos                     # Base for modifications targeting single equipment (equipmentId, properties)
│   ├── BasicEquipmentModificationInfos            # Lightweight property/attribute modification
│   ├── EquipmentCreationInfos                     # Base for creating new equipment (+ equipmentName)
│   │   ├── InjectionCreationInfos                 # Injections (voltageLevelId, busOrBusbarSectionId, feeder bay)
│   │   │   ├── LoadCreationInfos                  # Load creation
│   │   │   ├── GeneratorCreationInfos             # Generator creation (active/reactive limits, regulation)
│   │   │   ├── BatteryCreationInfos               # Battery storage creation
│   │   │   ├── ShuntCompensatorCreationInfos      # Capacitor / reactor creation
│   │   │   └── StaticVarCompensatorCreationInfos  # SVC creation
│   │   ├── BranchCreationInfos                    # Branches (terminals 1 & 2, limits, connection states)
│   │   │   ├── LineCreationInfos                  # AC transmission line creation
│   │   │   └── TwoWindingsTransformerCreationInfos# Two-windings power transformer creation
│   │   ├── SubstationCreationInfos                # Substation creation with nested voltage levels
│   │   ├── VoltageLevelCreationInfos              # Voltage level creation (topology kind, busbars, switches)
│   │   ├── VscCreationInfos                       # VSC HVDC line creation
│   │   ├── LccCreationInfos                       # LCC HVDC line creation
│   │   ├── ConverterStationCreationInfos          # VSC converter station creation
│   │   └── LccConverterStationCreationInfos       # LCC converter station creation
│   ├── InjectionModificationInfos                 # Attribute modifications for injections (partial updates)
│   │   ├── LoadModificationInfos                  # Load parameter updates
│   │   ├── GeneratorModificationInfos             # Generator parameter updates
│   │   ├── BatteryModificationInfos               # Battery parameter updates
│   │   └── ShuntCompensatorModificationInfos      # Shunt compensator updates
│   ├── BranchModificationInfos                    # Attribute modifications for branches
│   │   ├── LineModificationInfos                  # Line parameter updates
│   │   └── TwoWindingsTransformerModificationInfos# Transformer parameter updates
│   ├── SubstationModificationInfos                # Substation attribute updates
│   ├── VoltageLevelModificationInfos              # Voltage level attribute updates
│   ├── VscModificationInfos / LccModificationInfos# HVDC parameter updates
│   ├── ConverterStationModificationInfos          # VSC converter station updates
│   ├── LccConverterStationModificationInfos       # LCC converter station updates
│   ├── EquipmentDeletionInfos                     # Equipment deletion by ID and type
│   ├── EquipmentAttributeModificationInfos        # Single attribute modification by name
│   ├── OperatingStatusModificationInfos           # Operational status changes (lockout, trip, switch on)
│   └── VoltageLevelTopologyModificationInfos      # Busbar/switch configuration within a voltage level
├── CompositeModificationInfos                      # Ordered sequence of sub-modifications
├── ModificationReferenceInfos                      # Delegation to a modification by UUID
├── GeneratorScalingInfos / LoadScalingInfos        # Power scaling across equipment groups
├── GenerationDispatchInfos                         # Generation dispatch with loss & outage optimization
├── BalancesAdjustmentModificationInfos             # Area net position balances adjustment
├── VoltageInitModificationInfos                    # Voltage initialization across buses and generators
├── GroovyScriptInfos                               # Direct Groovy script execution on Network
├── TabularModificationInfos / TabularCreationInfos # Bulk tabular modifications and creations
├── LimitSetsTabularModificationInfos               # Bulk operational limit sets modifications
├── ByFormulaModificationInfos                      # Dynamic formula-based attribute calculations
├── ModificationByAssignmentInfos                   # Value assignment based on filter conditions
├── ByFilterDeletionInfos                           # Bulk equipment deletion matching filter criteria
├── NetworkReductionInfos                           # Network reduction to an area of interest defined by voltage level filters
└── Topology Modifications                          # Line splits, line attachments, feeder bay moves, coupling devices
```

---

## Modification Categories

### 1. Equipment Creation & Modification (CRUD)

Direct lifecycle operations on individual power system network elements:

- **Injections**: Loads, Generators, Battery storage, Shunt compensators (capacitors/reactors), Static Var Compensators (SVC).
- **Branches**: AC Lines, Two-Windings Transformers (with ratio and phase tap changers).
- **Substations & Topology**: Substations, Voltage levels (Bus-Breaker and Node-Breaker topologies).
- **HVDC Systems**: VSC (Voltage Source Converter) and LCC (Line-Commutated Converter) lines and converter stations.

### 2. Equipment Deletion

- `EQUIPMENT_DELETION`: Deletes an individual equipment item specified by ID and type.
- `BY_FILTER_DELETION`: Loads self-contained filter definitions through `FilterLoader` during DTO conversion, then evaluates them against the network to delete matching equipment.

### 3. Topology Modifications

Complex topological and structural rewiring operations:

- `LINE_SPLIT_WITH_VOLTAGE_LEVEL`: Splits a transmission line by inserting a new or existing voltage level.
- `LINE_ATTACH_TO_VOLTAGE_LEVEL`: Attaches an end of a line to an existing or new voltage level.
- `LINES_ATTACH_TO_SPLIT_LINES`: Reconnects existing lines to split line segments.
- `DELETE_VOLTAGE_LEVEL_ON_LINE`: Removes an intermediate voltage level and merges the line segments.
- `DELETE_ATTACHING_LINE`: Deletes an attaching line and restores original topology.
- `CREATE_COUPLING_DEVICE`: Creates a busbar coupling breaker/switch between busbar sections.
- `CREATE_VOLTAGE_LEVEL_TOPOLOGY`: Builds complete bus-breaker or node-breaker topology arrangements.
- `CREATE_VOLTAGE_LEVEL_SECTION`: Adds a new busbar section to a voltage level.
- `MOVE_VOLTAGE_LEVEL_FEEDER_BAYS`: Reorganizes feeder bay connections across busbar sections.
- `NETWORK_REDUCTION`: Reduces the network to an area of interest. The voltage levels matched by the supplied filters are excluded, all the equipment outside the retained area is removed, and the lines, transformers and HVDC lines crossing the boundary are replaced by fictitious loads.

### 4. Bulk & Programmatic Modifications

- `TABULAR_MODIFICATION` & `TABULAR_CREATION`: Executes batch attribute edits or equipment creations from tabular datasets.
- `LIMIT_SETS_TABULAR_MODIFICATION`: Bulk configuration of temporary and permanent operational limit sets.
- `BY_FORMULA_MODIFICATION`: Computes attribute values dynamically using mathematical formulas and equipment references.
- `MODIFICATION_BY_ASSIGNMENT`: Assigns values conditionally (String, Double, Boolean, Enum) to equipment matching filters.
- `EQUIPMENT_ATTRIBUTE_MODIFICATION`: Dynamically modifies a named property/attribute on target equipment.
- `GROOVY_SCRIPT`: Executes an arbitrary Groovy script against the `network` context for custom algorithms.

### 5. Operational & Power-Flow Adjustments

- `OPERATING_STATUS_MODIFICATION`: Switches equipment status (`LOCKOUT`, `TRIP`, `SWITCH_ON`, `ENERGISE_END_ONE`, `ENERGISE_END_TWO`).
- `GENERATOR_SCALING` & `LOAD_SCALING`: Proportional, stacked, or regular power scaling with active/reactive management.
- `GENERATION_DISPATCH`: Solves power dispatch to meet target balance considering outage rates and frequency reserves.
- `BALANCES_ADJUSTMENT_MODIFICATION`: Balances area exchanges and net positions using PowSyBl Balances Adjustment.
- `VOLTAGE_INIT_MODIFICATION`: Initializes network voltage profile (bus voltages, generator targets, transformer taps).

### 6. Composition & Orchestration

- `COMPOSITE_MODIFICATION`: Executes an ordered list of sub-modifications, using the retained construction context to convert child DTOs and propagating the root network tag through `initApplicationContext(rootNetworkTag)`.
- `MODIFICATION_REFERENCE`: Converts and executes its referenced DTO using the retained construction context and propagates the root network tag through `initApplicationContext(rootNetworkTag)`.

---

## Key Design Patterns

### 1. DTO ↔ Implementation Factory Pattern

Concrete DTOs implement conversion to their matching `AbstractModification`, using `toModification(context)` when external dependencies are needed. This separates serializable requests (which may carry UUIDs) from leaf execution data (resolved filters and parameters):

```java
// Caller execution workflow; the consumer supplies the required loaders.
ModificationContext context = ModificationContext.builder()
        .filterLoader(filterLoader)
        .filterWithDistributionKeysLoader(filterWithDistributionKeysLoader)
        .loadFlowParametersLoader(loadFlowParametersLoader)
        .build();

ModificationInfos dto = ...;
dto.check(); // DTO validation
AbstractModification modification = dto.toModification(context);
modification.initApplicationContext(rootNetworkTag);
modification.check(network); // Domain validation
modification.apply(network, reportNode); // Network mutation
```

For DTOs with no external dependencies, use `toModification()` or `toModification(ModificationContext.empty())`. Leaf modifications resolve resources during conversion. Composite and referenced modifications retain the context and defer conversion of nested DTOs until `apply`, so they must not be described as entirely loader-free executable values.

### 2. Partial Updates via `AttributeModification<T>`

To support partial updates where unspecified properties remain untouched, modification DTOs wrap mutable fields in `AttributeModification<T>`:

- **Field is `null`**: Property is omitted; current network value is preserved.
- **`OperationType.SET`**: Property is explicitly updated to the new value.
- **`OperationType.UNSET`**: Property is reset to its default or null value.

### 3. Polymorphic Serialization

Jackson's `@JsonTypeInfo` and `@JsonSubTypes` enable transparent serialization and deserialization of heterogeneous collections of modification descriptors via standard REST APIs and JSON files without custom parsing logic.

The executable root network tag is excluded from JSON; restore it using `initApplicationContext(rootNetworkTag)` before application. Composite/reference construction contexts are also excluded from JSON. For nested DTOs requiring loaders, reconstruct the wrapper with a suitable context, for example by converting the original DTO again. Restoring the tag alone does not restore loaders.

### 4. Hierarchical Reporting

All modifications log execution events, warnings, and messages through PowSyBl's `ReportNode` hierarchy. A dedicated SPI resource bundle (`NetworkModificationReportResourceBundle`) registers message templates in multiple languages (English, French).

### 5. Construction Context and Loader Abstraction

`ModificationContext` holds three consumer-provided functional interfaces in `org.gridsuite.modification.context`:

- `FilterLoader.load(List<UUID>)`: Returns a map of self-contained `org.gridsuite.filter.wip.Filter` objects indexed by UUID; missing filters are omitted and values are non-null.
- `FilterWithDistributionKeysLoader.load(List<UUID>)`: Returns filters with equipment distribution keys as `FilterWithDistributionKeys` values. Non-identifier-list filters have an empty distribution-key map; missing filters are omitted.
- `LoadFlowParametersLoader.load(UUID)`: Returns `Optional<LoadFlowParametersInfos>`; missing parameters produce `Optional.empty()`.

Only required loaders need to be supplied. Unset components are replaced by loaders that throw an explicit `IllegalStateException` when used. Loading definitions does not evaluate equipment selection: filters are evaluated separately against the target network.

Transport belongs to the consumer: loaders may delegate to HTTP clients, repositories or in-memory data. `IFilterService` and `ILoadFlowService` have been removed, but concrete clients can remain behind these loaders. Migration consists of supplying the context to `toModification(context)` and using `initApplicationContext(rootNetworkTag)` only for the tag.

---

## Error Handling Architecture

All domain errors produce a `NetworkModificationException` (inheriting from PowSyBl's `PowsyblException`).

- Each exception carries a `NetworkModificationExceptionType` enum value providing a descriptive message and clear error classification.
- Static factory methods on `NetworkModificationException` provide standard error construction:
  - `createEquipmentTypeUnknown(type)`
  - `createEquipmentTypeNotSupported(type)`
  - `createOperatingActionTypeUnsupported(actionType)`
  - `createEquipementAttributeNotEditable(equipmentType, attributeName)`
  - `createHybridHvdcUnsupported(hvdcId)`

---

## Data Flow

```
Consumer Application / Service
              │
              │  1. Deserialise JSON / construct DTO
              ▼
    ModificationInfos.check()                 ← Validates DTO consistency
              │
              │  2. Convert to executable modification
              ▼
    ModificationInfos.toModification(context) → AbstractModification
              │                               ← Required loaders resolve leaf data
              │  3. Initialize root network tag
              ▼
    AbstractModification.initApplicationContext(rootNetworkTag)
              │
              │  4. Validate against target network
              ▼
    AbstractModification.check(network)       ← Throws NetworkModificationException on conflict
              │
              │  5. Mutate network and record reporting
              ▼
    AbstractModification.apply(network, reportNode)
              │
              ▼
    Mutated PowSyBl Network + Populated ReportNode Tree
```

For composites and references, nested DTO conversion occurs inside `apply`, using the retained context. The root network tag is propagated to each nested modification through `initApplicationContext(rootNetworkTag)`.

---

## Package Summary

| Package | Description |
|---|---|
| `org.gridsuite.modification` | Root package: enums (`ModificationType`, `VariationType`, `VariationMode`, `ReactiveVariationMode`, `TapChangerType`) |
| `org.gridsuite.modification.context` | `ModificationContext`, construction-time loader contracts and `FilterUtils`; nested composite/reference conversion is deferred until application |
| `org.gridsuite.modification.context.dto` | Context value objects (`FilterWithDistributionKeys`) |
| `org.gridsuite.modification.dto` | Core modification DTOs (CRUD, topology, scaling, dispatch, operational, references) |
| `org.gridsuite.modification.dto.byfilter` | Filter-based modification DTOs |
| `org.gridsuite.modification.dto.byfilter.assignment` | Assignment descriptors for typed modifications (String, Double, Boolean, Enum) |
| `org.gridsuite.modification.dto.byfilter.equipmentfield` | Equipment attribute target field enums |
| `org.gridsuite.modification.dto.byfilter.formula` | Mathematical formula descriptors and operator enums |
| `org.gridsuite.modification.dto.tabular` | Tabular batch modifications and limit set DTOs |
| `org.gridsuite.modification.error` | `NetworkModificationException` and `NetworkModificationExceptionType` |
| `org.gridsuite.modification.modifications` | Executable modification logic classes |
| `org.gridsuite.modification.modifications.byfilter` | Filter-based and formula-based execution classes |
| `org.gridsuite.modification.modifications.data` | Execution data passed to modifications (HVDC/injection bases) |
| `org.gridsuite.modification.modifications.data.assignment` | Assignment data, one class per value type |
| `org.gridsuite.modification.modifications.data.scaling` | Scaling variation data, resolved against the network before the modification runs |
| `org.gridsuite.modification.modifications.olg` | Operational-limits-group bulk modification classes |
| `org.gridsuite.modification.modifications.tabular` | Tabular batch execution classes |
| `org.gridsuite.modification.report` | Internationalized report bundle SPI (`NetworkModificationReportResourceBundle`) |
| `org.gridsuite.modification.utils` | Shared utility classes (limits, measurements, properties, load-flow configuration) |
