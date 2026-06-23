# API Reference — `gridsuite-network-modification`

**Maven coordinates**

```xml
<dependency>
    <groupId>org.gridsuite</groupId>
    <artifactId>gridsuite-network-modification</artifactId>
    <version>0.88.0-SNAPSHOT</version>
</dependency>
```

---

## Table of Contents

1. [Core Interfaces](#1-core-interfaces)
2. [AbstractModification](#2-abstractmodification)
3. [ModificationInfos — Base DTO](#3-modificationinfos--base-dto)
4. [ModificationType Enum](#4-modificationtype-enum)
5. [DTO Hierarchy & Fields](#5-dto-hierarchy--fields)
   - 5.1 [Equipment Modification DTOs](#51-equipment-modification-dtos)
   - 5.2 [Equipment Creation DTOs](#52-equipment-creation-dtos)
   - 5.3 [Injection Creation DTOs](#53-injection-creation-dtos)
   - 5.4 [Branch Creation DTOs](#54-branch-creation-dtos)
   - 5.5 [Injection Modification DTOs](#55-injection-modification-dtos)
   - 5.6 [Branch Modification DTOs](#56-branch-modification-dtos)
   - 5.7 [Substation & Voltage Level DTOs](#57-substation--voltage-level-dtos)
   - 5.8 [HVDC DTOs](#58-hvdc-dtos)
   - 5.9 [Topology Modification DTOs](#59-topology-modification-dtos)
   - 5.10 [Deletion DTOs](#510-deletion-dtos)
   - 5.11 [Scaling & Dispatch DTOs](#511-scaling--dispatch-dtos)
   - 5.12 [Bulk / Programmatic Modification DTOs](#512-bulk--programmatic-modification-dtos)
   - 5.13 [Operational Modification DTOs](#513-operational-modification-dtos)
   - 5.14 [Composition & Reference DTOs](#514-composition--reference-dtos)
6. [Supporting Value Objects](#6-supporting-value-objects)
7. [NetworkModificationException](#7-networkmodificationexception)
8. [Enumerations](#8-enumerations)
9. [Usage Examples](#9-usage-examples)

---

## 1. Core Interfaces

### `IFilterService`

**Package:** `org.gridsuite.modification`

Abstraction over a remote filter service. Consumer applications must provide an implementation.

```java
public interface IFilterService {

    /**
     * Fetches filter definitions by their UUIDs.
     *
     * @param filtersUuids list of filter UUIDs
     * @return list of AbstractFilter objects
     */
    List<AbstractFilter> getFilters(List<UUID> filtersUuids);

    /**
     * Resolves filters against a live network and streams matching equipment.
     *
     * @param filtersUuids list of filter UUIDs
     * @param network      the network to resolve against
     * @return stream of matched equipment per filter
     */
    Stream<org.gridsuite.filter.identifierlistfilter.FilterEquipments> exportFilters(
            List<UUID> filtersUuids, Network network);

    /**
     * Returns a map from filter UUID to the equipment matched by that filter.
     *
     * @param network the network to resolve against
     * @param filters map of filter UUID → filter name
     * @return map of filter UUID → FilterEquipments
     */
    Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(Network network, Map<UUID, String> filters);
}
```

---

### `ILoadFlowService`

**Package:** `org.gridsuite.modification`

Abstraction over a load-flow parameter store. Required by modifications that run an internal load flow.

```java
public interface ILoadFlowService {

    /**
     * Retrieves load-flow parameters by UUID.
     *
     * @param loadFlowParametersUuid the UUID of the stored parameters
     * @return LoadFlowParametersInfos populated with all load-flow settings
     */
    LoadFlowParametersInfos getLoadFlowParametersInfos(UUID loadFlowParametersUuid);
}
```

---

## 2. `AbstractModification`

**Package:** `org.gridsuite.modification.modifications`  
**Extends:** `com.powsybl.iidm.modification.AbstractNetworkModification`

The abstract base class for every modification in the library.

### Methods

| Signature | Description |
|---|---|
| `void check(Network network)` | Pre-apply validation. Throws `NetworkModificationException` on error. Default implementation does nothing. |
| `void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService)` | Injects external services. Called before `check` when services are needed. Default implementation does nothing. |
| `void apply(Network network, ReportNode subReportNode)` | Applies the modification and writes events to the report node. Must be implemented by all subclasses. |
| `void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode)` | Variant that accepts an explicit `NamingStrategy`. Default delegates to `apply(network, subReportNode)`. |
| `String getName()` | Returns a stable, human-readable name for this modification type. Must be implemented by all subclasses. |

### Typical usage pattern

```java
// 1. Build the DTO
GeneratorCreationInfos infos = GeneratorCreationInfos.builder()
        .equipmentId("GEN1")
        .equipmentName("Generator 1")
        .voltageLevelId("VL1")
        .busOrBusbarSectionId("BBS1")
        .minP(0.0)
        .maxP(500.0)
        .targetP(200.0)
        .voltageRegulationOn(true)
        .targetV(400.0)
        .build();

// 2. Validate the DTO
infos.check();

// 3. Create the modification
AbstractModification modification = infos.toModification();

// 4. (Optional) inject services
modification.initApplicationContext(filterService, loadFlowService);

// 5. Validate against network
modification.check(network);

// 6. Apply
ReportNode reportNode = ReportNode.newRootReportNode()
        .withMessageTemplate("root", "Root")
        .build();
modification.apply(network, reportNode);
```

---

## 3. `ModificationInfos` — Base DTO

**Package:** `org.gridsuite.modification.dto`  
**Serialisation discriminator field:** `type` (value matches `ModificationType` enum name)

### Fields

| Field | Type | Description |
|---|---|---|
| `uuid` | `UUID` | Unique identifier of this modification instance |
| `type` | `ModificationType` | Read-only; automatically derived from `@JsonTypeName` on the subclass |
| `date` | `Instant` | Creation or update timestamp |
| `stashed` | `Boolean` | `true` = modification is staged/deactivated (default `false`) |
| `activated` | `Boolean` | Whether this modification is active in composite execution |
| `description` | `String` | Optional free-text user description |
| `messageType` | `String` | i18n message key (set by the modification after apply) |
| `messageValues` | `String` | JSON-serialised interpolation values for the message |

### Methods

| Signature | Description |
|---|---|
| `AbstractModification toModification()` | Factory: creates the matching `AbstractModification`. Must be overridden. |
| `ReportNode createSubReportNode(ReportNode)` | Creates a child `ReportNode` for this modification's reporting. Must be overridden. |
| `void check()` | Validates DTO fields. Throws if mandatory fields are absent. |
| `ModificationType getType()` | Returns the type discriminator. |
| `NetworkModificationException.Type getErrorType()` | Returns the exception type linked to this DTO class via `@ModificationErrorTypeName`. |
| `Map<String, String> getMapMessageValues()` | Returns message interpolation values as a plain map. |

---

## 4. `ModificationType` Enum

**Package:** `org.gridsuite.modification`

All supported modification types:

| Value | Category |
|---|---|
| `LOAD_CREATION` | Equipment creation |
| `LOAD_MODIFICATION` | Equipment modification |
| `BATTERY_CREATION` | Equipment creation |
| `BATTERY_MODIFICATION` | Equipment modification |
| `GENERATOR_CREATION` | Equipment creation |
| `GENERATOR_MODIFICATION` | Equipment modification |
| `SHUNT_COMPENSATOR_CREATION` | Equipment creation |
| `SHUNT_COMPENSATOR_MODIFICATION` | Equipment modification |
| `STATIC_VAR_COMPENSATOR_CREATION` | Equipment creation |
| `LINE_CREATION` | Equipment creation |
| `LINE_MODIFICATION` | Equipment modification |
| `TWO_WINDINGS_TRANSFORMER_CREATION` | Equipment creation |
| `TWO_WINDINGS_TRANSFORMER_MODIFICATION` | Equipment modification |
| `SUBSTATION_CREATION` | Equipment creation |
| `SUBSTATION_MODIFICATION` | Equipment modification |
| `VOLTAGE_LEVEL_CREATION` | Equipment creation |
| `VOLTAGE_LEVEL_MODIFICATION` | Equipment modification |
| `VSC_CREATION` | HVDC creation |
| `VSC_MODIFICATION` | HVDC modification |
| `CONVERTER_STATION_CREATION` | HVDC creation |
| `CONVERTER_STATION_MODIFICATION` | HVDC modification |
| `LCC_CREATION` | HVDC creation |
| `LCC_MODIFICATION` | HVDC modification |
| `LCC_CONVERTER_STATION_CREATION` | HVDC creation |
| `LCC_CONVERTER_STATION_MODIFICATION` | HVDC modification |
| `EQUIPMENT_DELETION` | Deletion |
| `BY_FILTER_DELETION` | Deletion |
| `LINE_SPLIT_WITH_VOLTAGE_LEVEL` | Topology |
| `LINE_ATTACH_TO_VOLTAGE_LEVEL` | Topology |
| `LINES_ATTACH_TO_SPLIT_LINES` | Topology |
| `DELETE_VOLTAGE_LEVEL_ON_LINE` | Topology |
| `DELETE_ATTACHING_LINE` | Topology |
| `VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION` | Topology |
| `CREATE_COUPLING_DEVICE` | Topology |
| `CREATE_VOLTAGE_LEVEL_TOPOLOGY` | Topology |
| `CREATE_VOLTAGE_LEVEL_SECTION` | Topology |
| `MOVE_VOLTAGE_LEVEL_FEEDER_BAYS` | Topology |
| `GENERATOR_SCALING` | Scaling |
| `LOAD_SCALING` | Scaling |
| `GENERATION_DISPATCH` | Operational |
| `BALANCES_ADJUSTMENT_MODIFICATION` | Operational |
| `OPERATING_STATUS_MODIFICATION` | Operational |
| `VOLTAGE_INIT_MODIFICATION` | Operational |
| `EQUIPMENT_ATTRIBUTE_MODIFICATION` | Attribute |
| `GROOVY_SCRIPT` | Scripting |
| `TABULAR_MODIFICATION` | Bulk |
| `TABULAR_CREATION` | Bulk |
| `LIMIT_SETS_TABULAR_MODIFICATION` | Bulk |
| `BY_FORMULA_MODIFICATION` | Bulk |
| `MODIFICATION_BY_ASSIGNMENT` | Bulk |
| `COMPOSITE_MODIFICATION` | Orchestration |
| `MODIFICATION_REFERENCE` | Orchestration |

---

## 5. DTO Hierarchy & Fields

### 5.1 Equipment Modification DTOs

#### `EquipmentModificationInfos` ← `ModificationInfos`

Base class for all modifications targeting a specific equipment.

| Field | Type | Description |
|---|---|---|
| `equipmentId` | `String` | **Required.** ID of the target equipment |
| `properties` | `List<FreePropertyInfos>` | Optional key-value properties to set on the equipment |

#### `BasicEquipmentModificationInfos` ← `EquipmentModificationInfos`

Lightweight modification carrying only `equipmentId` and properties. Used for simple attribute changes.

---

### 5.2 Equipment Creation DTOs

#### `EquipmentCreationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `equipmentName` | `String` | Human-readable display name for the new equipment |

#### `InjectionCreationInfos` ← `EquipmentCreationInfos`

Base class for all injection equipment (loads, generators, batteries, etc.).

| Field | Type | Description |
|---|---|---|
| `voltageLevelId` | `String` | **Required.** ID of the voltage level where the injection is created |
| `busOrBusbarSectionId` | `String` | **Required.** Bus (bus-breaker topology) or busbar section ID (node-breaker topology) |
| `connectionName` | `String` | Name of the connection point |
| `connectionDirection` | `ConnectablePosition.Direction` | Direction of connection (`TOP`, `BOTTOM`, `UNDEFINED`) |
| `connectionPosition` | `Integer` | Position order of the feeder bay |
| `terminalConnected` | `Boolean` | Whether the terminal is connected at creation (default `true`) |

#### `BranchCreationInfos` ← `EquipmentCreationInfos`

Base class for branch equipment (lines, transformers).

| Field | Type | Description |
|---|---|---|
| `voltageLevelId1` | `String` | Voltage level ID at terminal 1 |
| `busOrBusbarSectionId1` | `String` | Bus/busbar section at terminal 1 |
| `connectionName1` | `String` | Connection name at terminal 1 |
| `connectionDirection1` | `ConnectablePosition.Direction` | Connection direction at terminal 1 |
| `connectionPosition1` | `Integer` | Feeder order position at terminal 1 |
| `connected1` | `Boolean` | Terminal 1 connection state |
| `voltageLevelId2` | `String` | Voltage level ID at terminal 2 |
| `busOrBusbarSectionId2` | `String` | Bus/busbar section at terminal 2 |
| `connectionName2` | `String` | Connection name at terminal 2 |
| `connectionDirection2` | `ConnectablePosition.Direction` | Connection direction at terminal 2 |
| `connectionPosition2` | `Integer` | Feeder order position at terminal 2 |
| `connected2` | `Boolean` | Terminal 2 connection state |
| `currentLimits1` | `CurrentLimitsInfos` | Permanent and temporary current limits at terminal 1 |
| `currentLimits2` | `CurrentLimitsInfos` | Permanent and temporary current limits at terminal 2 |

---

### 5.3 Injection Creation DTOs

#### `LoadCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `loadType` | `LoadType` | Type of load (`UNDEFINED`, `AUXILIARY`, `FICTITIOUS`) |
| `p0` | `double` | Active power consumption (MW) |
| `q0` | `double` | Reactive power consumption (MVar) |

#### `GeneratorCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `energySource` | `EnergySource` | Energy source type (`HYDRO`, `NUCLEAR`, `WIND`, `SOLAR`, `OTHER`, …) |
| `minP` | `double` | Minimum active power output (MW) |
| `maxP` | `double` | Maximum active power output (MW) |
| `ratedS` | `Double` | Rated nominal power (MVA) |
| `targetP` | `double` | Active power set point (MW) |
| `targetQ` | `Double` | Reactive power set point (MVar) |
| `voltageRegulationOn` | `boolean` | Whether voltage regulation is enabled |
| `targetV` | `Double` | Voltage set point (kV) |
| `plannedActivePowerSetPoint` | `Double` | Planning active power set point |
| `marginalCost` | `Double` | Marginal cost |
| `plannedOutageRate` | `Double` | Planning outage rate |
| `forcedOutageRate` | `Double` | Forced outage rate |
| `minQ` | `Double` | Minimum reactive power (MVar) |
| `maxQ` | `Double` | Maximum reactive power (MVar) |
| `reactiveCapabilityCurvePoints` | `List<ReactiveCapabilityCurvePointsInfos>` | Points of the reactive capability curve |
| `regulatingTerminalId` | `String` | ID of the regulating terminal (if remote regulation) |
| `regulatingTerminalType` | `String` | Equipment type of the regulating terminal |
| `regulatingTerminalVlId` | `String` | Voltage level of the regulating terminal |
| `qPercent` | `Double` | Reactive droop coefficient |
| `stepUpTransformerX` | `Double` | Step-up transformer reactance (Ω) |
| `directTransX` | `Double` | Direct axis transient reactance |
| `participate` | `Boolean` | Whether this generator participates in frequency control |
| `droop` | `Float` | Frequency droop coefficient |

#### `BatteryCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `minP` | `double` | Minimum active power (MW) |
| `maxP` | `double` | Maximum active power (MW) |
| `targetP` | `double` | Active power set point (MW) |
| `targetQ` | `Double` | Reactive power set point (MVar) |
| `participate` | `Boolean` | Participates in frequency regulation |
| `droop` | `Float` | Frequency droop |
| `minQ` / `maxQ` | `Double` | Reactive limits |
| `reactiveCapabilityCurvePoints` | `List<ReactiveCapabilityCurvePointsInfos>` | Reactive capability curve |

#### `ShuntCompensatorCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `maxSusceptance` | `Double` | Maximum susceptance (S) |
| `maxQAtNominalV` | `Double` | Maximum reactive power at nominal voltage (MVar) |
| `shuntCompensatorType` | `ShuntCompensatorType` | `CAPACITOR` or `REACTOR` |
| `sectionCount` | `Integer` | Current number of sections |
| `maximumSectionCount` | `Integer` | Maximum number of sections |
| `regulatingTerminalId` | `String` | Regulating terminal ID |
| `regulatingTerminalType` | `String` | Equipment type of regulating terminal |
| `regulatingTerminalVlId` | `String` | Voltage level of regulating terminal |
| `voltageSetpoint` | `Double` | Target voltage (kV) |
| `qPercent` | `Double` | Reactive droop |

#### `StaticVarCompensatorCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `minSusceptance` / `maxSusceptance` | `Double` | Susceptance limits (S) |
| `minQ` / `maxQ` | `Double` | Reactive power limits (MVar) |
| `regulationMode` | `StaticVarCompensator.RegulationMode` | `VOLTAGE`, `REACTIVE_POWER`, `OFF` |
| `voltageSetpoint` | `Double` | Voltage set point (kV) |
| `reactivePowerSetpoint` | `Double` | Reactive power set point (MVar) |
| `voltageRegulationType` | `VoltageRegulationType` | Local or distant regulation type |
| `regulatingTerminalId` | `String` | Regulating terminal ID |
| `regulatingTerminalType` | `String` | Regulating terminal equipment type |
| `regulatingTerminalVlId` | `String` | Regulating terminal voltage level |

---

### 5.4 Branch Creation DTOs

#### `LineCreationInfos` ← `BranchCreationInfos`

| Field | Type | Description |
|---|---|---|
| `r` | `double` | Resistance (Ω) |
| `x` | `double` | Reactance (Ω) |
| `g1` / `b1` | `double` | Conductance/susceptance at terminal 1 (S) |
| `g2` / `b2` | `double` | Conductance/susceptance at terminal 2 (S) |

#### `TwoWindingsTransformerCreationInfos` ← `BranchCreationInfos`

| Field | Type | Description |
|---|---|---|
| `r` | `double` | Resistance (Ω) |
| `x` | `double` | Reactance (Ω) |
| `g` | `double` | Conductance (S) |
| `b` | `double` | Susceptance (S) |
| `ratedU1` | `double` | Rated voltage at terminal 1 (kV) |
| `ratedU2` | `double` | Rated voltage at terminal 2 (kV) |
| `ratedS` | `Double` | Rated apparent power (MVA) |
| `ratioTapChanger` | `RatioTapChangerCreationInfos` | Ratio tap changer definition |
| `phaseTapChanger` | `PhaseTapChangerCreationInfos` | Phase tap changer definition |

---

### 5.5 Injection Modification DTOs

All injection modification DTOs use `AttributeModification<T>` wrappers on their fields, allowing partial updates where `null` means "leave unchanged".

#### `InjectionModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `voltageLevelId` | `AttributeModification<String>` | New voltage level ID |
| `busOrBusbarSectionId` | `AttributeModification<String>` | New bus/busbar section ID |

#### `LoadModificationInfos` ← `InjectionModificationInfos`

| Field | Type | Description |
|---|---|---|
| `loadType` | `AttributeModification<LoadType>` | New load type |
| `p0` | `AttributeModification<Double>` | New active power consumption |
| `q0` | `AttributeModification<Double>` | New reactive power consumption |

#### `GeneratorModificationInfos` ← `InjectionModificationInfos`

| Field | Type | Description |
|---|---|---|
| `energySource` | `AttributeModification<EnergySource>` | New energy source |
| `minP` / `maxP` | `AttributeModification<Double>` | Active power limits |
| `ratedS` | `AttributeModification<Double>` | Rated power |
| `targetP` / `targetQ` / `targetV` | `AttributeModification<Double>` | Set points |
| `voltageRegulationOn` | `AttributeModification<Boolean>` | Voltage regulation state |
| `participate` | `AttributeModification<Boolean>` | Frequency participation |
| `droop` | `AttributeModification<Float>` | Droop coefficient |
| `reactiveCapabilityCurvePoints` | `List<ReactiveCapabilityCurvePointsInfos>` | New curve points |

#### `BatteryModificationInfos` ← `InjectionModificationInfos`

Fields mirror `GeneratorModificationInfos` for applicable attributes.

---

### 5.6 Branch Modification DTOs

#### `BranchModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `r` | `AttributeModification<Double>` | Resistance |
| `x` | `AttributeModification<Double>` | Reactance |
| `operationalLimitsGroups1` | `List<OperationalLimitsGroupModificationInfos>` | Limit groups at terminal 1 |
| `operationalLimitsGroups2` | `List<OperationalLimitsGroupModificationInfos>` | Limit groups at terminal 2 |
| `connected1` / `connected2` | `AttributeModification<Boolean>` | Terminal connection states |

#### `LineModificationInfos` ← `BranchModificationInfos`

Adds `g1`, `b1`, `g2`, `b2` `AttributeModification<Double>` fields.

#### `TwoWindingsTransformerModificationInfos` ← `BranchModificationInfos`

Adds `g`, `b`, `ratedU1`, `ratedU2`, `ratedS`, `ratioTapChanger`, `phaseTapChanger` as attribute modifications.

---

### 5.7 Substation & Voltage Level DTOs

#### `SubstationCreationInfos` ← `EquipmentCreationInfos`

| Field | Type | Description |
|---|---|---|
| `country` | `Country` | Country code (ISO 3166-1 alpha-2) |
| `voltageLevels` | `List<VoltageLevelCreationInfos>` | Voltage levels to create inside the substation |

#### `SubstationModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `country` | `AttributeModification<Country>` | New country |

#### `VoltageLevelCreationInfos` ← `EquipmentCreationInfos`

| Field | Type | Description |
|---|---|---|
| `substationId` | `String` | Parent substation ID |
| `nominalV` | `double` | Nominal voltage (kV) |
| `lowVoltageLimit` | `Double` | Low voltage limit (kV) |
| `highVoltageLimit` | `Double` | High voltage limit (kV) |
| `ipMin` / `ipMax` | `Double` | Minimum/maximum current (A) |
| `busbarCount` | `int` | Number of busbar sections (node-breaker) |
| `sectionCount` | `int` | Number of sections per busbar (node-breaker) |
| `switchKinds` | `List<SwitchKind>` | Switch types between sections |
| `couplingDevices` | `List<CouplingDeviceInfos>` | Coupling devices |
| `topologyKind` | `TopologyKind` | `BUS_BREAKER` or `NODE_BREAKER` |

#### `VoltageLevelModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `nominalV` | `AttributeModification<Double>` | Nominal voltage |
| `lowVoltageLimit` / `highVoltageLimit` | `AttributeModification<Double>` | Voltage limits |
| `ipMin` / `ipMax` | `AttributeModification<Double>` | Current limits |

---

### 5.8 HVDC DTOs

#### `VscCreationInfos` ← `EquipmentCreationInfos`

| Field | Type | Description |
|---|---|---|
| `dcNominalVoltage` | `double` | DC nominal voltage (kV) |
| `dcResistance` | `double` | DC resistance (Ω) |
| `nominalV` | `double` | AC nominal voltage (kV) |
| `maxP` | `double` | Maximum active power (MW) |
| `activePowerSetpoint` | `double` | Active power set point (MW) |
| `operatorActivePowerLimitSide1` / `Side2` | `Float` | Operator active power limits per side |
| `convertersMode` | `HvdcLine.ConvertersMode` | `SIDE_1_RECTIFIER_SIDE_2_INVERTER` or reversed |
| `converterStation1` / `converterStation2` | `ConverterStationCreationInfos` | VSC converter stations |
| `angleDroopActivePowerControl` | `Boolean` | Enable droop control |
| `p0` | `Float` | Active power reference for droop |
| `droop` | `Float` | Droop coefficient |

#### `LccCreationInfos` ← `EquipmentCreationInfos`

| Field | Type | Description |
|---|---|---|
| `nominalV` / `dcNominalVoltage` / `dcResistance` | `double` | Electrical parameters |
| `maxP` | `double` | Maximum active power (MW) |
| `activePowerSetpoint` | `double` | Active power set point (MW) |
| `convertersMode` | `HvdcLine.ConvertersMode` | Converter mode |
| `converterStation1` / `converterStation2` | `LccConverterStationCreationInfos` | LCC converter stations |

---

### 5.9 Topology Modification DTOs

#### `LineSplitWithVoltageLevelInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `lineToSplitId` | `String` | ID of the line to split |
| `percent` | `double` | Position of the split point as a percentage from terminal 1 |
| `mayNewVoltageLevelInfos` | `VoltageLevelCreationInfos` | New voltage level to create at the split point (if not existing) |
| `existingVoltageLevelId` | `String` | Existing voltage level to use at the split point |
| `bbsOrBusId` | `String` | Bus/busbar section in the split voltage level |
| `newLine1Id` / `newLine2Id` | `String` | IDs for the two resulting lines |
| `newLine1Name` / `newLine2Name` | `String` | Names for the two resulting lines |

#### `LineAttachToVoltageLevelInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `lineToAttachToId` | `String` | ID of the line to attach |
| `percent` | `double` | Split position as percentage |
| `attachmentPointId` | `String` | ID of the attachment voltage level or new bus |
| `attachmentPointName` | `String` | Name of the attachment point |
| `mayNewVoltageLevelInfos` | `VoltageLevelCreationInfos` | New voltage level if needed |
| `existingVoltageLevelId` | `String` | Existing voltage level to attach to |
| `bbsOrBusId` | `String` | Bus/busbar section ID at attachment |
| `attachmentLineId` / `attachmentLineName` | `String` | New attachment line identity |
| `newLine1Id` / `newLine1Name` | `String` | First segment of the split line |
| `newLine2Id` / `newLine2Name` | `String` | Second segment of the split line |

#### `DeleteVoltageLevelOnLineInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `lineToAttachTo1Id` / `lineToAttachTo2Id` | `String` | The two lines on each side of the voltage level |
| `replacingLine1Id` / `replacingLine1Name` | `String` | New line replacing the split configuration |

#### `CreateVoltageLevelTopologyInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `substationId` | `String` | Target substation |
| `voltageLevelId` / `voltageLevelName` | `String` | New voltage level identity |
| `nominalV` | `double` | Nominal voltage (kV) |
| `lowVoltageLimit` / `highVoltageLimit` | `Double` | Voltage limits |
| `busbarCount` / `sectionCount` | `int` | Topology dimensions |
| `switchKinds` | `List<SwitchKind>` | Inter-section switch types |

#### `CreateVoltageLevelSectionInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `voltageLevelId` | `String` | Target voltage level |
| `switchKinds` | `List<SwitchKind>` | Switch types for the new section |

#### `MoveVoltageLevelFeederBaysInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `voltageLevelId` | `String` | Target voltage level |
| `feederBaysMoves` | `List<MoveFeederBayInfos>` | Ordered list of bay moves |

#### `VoltageLevelTopologyModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `busbarSectionToSwitchesAttributes` | `List<BusbarSectionToSwitchesAttributes>` | Map of busbar section to switch configuration |

---

### 5.10 Deletion DTOs

#### `EquipmentDeletionInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `equipmentType` | `String` | Type name of the equipment to delete |
| `equipmentInfos` | `AbstractEquipmentDeletionInfos` | Additional deletion parameters (e.g. HVDC shunt compensators) |

#### `ByFilterDeletionInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `equipmentType` | `IdentifiableType` | Type of equipment to delete |
| `filters` | `List<FilterInfos>` | Filters identifying the equipment to delete |

---

### 5.11 Scaling & Dispatch DTOs

#### `ScalingInfos` ← `ModificationInfos`

Base class for scaling modifications.

| Field | Type | Description |
|---|---|---|
| `variations` | `List<ScalingVariationInfos>` | List of scaling variations to apply |
| `variationType` | `VariationType` | `DELTA_P` (add delta) or `TARGET_P` (set absolute value) |

#### `ScalingVariationInfos`

| Field | Type | Description |
|---|---|---|
| `filters` | `List<FilterInfos>` | Equipment selection filters |
| `variationMode` | `VariationMode` | How power is distributed (`PROPORTIONAL_TO_PMAX`, `REGULAR_DISTRIBUTION`, `STACKING_UP`, `VENTILATION`) |
| `variationValue` | `double` | The variation amount (MW) |
| `reactiveVariationMode` | `ReactiveVariationMode` | Reactive power handling (`CONSTANT_Q`, `TAN_PHI_FIXED`) |

#### `GeneratorScalingInfos` ← `ScalingInfos`

Adds no additional fields. Delegates to `GeneratorScaling` implementation.

#### `LoadScalingInfos` ← `ScalingInfos`

Adds no additional fields. Delegates to `LoadScaling` implementation.

#### `GenerationDispatchInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `lossCoefficient` | `double` | Network loss coefficient (%) |
| `defaultOutageRate` | `double` | Default generator outage rate |
| `generatorsWithoutOutage` | `List<GeneratorsWithoutOutageInfos>` | Generators exempt from outage |
| `generatorsWithFixedSupply` | `List<GeneratorsFilterInfos>` | Generators with fixed supply |
| `generatorsFrequencyReserve` | `List<GeneratorsFrequencyReserveInfos>` | Frequency reserve requirements |
| `substationsGeneratorsOrdering` | `List<SubstationsGeneratorsOrderingInfos>` | Ordering of generators by substation |
| `loadFlowParametersUuid` | `UUID` | UUID of load-flow parameters to use |

---

### 5.12 Bulk / Programmatic Modification DTOs

#### `EquipmentAttributeModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `equipmentAttributeName` | `String` | Name of the attribute to change |
| `equipmentAttributeValue` | `Object` | New value for the attribute |
| `equipmentType` | `IdentifiableType` | Type of the targeted equipment |

#### `GroovyScriptInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `script` | `String` | **Required.** Groovy script code to execute against the `network` variable |

#### `TabularModificationInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `modificationType` | `ModificationType` | Type of individual row modification |
| `modifications` | `List<ModificationInfos>` | One DTO per equipment to modify |

#### `TabularCreationInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `creationType` | `ModificationType` | Type of individual row creation |
| `creations` | `List<ModificationInfos>` | One DTO per equipment to create |

#### `ByFormulaModificationInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `identifiableType` | `IdentifiableType` | Equipment type to target |
| `formulaInfosList` | `List<FormulaInfos>` | List of formula descriptors |

#### `ModificationByAssignmentInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `identifiableType` | `IdentifiableType` | Equipment type to target |
| `assignmentInfosList` | `List<AbstractAssignmentInfos>` | List of assignment rules |

---

### 5.13 Operational Modification DTOs

#### `OperatingStatusModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `energizedVoltageLevelId` | `String` | Voltage level energizing the equipment |
| `action` | `ActionType` | `LOCKOUT`, `TRIP`, `SWITCH_ON`, `ENERGISE_END_ONE`, `ENERGISE_END_TWO` |

#### `VoltageInitModificationInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `generators` | `List<VoltageInitGeneratorModificationInfos>` | Generator set points to initialise |
| `transformers` | `List<VoltageInitTransformerModificationInfos>` | Transformer tap positions to set |
| `staticVarCompensators` | `List<VoltageInitStaticVarCompensatorModificationInfos>` | SVC settings |
| `vscConverterStations` | `List<VoltageInitVscConverterStationModificationInfos>` | VSC station settings |
| `shuntCompensators` | `List<VoltageInitShuntCompensatorModificationInfos>` | Shunt section counts |
| `buses` | `List<VoltageInitBusModificationInfos>` | Bus voltage angles |

#### `BalancesAdjustmentModificationInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `areas` | `List<BalancesAdjustmentAreaInfos>` | Areas with their net position targets |
| `loadFlowParametersUuid` | `UUID` | UUID of load-flow parameters |

---

### 5.14 Composition & Reference DTOs

#### `CompositeModificationInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `name` | `String` | Human-readable name of the composite |
| `modificationsInfos` | `List<ModificationInfos>` | Ordered list of sub-modifications |
| `maxDepth` | `Integer` | Maximum nesting depth (computed at runtime) |

Sub-modifications are only executed when both `activated = true` and `stashed = false`. Errors in sub-modifications are caught and logged; execution continues with the next sub-modification.

#### `ModificationReferenceInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `referenceId` | `UUID` | **Required.** UUID of the referenced modification |
| `referenceType` | `Type` | `BASIC` (single modification) or `DIRECTORY` (a group) |
| `referenceInfos` | `ModificationInfos` | **Required.** Resolved modification DTO (must be populated before use) |

`check()` asserts that `referenceId`, `referenceType`, and `referenceInfos` are all non-null.

---

## 6. Supporting Value Objects

### `AttributeModification<T>`

Generic wrapper for partial attribute updates in modification DTOs.

| Field | Type | Description |
|---|---|---|
| `value` | `T` | The new value (may be null for UNSET) |
| `op` | `OperationType` | `SET` or `UNSET` |

```java
// Set a new value
AttributeModification<Double> setP = new AttributeModification<>(200.0, OperationType.SET);

// Unset (reset to null/default)
AttributeModification<Double> unsetP = new AttributeModification<>(null, OperationType.UNSET);

// Factory helper
AttributeModification<Double> fromValue = AttributeModification.toAttributeModification(200.0, OperationType.SET);
```

`applyModification(initialValue)` returns the new value according to the operation type.

---

### `FilterInfos`

| Field | Type | Description |
|---|---|---|
| `id` | `UUID` | UUID of the filter |
| `name` | `String` | Display name of the filter |

---

### `CurrentLimitsInfos`

| Field | Type | Description |
|---|---|---|
| `permanentLimit` | `Double` | Permanent current limit (A) |
| `temporaryLimits` | `List<CurrentTemporaryLimitCreationInfos>` | Temporary limits with acceptable durations |

---

### `FreePropertyInfos`

| Field | Type | Description |
|---|---|---|
| `name` | `String` | Property name |
| `value` | `String` | Property value |
| `deletionMark` | `boolean` | When `true`, the property is removed from the equipment |

---

### `ReactiveCapabilityCurvePointsInfos`

| Field | Type | Description |
|---|---|---|
| `p` | `double` | Active power point (MW) |
| `minQ` | `double` | Minimum reactive power at that point (MVar) |
| `maxQ` | `double` | Maximum reactive power at that point (MVar) |

---

### `TapChangerCreationInfos` / `RatioTapChangerCreationInfos` / `PhaseTapChangerCreationInfos`

Common tap changer fields:

| Field | Type | Description |
|---|---|---|
| `lowTapPosition` | `int` | Lowest tap position index |
| `tapPosition` | `int` | Current tap position |
| `regulating` | `boolean` | Whether the tap changer is in automatic regulation |
| `steps` | `List<TapChangerStepCreationInfos>` | List of tap steps |

`RatioTapChangerCreationInfos` adds: `loadTapChangingCapabilities`, `targetV`, `targetDeadband`, `regulatingTerminalId/Type/VlId`.

`PhaseTapChangerCreationInfos` adds: `regulationMode` (`CURRENT_LIMITER`, `ACTIVE_POWER_CONTROL`, `FIXED_TAP`), `regulationValue`, `targetDeadband`.

---

## 7. `NetworkModificationException`

**Package:** `org.gridsuite.modification`  
**Extends:** `com.powsybl.commons.PowsyblException`

### Constructor

```java
public NetworkModificationException(Type type, String message)
public NetworkModificationException(Type type, Exception cause)
public NetworkModificationException(Type type)
```

### `Type` Enum (selection)

| Type | HTTP Status | Meaning |
|---|---|---|
| `NETWORK_NOT_FOUND` | 404 | Network not found |
| `VARIANT_NOT_FOUND` | 404 | Network variant not found |
| `EQUIPMENT_NOT_FOUND` | 404 | Generic equipment not found |
| `GENERATOR_NOT_FOUND` | 404 | Generator not found |
| `LOAD_NOT_FOUND` | 404 | Load not found |
| `LINE_NOT_FOUND` | 404 | Line not found |
| `VOLTAGE_LEVEL_NOT_FOUND` | 404 | Voltage level not found |
| `SUBSTATION_NOT_FOUND` | 404 | Substation not found |
| `SWITCH_NOT_FOUND` | 404 | Switch not found |
| `BATTERY_NOT_FOUND` | 404 | Battery not found |
| `TWO_WINDINGS_TRANSFORMER_NOT_FOUND` | 404 | Transformer not found |
| `GENERATOR_ALREADY_EXISTS` | 400 | Generator ID already in use |
| `LOAD_ALREADY_EXISTS` | 400 | Load ID already in use |
| `LINE_ALREADY_EXISTS` | 400 | Line ID already in use |
| `ATTRIBUTE_NOT_EDITABLE` | 400 | The requested attribute cannot be modified |
| `GROOVY_SCRIPT_EMPTY` | 400 | Empty Groovy script provided |
| `GROOVY_SCRIPT_ERROR` | 400 | Groovy execution error |
| `CREATE_GENERATOR_ERROR` | 500 | Error during generator creation |
| `CREATE_LOAD_ERROR` | 500 | Error during load creation |
| `CREATE_LINE_ERROR` | 500 | Error during line creation |
| `MODIFICATION_ERROR` | 500 | Generic modification error |
| `WRONG_EQUIPMENT_TYPE` | 500 | Equipment has unexpected type |

---

## 8. Enumerations

### `OperationType`

| Value | Description |
|---|---|
| `SET` | Set the attribute to a new value |
| `UNSET` | Remove / reset the attribute |

### `VariationType`

| Value | Description |
|---|---|
| `DELTA_P` | Apply a relative change (add delta) |
| `TARGET_P` | Set an absolute target value |

### `VariationMode`

| Value | Description |
|---|---|
| `PROPORTIONAL_TO_PMAX` | Scale proportionally to each equipment's Pmax |
| `PROPORTIONAL_TO_P` | Scale proportionally to current output |
| `REGULAR_DISTRIBUTION` | Distribute equally |
| `STACKING_UP` | Stack up modifications sequentially |
| `VENTILATION` | Ventilate based on a coefficient |

### `ReactiveVariationMode`

| Value | Description |
|---|---|
| `CONSTANT_Q` | Keep reactive power constant |
| `TAN_PHI_FIXED` | Keep tan(φ) = Q/P fixed |

### `TapChangerType`

| Value | Description |
|---|---|
| `RATIO` | Ratio tap changer |
| `PHASE` | Phase-shifting tap changer |

### `ShuntCompensatorType`

| Value | Description |
|---|---|
| `CAPACITOR` | Capacitor bank (reactive power injection) |
| `REACTOR` | Reactor bank (reactive power absorption) |

### `VoltageRegulationType`

| Value | Description |
|---|---|
| `DISTANT` | Regulates voltage at a remote terminal |
| `LOCAL` | Regulates voltage at its own terminal |

### `RegulationSide`

| Value | Description |
|---|---|
| `SIDE_1` | Regulation applies at terminal 1 |
| `SIDE_2` | Regulation applies at terminal 2 |

---

## 9. Usage Examples

### Example 1 — Create a load

```java
LoadCreationInfos loadInfos = LoadCreationInfos.builder()
        .equipmentId("LOAD_A")
        .equipmentName("Load A")
        .voltageLevelId("VL1")
        .busOrBusbarSectionId("BUS1")
        .loadType(LoadType.UNDEFINED)
        .p0(50.0)
        .q0(10.0)
        .build();

AbstractModification mod = loadInfos.toModification();
mod.check(network);
mod.apply(network, reportNode);
```

### Example 2 — Partially modify a generator

```java
GeneratorModificationInfos genModif = GeneratorModificationInfos.builder()
        .equipmentId("GEN1")
        // only change targetP; leave everything else untouched
        .targetP(AttributeModification.toAttributeModification(350.0, OperationType.SET))
        .voltageRegulationOn(AttributeModification.toAttributeModification(true, OperationType.SET))
        .build();

genModif.toModification().apply(network, reportNode);
```

### Example 3 — Composite modification

```java
List<ModificationInfos> subMods = List.of(loadInfos, genModif);

CompositeModificationInfos composite = CompositeModificationInfos.builder()
        .name("Morning scenario")
        .modificationsInfos(subMods.stream()
                .map(m -> { m.setActivated(true); m.setStashed(false); return m; })
                .toList())
        .build();

AbstractModification compositeModification = composite.toModification();
compositeModification.initApplicationContext(filterService, loadFlowService);
compositeModification.apply(network, reportNode);
```

### Example 4 — Delete equipment matching a filter

```java
ByFilterDeletionInfos deletion = ByFilterDeletionInfos.builder()
        .equipmentType(IdentifiableType.LOAD)
        .filters(List.of(new FilterInfos(myFilterUuid, "My filter")))
        .build();

AbstractModification mod = deletion.toModification();
mod.initApplicationContext(filterService, null);
mod.check(network);
mod.apply(network, reportNode);
```

### Example 5 — JSON deserialisation

Because `ModificationInfos` uses Jackson polymorphism, a heterogeneous JSON array is handled transparently:

```json
[
  { "type": "LOAD_CREATION", "equipmentId": "LD1", "voltageLevelId": "VL1", "busOrBusbarSectionId": "BUS1", "p0": 100.0, "q0": 20.0 },
  { "type": "GENERATOR_MODIFICATION", "equipmentId": "GEN1", "targetP": { "value": 400.0, "op": "SET" } }
]
```

```java
ObjectMapper mapper = new ObjectMapper();
List<ModificationInfos> modifications = mapper.readValue(json,
        mapper.getTypeFactory().constructCollectionType(List.class, ModificationInfos.class));
```

---

*Generated for `gridsuite-network-modification` version `0.88.0-SNAPSHOT` — © RTE (http://www.rte-france.com) — MPL-2.0*
