# API Reference — `gridsuite-network-modification`

**Maven Coordinates**

```xml
<dependency>
    <groupId>org.gridsuite</groupId>
    <artifactId>gridsuite-network-modification</artifactId>
    <version>1.8.0-SNAPSHOT</version>
</dependency>
```

---

## Table of Contents

1. [Core Interfaces](#1-core-interfaces)
2. [AbstractModification Lifecycle](#2-abstractmodification-lifecycle)
3. [ModificationInfos — Base DTO](#3-modificationinfos--base-dto)
4. [ModificationType Enum](#4-modificationtype-enum)
5. [DTO Hierarchy & Field References](#5-dto-hierarchy--field-references)
   - 5.1 [Equipment Modification DTOs](#51-equipment-modification-dtos)
   - 5.2 [Equipment Creation Base DTOs](#52-equipment-creation-base-dtos)
   - 5.3 [Injection Creation DTOs](#53-injection-creation-dtos)
   - 5.4 [Branch Creation DTOs](#54-branch-creation-dtos)
   - 5.5 [Injection Modification DTOs](#55-injection-modification-dtos)
   - 5.6 [Branch Modification DTOs](#56-branch-modification-dtos)
   - 5.7 [Substation & Voltage Level DTOs](#57-substation--voltage-level-dtos)
   - 5.8 [HVDC DTOs](#58-hvdc-dtos)
   - 5.9 [Topology Modification DTOs](#59-topology-modification-dtos)
   - 5.10 [Deletion DTOs](#510-deletion-dtos)
   - 5.11 [Scaling & Dispatch DTOs](#511-scaling--dispatch-dtos)
   - 5.12 [Bulk & Programmatic Modification DTOs](#512-bulk--programmatic-modification-dtos)
   - 5.13 [Operational Modification DTOs](#513-operational-modification-dtos)
   - 5.14 [Composition & Reference DTOs](#514-composition--reference-dtos)
6. [Supporting Value Objects](#6-supporting-value-objects)
7. [Exceptions & Error Types](#7-exceptions--error-types)
8. [Enumerations Reference](#8-enumerations-reference)
9. [Usage Examples](#9-usage-examples)

---

## 1. Core Interfaces

### `IFilterService`

**Package:** `org.gridsuite.modification`

Interface to decouple filter evaluation and remote filter service resolution from the core modification engine.

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

Interface for retrieving stored load-flow parameters by UUID for modifications that execute power flow simulations.

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

## 2. `AbstractModification` Lifecycle

**Package:** `org.gridsuite.modification.modifications`  
**Extends:** `com.powsybl.iidm.modification.AbstractNetworkModification`

The abstract base class for all concrete modification implementations.

### Methods

| Method | Description |
|---|---|
| `void check(Network network)` | Validates inputs and constraints against the live network. Throws `NetworkModificationException` on conflict or missing prerequisites. Default implementation does nothing. |
| `void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService)` | Injects external service instances. Called prior to `check` when service dependencies exist. Default implementation does nothing. |
| `void apply(Network network, ReportNode subReportNode)` | Mutates the `Network` and logs structured progress and audit messages to `subReportNode`. |
| `void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode)` | Variant supporting a custom naming strategy. Defaults to delegating to `apply(network, subReportNode)`. |
| `String getName()` | Returns the descriptive name for the modification type. |

---

## 3. `ModificationInfos` — Base DTO

**Package:** `org.gridsuite.modification.dto`  
**JSON Discriminator Property:** `type` (matches `ModificationType` enum name)

### Fields

| Field | Type | Description |
|---|---|---|
| `uuid` | `UUID` | Unique identifier of this modification instance |
| `type` | `ModificationType` | Discriminator property; automatically derived from `@JsonTypeName` on concrete subclasses |
| `date` | `Instant` | Creation or update timestamp |
| `stashed` | `Boolean` | Staging flag (default `false`). When `true`, skipped during execution |
| `activated` | `Boolean` | Activation flag (default `true`). When `false`, skipped during execution |
| `description` | `String` | Optional free-text description |
| `messageType` | `String` | i18n message template key |
| `messageValues` | `String` | Serialized message interpolation parameters |

### Methods

| Method | Description |
|---|---|
| `AbstractModification toModification()` | Factory method instantiating the corresponding `AbstractModification`. |
| `ReportNode createSubReportNode(ReportNode reportNode)` | Creates and attaches a child `ReportNode` with the appropriate message template. |
| `void check()` | Validates internal DTO fields before conversion. |
| `ModificationType getType()` | Returns the modification type enum value. |
| `Map<String, String> getMapMessageValues()` | Returns message interpolation values as key-value pairs. |

---

## 4. `ModificationType` Enum

**Package:** `org.gridsuite.modification`

Supported modification types in `ModificationType.java`:

| Modification Type | Category |
|---|---|
| `LOAD_CREATION` | Injections |
| `LOAD_MODIFICATION` | Injections |
| `BATTERY_CREATION` | Injections |
| `BATTERY_MODIFICATION` | Injections |
| `GENERATOR_CREATION` | Injections |
| `GENERATOR_MODIFICATION` | Injections |
| `SHUNT_COMPENSATOR_CREATION` | Injections |
| `SHUNT_COMPENSATOR_MODIFICATION` | Injections |
| `STATIC_VAR_COMPENSATOR_CREATION` | Injections |
| `LINE_CREATION` | Branches |
| `LINE_MODIFICATION` | Branches |
| `TWO_WINDINGS_TRANSFORMER_CREATION` | Branches |
| `TWO_WINDINGS_TRANSFORMER_MODIFICATION` | Branches |
| `SUBSTATION_CREATION` | Substations & Voltage Levels |
| `SUBSTATION_MODIFICATION` | Substations & Voltage Levels |
| `VOLTAGE_LEVEL_CREATION` | Substations & Voltage Levels |
| `VOLTAGE_LEVEL_MODIFICATION` | Substations & Voltage Levels |
| `VSC_CREATION` | HVDC |
| `VSC_MODIFICATION` | HVDC |
| `CONVERTER_STATION_CREATION` | HVDC |
| `CONVERTER_STATION_MODIFICATION` | HVDC |
| `LCC_CREATION` | HVDC |
| `LCC_MODIFICATION` | HVDC |
| `LCC_CONVERTER_STATION_CREATION` | HVDC |
| `LCC_CONVERTER_STATION_MODIFICATION` | HVDC |
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
| `GENERATOR_SCALING` | Scaling & Dispatch |
| `LOAD_SCALING` | Scaling & Dispatch |
| `GENERATION_DISPATCH` | Scaling & Dispatch |
| `BALANCES_ADJUSTMENT_MODIFICATION` | Operational |
| `OPERATING_STATUS_MODIFICATION` | Operational |
| `VOLTAGE_INIT_MODIFICATION` | Operational |
| `EQUIPMENT_ATTRIBUTE_MODIFICATION` | Bulk & Programmatic |
| `GROOVY_SCRIPT` | Bulk & Programmatic |
| `TABULAR_MODIFICATION` | Bulk & Programmatic |
| `TABULAR_CREATION` | Bulk & Programmatic |
| `LIMIT_SETS_TABULAR_MODIFICATION` | Bulk & Programmatic |
| `BY_FORMULA_MODIFICATION` | Bulk & Programmatic |
| `MODIFICATION_BY_ASSIGNMENT` | Bulk & Programmatic |
| `COMPOSITE_MODIFICATION` | Composition & Reference |
| `MODIFICATION_REFERENCE` | Composition & Reference |

---

## 5. DTO Hierarchy & Field References

### 5.1 Equipment Modification DTOs

#### `EquipmentModificationInfos` ← `ModificationInfos`

Base class for modifications targeting an existing equipment entity.

| Field | Type | Description |
|---|---|---|
| `equipmentId` | `String` | **Required.** Identifier of the target equipment |
| `properties` | `List<FreePropertyInfos>` | Custom key-value properties to assign or delete |

#### `BasicEquipmentModificationInfos` ← `EquipmentModificationInfos`

Lightweight DTO for simple equipment property alterations.

---

### 5.2 Equipment Creation Base DTOs

#### `EquipmentCreationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `equipmentName` | `String` | Human-readable name for the new equipment |

#### `InjectionCreationInfos` ← `EquipmentCreationInfos`

Base class for injection equipment creations.

| Field | Type | Description |
|---|---|---|
| `voltageLevelId` | `String` | **Required.** Voltage level identifier |
| `busOrBusbarSectionId` | `String` | **Required.** Bus (bus-breaker) or busbar section (node-breaker) identifier |
| `connectionName` | `String` | Feeder connection name |
| `connectionDirection` | `ConnectablePosition.Direction` | Direction: `TOP`, `BOTTOM`, or `UNDEFINED` |
| `connectionPosition` | `Integer` | Feeder position index in the bay |
| `terminalConnected` | `Boolean` | Initial connection status (default `true`) |

#### `BranchCreationInfos` ← `EquipmentCreationInfos`

Base class for branch equipment creations (lines, transformers).

| Field | Type | Description |
|---|---|---|
| `voltageLevelId1` / `voltageLevelId2` | `String` | Voltage level ID at terminals 1 & 2 |
| `busOrBusbarSectionId1` / `Id2` | `String` | Bus/busbar section ID at terminals 1 & 2 |
| `connectionName1` / `Name2` | `String` | Connection name at terminals 1 & 2 |
| `connectionDirection1` / `Direction2` | `ConnectablePosition.Direction` | Connection direction at terminals 1 & 2 |
| `connectionPosition1` / `Position2` | `Integer` | Feeder position order at terminals 1 & 2 |
| `connected1` / `connected2` | `Boolean` | Connection status at terminals 1 & 2 |
| `currentLimits1` / `currentLimits2` | `CurrentLimitsInfos` | Permanent and temporary current limits at terminals 1 & 2 |

---

### 5.3 Injection Creation DTOs

#### `LoadCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `loadType` | `LoadType` | `UNDEFINED`, `AUXILIARY`, `FICTITIOUS` |
| `p0` | `double` | Active power consumption (MW) |
| `q0` | `double` | Reactive power consumption (MVar) |

#### `GeneratorCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `energySource` | `EnergySource` | `HYDRO`, `NUCLEAR`, `WIND`, `SOLAR`, `THERMAL`, `OTHER`, … |
| `minP` / `maxP` | `double` | Minimum / maximum active power output (MW) |
| `ratedS` | `Double` | Rated nominal apparent power (MVA) |
| `targetP` | `double` | Active power set point (MW) |
| `targetQ` | `Double` | Reactive power set point (MVar) |
| `voltageRegulationOn` | `boolean` | Voltage regulation enabled |
| `targetV` | `Double` | Voltage set point (kV) |
| `minQ` / `maxQ` | `Double` | Minimum / maximum reactive power limits (MVar) |
| `plannedActivePowerSetPoint` | `Double` | Planned active power set point |
| `marginalCost` | `Double` | Marginal generation cost |
| `plannedOutageRate` / `forcedOutageRate` | `Double` | Outage rates |
| `reactiveCapabilityCurvePoints` | `List<ReactiveCapabilityCurvePointsInfos>` | Reactive capability curve definition |
| `regulatingTerminalId` | `String` | Remote terminal ID for regulation |
| `regulatingTerminalType` | `String` | Remote terminal equipment type |
| `regulatingTerminalVlId` | `String` | Remote terminal voltage level ID |
| `qPercent` | `Double` | Reactive droop coefficient |
| `stepUpTransformerX` | `Double` | Step-up transformer reactance (Ω) |
| `directTransX` | `Double` | Direct-axis transient reactance |
| `participate` | `Boolean` | Participation in frequency control |
| `droop` | `Float` | Frequency droop coefficient |

#### `BatteryCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `minP` / `maxP` | `double` | Active power limits (MW) |
| `targetP` / `targetQ` | `double` / `Double` | Active / reactive power set points |
| `participate` | `Boolean` | Frequency control participation |
| `droop` | `Float` | Frequency droop coefficient |
| `minQ` / `maxQ` | `Double` | Reactive power limits |
| `reactiveCapabilityCurvePoints` | `List<ReactiveCapabilityCurvePointsInfos>` | Reactive capability curve points |

#### `ShuntCompensatorCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `maxSusceptance` | `Double` | Maximum susceptance (S) |
| `maxQAtNominalV` | `Double` | Maximum reactive power at nominal voltage (MVar) |
| `shuntCompensatorType` | `ShuntCompensatorType` | `CAPACITOR` or `REACTOR` |
| `sectionCount` / `maximumSectionCount` | `Integer` | Current and maximum section count |
| `regulatingTerminalId` / `Type` / `VlId` | `String` | Regulating terminal descriptor |
| `voltageSetpoint` | `Double` | Voltage set point (kV) |
| `qPercent` | `Double` | Reactive droop percentage |

#### `StaticVarCompensatorCreationInfos` ← `InjectionCreationInfos`

| Field | Type | Description |
|---|---|---|
| `minSusceptance` / `maxSusceptance` | `Double` | Susceptance boundaries (S) |
| `minQ` / `maxQ` | `Double` | Reactive power boundaries (MVar) |
| `regulationMode` | `StaticVarCompensator.RegulationMode` | `VOLTAGE`, `REACTIVE_POWER`, `OFF` |
| `voltageSetpoint` | `Double` | Target voltage set point (kV) |
| `reactivePowerSetpoint` | `Double` | Target reactive power set point (MVar) |
| `voltageRegulationType` | `VoltageRegulationType` | `LOCAL` or `DISTANT` |
| `regulatingTerminalId` / `Type` / `VlId` | `String` | Remote regulation terminal properties |

---

### 5.4 Branch Creation DTOs

#### `LineCreationInfos` ← `BranchCreationInfos`

| Field | Type | Description |
|---|---|---|
| `r` | `double` | Series resistance (Ω) |
| `x` | `double` | Series reactance (Ω) |
| `g1` / `b1` | `double` | Shunt conductance / susceptance at terminal 1 (S) |
| `g2` / `b2` | `double` | Shunt conductance / susceptance at terminal 2 (S) |

#### `TwoWindingsTransformerCreationInfos` ← `BranchCreationInfos`

| Field | Type | Description |
|---|---|---|
| `r` | `double` | Series resistance (Ω) |
| `x` | `double` | Series reactance (Ω) |
| `g` / `b` | `double` | Magnetizing conductance / susceptance (S) |
| `ratedU1` / `ratedU2` | `double` | Rated voltages at terminals 1 & 2 (kV) |
| `ratedS` | `Double` | Rated nominal apparent power (MVA) |
| `ratioTapChanger` | `RatioTapChangerCreationInfos` | Optional ratio tap changer configuration |
| `phaseTapChanger` | `PhaseTapChangerCreationInfos` | Optional phase tap changer configuration |

---

### 5.5 Injection Modification DTOs

Injection modification DTOs utilize `AttributeModification<T>` properties for selective, partial updates.

#### `InjectionModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `voltageLevelId` | `AttributeModification<String>` | Target voltage level |
| `busOrBusbarSectionId` | `AttributeModification<String>` | Target bus / busbar section |

#### `LoadModificationInfos` ← `InjectionModificationInfos`

| Field | Type | Description |
|---|---|---|
| `loadType` | `AttributeModification<LoadType>` | Load type |
| `p0` | `AttributeModification<Double>` | Active power consumption (MW) |
| `q0` | `AttributeModification<Double>` | Reactive power consumption (MVar) |

#### `GeneratorModificationInfos` ← `InjectionModificationInfos`

| Field | Type | Description |
|---|---|---|
| `energySource` | `AttributeModification<EnergySource>` | Energy source |
| `minP` / `maxP` | `AttributeModification<Double>` | Active power boundaries (MW) |
| `ratedS` | `AttributeModification<Double>` | Rated apparent power (MVA) |
| `targetP` / `targetQ` / `targetV` | `AttributeModification<Double>` | Operational set points |
| `voltageRegulationOn` | `AttributeModification<Boolean>` | Voltage regulation status |
| `participate` | `AttributeModification<Boolean>` | Frequency regulation participation |
| `droop` | `AttributeModification<Float>` | Droop coefficient |
| `reactiveCapabilityCurvePoints` | `List<ReactiveCapabilityCurvePointsInfos>` | Capability curve points |

#### `BatteryModificationInfos` ← `InjectionModificationInfos`

Mirrors generator modification attributes applicable to battery storage systems.

#### `ShuntCompensatorModificationInfos` ← `InjectionModificationInfos`

| Field | Type | Description |
|---|---|---|
| `sectionCount` | `AttributeModification<Integer>` | Number of active sections |
| `maximumSectionCount` | `AttributeModification<Integer>` | Maximum section capacity |
| `voltageSetpoint` | `AttributeModification<Double>` | Target voltage set point |

---

### 5.6 Branch Modification DTOs

#### `BranchModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `r` / `x` | `AttributeModification<Double>` | Resistance / Reactance |
| `operationalLimitsGroups1` / `2` | `List<OperationalLimitsGroupModificationInfos>` | Limit group modifications at terminals 1 & 2 |
| `connected1` / `connected2` | `AttributeModification<Boolean>` | Terminal connection status |

#### `LineModificationInfos` ← `BranchModificationInfos`

Adds `g1`, `b1`, `g2`, `b2` wrapped in `AttributeModification<Double>`.

#### `TwoWindingsTransformerModificationInfos` ← `BranchModificationInfos`

Adds `g`, `b`, `ratedU1`, `ratedU2`, `ratedS`, `ratioTapChanger`, and `phaseTapChanger` attribute modifications.

---

### 5.7 Substation & Voltage Level DTOs

#### `SubstationCreationInfos` ← `EquipmentCreationInfos`

| Field | Type | Description |
|---|---|---|
| `country` | `Country` | Country code (ISO 3166-1 alpha-2) |
| `voltageLevels` | `List<VoltageLevelCreationInfos>` | Child voltage levels to instantiate within substation |

#### `SubstationModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `country` | `AttributeModification<Country>` | Updated country code |

#### `VoltageLevelCreationInfos` ← `EquipmentCreationInfos`

| Field | Type | Description |
|---|---|---|
| `substationId` | `String` | Parent substation identifier |
| `nominalV` | `double` | Nominal voltage (kV) |
| `lowVoltageLimit` / `highVoltageLimit` | `Double` | Operating voltage limits (kV) |
| `ipMin` / `ipMax` | `Double` | Short-circuit current limits (A) |
| `busbarCount` | `int` | Number of busbars (node-breaker) |
| `sectionCount` | `int` | Section count per busbar |
| `switchKinds` | `List<SwitchKind>` | Switch types between sections |
| `couplingDevices` | `List<CouplingDeviceInfos>` | Initial coupling devices |
| `topologyKind` | `TopologyKind` | `BUS_BREAKER` or `NODE_BREAKER` |

#### `VoltageLevelModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `nominalV` | `AttributeModification<Double>` | Nominal voltage |
| `lowVoltageLimit` / `highVoltageLimit` | `AttributeModification<Double>` | Voltage operating limits |
| `ipMin` / `ipMax` | `AttributeModification<Double>` | Current limits |

---

### 5.8 HVDC DTOs

#### `VscCreationInfos` ← `EquipmentCreationInfos`

| Field | Type | Description |
|---|---|---|
| `dcNominalVoltage` | `double` | DC nominal voltage (kV) |
| `dcResistance` | `double` | DC line resistance (Ω) |
| `nominalV` | `double` | AC nominal voltage (kV) |
| `maxP` | `double` | Maximum active power capacity (MW) |
| `activePowerSetpoint` | `double` | Active power set point (MW) |
| `operatorActivePowerLimitSide1` / `Side2` | `Float` | Operator limits |
| `convertersMode` | `HvdcLine.ConvertersMode` | Converter rectifier/inverter modes |
| `converterStation1` / `converterStation2` | `ConverterStationCreationInfos` | VSC converter station specifications |
| `angleDroopActivePowerControl` | `Boolean` | Enable angle droop control |
| `p0` / `droop` | `Float` | Active power reference and droop slope |

#### `LccCreationInfos` ← `EquipmentCreationInfos`

| Field | Type | Description |
|---|---|---|
| `nominalV` / `dcNominalVoltage` / `dcResistance` | `double` | Electrical parameters |
| `maxP` / `activePowerSetpoint` | `double` | Power limits and set points |
| `convertersMode` | `HvdcLine.ConvertersMode` | Converter mode |
| `converterStation1` / `converterStation2` | `LccConverterStationCreationInfos` | LCC converter stations |

---

### 5.9 Topology Modification DTOs

| DTO Class | Fields | Purpose |
|---|---|---|
| `LineSplitWithVoltageLevelInfos` | `lineToSplitId`, `percent`, `mayNewVoltageLevelInfos`, `existingVoltageLevelId`, `bbsOrBusId`, `newLine1Id`, `newLine2Id`, `newLine1Name`, `newLine2Name` | Split line by inserting a voltage level |
| `LineAttachToVoltageLevelInfos` | `lineToAttachToId`, `percent`, `attachmentPointId`, `attachmentPointName`, `mayNewVoltageLevelInfos`, `existingVoltageLevelId`, `bbsOrBusId`, `attachmentLineId`, `attachmentLineName`, `newLine1Id`, `newLine2Id` | Attach end of line to a voltage level |
| `LinesAttachToSplitLinesInfos` | `lineToAttachTo1Id`, `lineToAttachTo2Id`, `attachedLineId`, `voltageLevelId`, `bbsBusId`, `replacingLine1Id`, `replacingLine1Name`, `replacingLine2Id`, `replacingLine2Name` | Attach lines around a split configuration |
| `DeleteVoltageLevelOnLineInfos` | `lineToAttachTo1Id`, `lineToAttachTo2Id`, `replacingLine1Id`, `replacingLine1Name` | Remove intermediate voltage level from line |
| `DeleteAttachingLineInfos` | `lineToAttachTo1Id`, `lineToAttachTo2Id`, `attachedLineId`, `replacingLine1Id`, `replacingLine1Name` | Delete attaching line |
| `CreateCouplingDeviceInfos` | `voltageLevelId`, `couplingDeviceInfos` | Add coupling breaker between busbars |
| `CreateVoltageLevelTopologyInfos` | `substationId`, `voltageLevelId`, `voltageLevelName`, `nominalV`, `lowVoltageLimit`, `highVoltageLimit`, `busbarCount`, `sectionCount`, `switchKinds` | Create full voltage level topology |
| `CreateVoltageLevelSectionInfos` | `voltageLevelId`, `switchKinds` | Add a section to an existing voltage level |
| `MoveVoltageLevelFeederBaysInfos` | `voltageLevelId`, `feederBaysMoves` | Reallocate bays across busbar sections |
| `VoltageLevelTopologyModificationInfos` | `busbarSectionToSwitchesAttributes` | Update switch topology configuration |

---

### 5.10 Deletion DTOs

#### `EquipmentDeletionInfos` ← `EquipmentModificationInfos`

Deletes a single piece of equipment identified by `equipmentId` and `equipmentType`.

#### `ByFilterDeletionInfos` ← `ModificationInfos`

Deletes all equipment matching filter criteria.

| Field | Type | Description |
|---|---|---|
| `equipmentType` | `IdentifiableType` | Target equipment type |
| `filters` | `List<FilterInfos>` | Filter identifiers used to select equipment |

---

### 5.11 Scaling & Dispatch DTOs

#### `ScalingInfos` ← `ModificationInfos`

Abstract base for power scaling.

| Field | Type | Description |
|---|---|---|
| `variations` | `List<ScalingVariationInfos>` | List of scaling variations |
| `variationType` | `VariationType` | `DELTA_P` (relative delta) or `TARGET_P` (absolute value) |

- `GeneratorScalingInfos` ← `ScalingInfos` (scales active power across selected generators).
- `LoadScalingInfos` ← `ScalingInfos` (scales active power across selected loads).

#### `ScalingVariationInfos`

| Field | Type | Description |
|---|---|---|
| `filters` | `List<FilterInfos>` | Selection filter definitions |
| `variationMode` | `VariationMode` | `PROPORTIONAL_TO_PMAX`, `PROPORTIONAL_TO_P`, `REGULAR_DISTRIBUTION`, `STACKING_UP`, `VENTILATION` |
| `variationValue` | `double` | Target power variation value (MW) |
| `reactiveVariationMode` | `ReactiveVariationMode` | `CONSTANT_Q` or `TAN_PHI_FIXED` |

#### `GenerationDispatchInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `lossCoefficient` | `double` | Loss percentage factor |
| `defaultOutageRate` | `double` | Generator outage rate default |
| `generatorsWithoutOutage` | `List<GeneratorsWithoutOutageInfos>` | Exempted generators |
| `generatorsWithFixedSupply` | `List<GeneratorsFilterInfos>` | Fixed supply generators |
| `generatorsFrequencyReserve` | `List<GeneratorsFrequencyReserveInfos>` | Frequency reserve allocation |
| `substationsGeneratorsOrdering` | `List<SubstationsGeneratorsOrderingInfos>` | Dispatch ordering preferences |
| `loadFlowParametersUuid` | `UUID` | Load-flow settings reference |

---

### 5.12 Bulk & Programmatic Modification DTOs

| DTO Class | Fields | Purpose |
|---|---|---|
| `EquipmentAttributeModificationInfos` | `equipmentAttributeName`, `equipmentAttributeValue`, `equipmentType` | Modify a specific attribute by name |
| `GroovyScriptInfos` | `script` | Execute dynamic Groovy script on `network` |
| `TabularModificationInfos` | `modificationType`, `modifications` | Batch modify multiple equipment from a table |
| `TabularCreationInfos` | `creationType`, `creations` | Batch create multiple equipment from a table |
| `LimitSetsTabularModificationInfos` | Inherits `TabularModificationInfos` | Bulk edit operational limit sets |
| `ByFormulaModificationInfos` | `identifiableType`, `formulaInfosList` | Calculate attribute values via mathematical expressions |
| `ModificationByAssignmentInfos` | `identifiableType`, `assignmentInfosList` | Assign values based on filter conditions |

---

### 5.13 Operational Modification DTOs

#### `OperatingStatusModificationInfos` ← `EquipmentModificationInfos`

| Field | Type | Description |
|---|---|---|
| `energizedVoltageLevelId` | `String` | Voltage level providing energization |
| `action` | `ActionType` | `LOCKOUT`, `TRIP`, `SWITCH_ON`, `ENERGISE_END_ONE`, `ENERGISE_END_TWO` |

#### `BalancesAdjustmentModificationInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `areas` | `List<BalancesAdjustmentAreaInfos>` | Area exchange targets |
| `loadFlowParametersUuid` | `UUID` | Load-flow settings reference |

#### `VoltageInitModificationInfos` ← `ModificationInfos`

Contains collections of voltage initialization targets for `generators`, `transformers`, `staticVarCompensators`, `vscConverterStations`, `shuntCompensators`, and `buses`.

---

### 5.14 Composition & Reference DTOs

#### `CompositeModificationInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `name` | `String` | Descriptive scenario/composite name |
| `modificationsInfos` | `List<ModificationInfos>` | Ordered list of child modifications |
| `maxDepth` | `Integer` | Computed maximum nesting depth |

#### `ModificationReferenceInfos` ← `ModificationInfos`

| Field | Type | Description |
|---|---|---|
| `referenceId` | `UUID` | UUID of the referenced modification |
| `referenceType` | `Type` | `BASIC` or `DIRECTORY` |
| `referenceInfos` | `ModificationInfos` | Resolved DTO representation |

---

## 6. Supporting Value Objects

### `AttributeModification<T>`

Generic container for partial update operations:

```java
// Explicit update
AttributeModification<Double> setP = new AttributeModification<>(250.0, OperationType.SET);

// Reset to default
AttributeModification<Double> unsetP = new AttributeModification<>(null, OperationType.UNSET);

// Helper factory
AttributeModification<Double> modP = AttributeModification.toAttributeModification(250.0, OperationType.SET);
```

### `FilterInfos`

Carries `UUID id` and `String name` for referencing filter rules.

### `CurrentLimitsInfos`

Carries `Double permanentLimit` and `List<CurrentTemporaryLimitCreationInfos> temporaryLimits`.

### `FreePropertyInfos`

Carries `String name`, `String value`, and `boolean deletionMark` for arbitrary equipment metadata tags.

---

## 7. Exceptions & Error Types

**Package:** `org.gridsuite.modification.error`

`NetworkModificationException` extends `com.powsybl.commons.PowsyblException` and encapsulates domain-level errors encountered during check or apply phases.

### Error Types (`NetworkModificationExceptionType`)

| Type | Default Message |
|---|---|
| `GROOVY_SCRIPT_EMPTY` | The groovy script is empty |
| `LINE_NOT_FOUND` | The line could not be found |
| `LOAD_NOT_FOUND` | The load could not be found |
| `BATTERY_NOT_FOUND` | The battery could not be found |
| `GENERATOR_NOT_FOUND` | The generator could not be found |
| `TWO_WINDINGS_TRANSFORMER_NOT_FOUND` | The two windings transformer could not be found |
| `UNKNOWN_EQUIPMENT_TYPE` | The equipment type is unknown |
| `WRONG_EQUIPMENT_TYPE` | The equipment type does not match the expected type |
| `MODIFICATION_ERROR` | An error occurred while applying the modification |
| `VOLTAGE_LEVEL_NOT_FOUND` | The voltage level could not be found |
| `BUSBAR_SECTION_NOT_FOUND` | The busbar section could not be found |
| `BUS_NOT_FOUND` | The bus could not be found |
| `CREATE_BATTERY_ERROR` | An error occurred while creating the battery |
| `CREATE_GENERATOR_ERROR` | An error occurred while creating the generator |
| `CREATE_SHUNT_COMPENSATOR_ERROR` | An error occurred while creating the shunt compensator |
| `MODIFY_SHUNT_COMPENSATOR_ERROR` | An error occurred while modifying the shunt compensator |
| `CREATE_STATIC_VAR_COMPENSATOR_ERROR` | An error occurred while creating the static var compensator |
| `EQUIPMENT_NOT_FOUND` | The equipment could not be found |
| `ATTRIBUTE_NOT_EDITABLE` | The equipment attribute is not editable |
| `CREATE_LINE_ERROR` | An error occurred while creating the line |
| `MODIFY_LINE_ERROR` | An error occurred while modifying the line |
| `CREATE_TWO_WINDINGS_TRANSFORMER_ERROR` | An error occurred while creating the two windings transformer |
| `MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR` | An error occurred while modifying the two windings transformer |
| `CREATE_VOLTAGE_LEVEL_ERROR` | An error occurred while creating the voltage level |
| `MODIFY_VOLTAGE_LEVEL_ERROR` | An error occurred while modifying the voltage level |
| `SUBSTATION_NOT_FOUND` | The substation could not be found |
| `BATTERY_ALREADY_EXISTS` | A battery with this identifier already exists |
| `LOAD_ALREADY_EXISTS` | A load with this identifier already exists |
| `VOLTAGE_LEVEL_ALREADY_EXISTS` | A voltage level with this identifier already exists |
| `GENERATOR_ALREADY_EXISTS` | A generator with this identifier already exists |
| `SHUNT_COMPENSATOR_ALREADY_EXISTS` | A shunt compensator with this identifier already exists |
| `SHUNT_COMPENSATOR_NOT_FOUND` | The shunt compensator could not be found |
| `STATIC_VAR_COMPENSATOR_ALREADY_EXISTS` | A static var compensator with this identifier already exists |
| `STATIC_VAR_COMPENSATOR_NOT_FOUND` | The static var compensator could not be found |
| `LINE_ALREADY_EXISTS` | A line with this identifier already exists |
| `TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS` | A two windings transformer with this identifier already exists |
| `TWO_WINDINGS_TRANSFORMER_CREATION_ERROR` | An error occurred while creating the two windings transformer |
| `BRANCH_MODIFICATION_ERROR` | An error occurred while modifying the branch |
| `INJECTION_MODIFICATION_ERROR` | An error occurred while modifying the injection |
| `MODIFY_BATTERY_ERROR` | An error occurred while modifying the battery |
| `OPERATING_STATUS_MODIFICATION_ERROR` | An error occurred while modifying the operating status |
| `OPERATING_ACTION_TYPE_EMPTY` | The operating action type is empty |
| `OPERATING_ACTION_TYPE_UNSUPPORTED` | The operating action type is not supported |
| `EQUIPMENT_TYPE_UNSUPPORTED` | The equipment type is not supported |
| `MODIFY_GENERATOR_ERROR` | An error occurred while modifying the generator |
| `EQUIPMENT_ATTRIBUTE_NAME_ERROR` | The equipment attribute name is invalid |
| `EQUIPMENT_ATTRIBUTE_VALUE_ERROR` | The equipment attribute value is invalid |
| `GENERATOR_SCALING_ERROR` | An error occurred while scaling the generators |
| `LOAD_SCALING_ERROR` | An error occurred while scaling the loads |
| `GENERATION_DISPATCH_ERROR` | An error occurred while dispatching the generation |
| `VOLTAGE_INIT_MODIFICATION_ERROR` | An error occurred while applying the voltage init modification |
| `TABULAR_MODIFICATION_ERROR` | An error occurred while applying the tabular modification |
| `TABULAR_CREATION_ERROR` | An error occurred while applying the tabular creation |
| `CREATE_VSC_ERROR` | An error occurred while creating the VSC converter station |
| `MODIFY_VSC_ERROR` | An error occurred while modifying the VSC converter station |
| `CREATE_LCC_ERROR` | An error occurred while creating the LCC converter station |
| `MODIFY_LCC_ERROR` | An error occurred while modifying the LCC converter station |
| `HVDC_LINE_ALREADY_EXISTS` | An HVDC line with this identifier already exists |
| `VSC_CONVERTER_STATION_NOT_FOUND` | The VSC converter station could not be found |
| `LCC_CONVERTER_STATION_NOT_FOUND` | The LCC converter station could not be found |
| `BY_FORMULA_MODIFICATION_ERROR` | An error occurred while applying the modification by formula |
| `MODIFICATION_BY_ASSIGNMENT_ERROR` | An error occurred while applying the modification by assignment |
| `HVDC_LINE_NOT_FOUND` | The HVDC line could not be found |
| `WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL` | The HVDC angle droop active power control configuration is invalid |
| `UNSUPPORTED_HYBRID_HVDC` | The hybrid HVDC line is not supported |
| `MODIFY_VOLTAGE_LEVEL_TOPOLOGY_ERROR` | An error occurred while modifying the voltage level topology |
| `CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR` | An error occurred while creating the voltage level topology |
| `MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR` | An error occurred while moving the voltage level feeder bays |

---

## 8. Enumerations Reference

### `OperationType`

`SET` (apply new value), `UNSET` (reset to default/null).

### `VariationType`

`DELTA_P` (relative change), `TARGET_P` (absolute value).

### `VariationMode`

`PROPORTIONAL_TO_PMAX`, `PROPORTIONAL_TO_P`, `REGULAR_DISTRIBUTION`, `STACKING_UP`, `VENTILATION`.

### `ReactiveVariationMode`

`CONSTANT_Q` (keep Q constant), `TAN_PHI_FIXED` (keep power factor fixed).

### `TapChangerType`

`RATIO`, `PHASE`.

### `ShuntCompensatorType`

`CAPACITOR`, `REACTOR`.

### `VoltageRegulationType`

`LOCAL`, `DISTANT`.

### `RegulationSide`

`SIDE_1`, `SIDE_2`.

### `OperatingStatusModificationInfos.ActionType`

`LOCKOUT`, `TRIP`, `SWITCH_ON`, `ENERGISE_END_ONE`, `ENERGISE_END_TWO`.

---

## 9. Usage Examples

### Example 1 — Create a Load

```java
LoadCreationInfos loadInfos = LoadCreationInfos.builder()
        .equipmentId("LOAD_1")
        .equipmentName("Industrial Load 1")
        .voltageLevelId("VL_NORTH_400")
        .busOrBusbarSectionId("BUS_1")
        .loadType(LoadType.UNDEFINED)
        .p0(120.0)
        .q0(30.0)
        .build();

loadInfos.check();
AbstractModification modification = loadInfos.toModification();
modification.check(network);

ReportNode reportNode = ReportNode.newRootReportNode()
        .withMessageTemplate("root", "Root Report")
        .build();

modification.apply(network, reportNode);
```

### Example 2 — Partial Generator Modification

```java
GeneratorModificationInfos genModif = GeneratorModificationInfos.builder()
        .equipmentId("GEN_HYDRO_1")
        .targetP(AttributeModification.toAttributeModification(280.0, OperationType.SET))
        .voltageRegulationOn(AttributeModification.toAttributeModification(true, OperationType.SET))
        .build();

genModif.toModification().apply(network, reportNode);
```

### Example 3 — Filter-Based Equipment Deletion

```java
ByFilterDeletionInfos deletion = ByFilterDeletionInfos.builder()
        .equipmentType(IdentifiableType.LOAD)
        .filters(List.of(new FilterInfos(filterUuid, "Decommissioned Loads")))
        .build();

AbstractModification mod = deletion.toModification();
mod.initApplicationContext(filterService, null);
mod.check(network);
mod.apply(network, reportNode);
```

### Example 4 — Composite Scenario Execution

```java
CompositeModificationInfos scenario = CompositeModificationInfos.builder()
        .name("Peak Load Scenario")
        .modificationsInfos(List.of(loadInfos, genModif))
        .build();

AbstractModification compositeMod = scenario.toModification();
compositeMod.initApplicationContext(filterService, loadFlowService);
compositeMod.check(network);
compositeMod.apply(network, reportNode);
```

### Example 5 — Polymorphic JSON Deserialization

```json
[
  {
    "type": "LOAD_CREATION",
    "equipmentId": "LOAD_NEW",
    "voltageLevelId": "VL1",
    "busOrBusbarSectionId": "BUS1",
    "p0": 45.0,
    "q0": 12.0
  },
  {
    "type": "GENERATOR_MODIFICATION",
    "equipmentId": "GEN1",
    "targetP": {
      "value": 310.0,
      "op": "SET"
    }
  }
]
```

```java
ObjectMapper mapper = new ObjectMapper();
List<ModificationInfos> modifications = mapper.readValue(
        jsonString,
        mapper.getTypeFactory().constructCollectionType(List.class, ModificationInfos.class)
);
```

---

*Documentation for `gridsuite-network-modification` — © RTE (http://www.rte-france.com) — MPL-2.0*
