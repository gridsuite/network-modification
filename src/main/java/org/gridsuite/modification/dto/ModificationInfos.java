package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException.Type;
import org.gridsuite.modification.dto.tabular.LimitSetsTabularModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularCreationInfos;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type", include = JsonTypeInfo.As.EXISTING_PROPERTY, visible = true)
@JsonSubTypes({
    @JsonSubTypes.Type(value = EquipmentAttributeModificationInfos.class, name = "EQUIPMENT_ATTRIBUTE_MODIFICATION"),
    @JsonSubTypes.Type(value = LoadCreationInfos.class, name = "LOAD_CREATION"),
    @JsonSubTypes.Type(value = LoadModificationInfos.class, name = "LOAD_MODIFICATION"),
    @JsonSubTypes.Type(value = BalancesAdjustmentModificationInfos.class, name = "BALANCES_ADJUSTMENT_MODIFICATION"),
    @JsonSubTypes.Type(value = BatteryCreationInfos.class, name = "BATTERY_CREATION"),
    @JsonSubTypes.Type(value = BatteryModificationInfos.class, name = "BATTERY_MODIFICATION"),
    @JsonSubTypes.Type(value = GeneratorCreationInfos.class, name = "GENERATOR_CREATION"),
    @JsonSubTypes.Type(value = GeneratorModificationInfos.class, name = "GENERATOR_MODIFICATION"),
    @JsonSubTypes.Type(value = EquipmentDeletionInfos.class, name = "EQUIPMENT_DELETION"),
    @JsonSubTypes.Type(value = ByFilterDeletionInfos.class, name = "BY_FILTER_DELETION"),
    @JsonSubTypes.Type(value = LineCreationInfos.class, name = "LINE_CREATION"),
    @JsonSubTypes.Type(value = LineModificationInfos.class, name = "LINE_MODIFICATION"),
    @JsonSubTypes.Type(value = TwoWindingsTransformerCreationInfos.class, name = "TWO_WINDINGS_TRANSFORMER_CREATION"),
    @JsonSubTypes.Type(value = TwoWindingsTransformerModificationInfos.class, name = "TWO_WINDINGS_TRANSFORMER_MODIFICATION"),
    @JsonSubTypes.Type(value = GroovyScriptInfos.class, name = "GROOVY_SCRIPT"),
    @JsonSubTypes.Type(value = SubstationCreationInfos.class, name = "SUBSTATION_CREATION"),
    @JsonSubTypes.Type(value = SubstationModificationInfos.class, name = "SUBSTATION_MODIFICATION"),
    @JsonSubTypes.Type(value = ShuntCompensatorCreationInfos.class, name = "SHUNT_COMPENSATOR_CREATION"),
    @JsonSubTypes.Type(value = ShuntCompensatorModificationInfos.class, name = "SHUNT_COMPENSATOR_MODIFICATION"),
    @JsonSubTypes.Type(value = StaticVarCompensatorCreationInfos.class, name = "STATIC_VAR_COMPENSATOR_CREATION"),
    @JsonSubTypes.Type(value = VoltageLevelCreationInfos.class, name = "VOLTAGE_LEVEL_CREATION"),
    @JsonSubTypes.Type(value = VoltageLevelModificationInfos.class, name = "VOLTAGE_LEVEL_MODIFICATION"),
    @JsonSubTypes.Type(value = LineSplitWithVoltageLevelInfos.class, name = "LINE_SPLIT_WITH_VOLTAGE_LEVEL"),
    @JsonSubTypes.Type(value = LineAttachToVoltageLevelInfos.class, name = "LINE_ATTACH_TO_VOLTAGE_LEVEL"),
    @JsonSubTypes.Type(value = LinesAttachToSplitLinesInfos.class, name = "LINES_ATTACH_TO_SPLIT_LINES"),
    @JsonSubTypes.Type(value = GeneratorScalingInfos.class, name = "GENERATOR_SCALING"),
    @JsonSubTypes.Type(value = LoadScalingInfos.class, name = "LOAD_SCALING"),
    @JsonSubTypes.Type(value = OperatingStatusModificationInfos.class, name = "OPERATING_STATUS_MODIFICATION"),
    @JsonSubTypes.Type(value = DeleteVoltageLevelOnLineInfos.class, name = "DELETE_VOLTAGE_LEVEL_ON_LINE"),
    @JsonSubTypes.Type(value = DeleteAttachingLineInfos.class, name = "DELETE_ATTACHING_LINE"),
    @JsonSubTypes.Type(value = GenerationDispatchInfos.class, name = "GENERATION_DISPATCH"),
    @JsonSubTypes.Type(value = VoltageInitModificationInfos.class, name = "VOLTAGE_INIT_MODIFICATION"),
    @JsonSubTypes.Type(value = VscCreationInfos.class, name = "VSC_CREATION"),
    @JsonSubTypes.Type(value = VscModificationInfos.class, name = "VSC_MODIFICATION"),
    @JsonSubTypes.Type(value = ConverterStationCreationInfos.class, name = "CONVERTER_STATION_CREATION"),
    @JsonSubTypes.Type(value = ConverterStationModificationInfos.class, name = "CONVERTER_STATION_MODIFICATION"),
    @JsonSubTypes.Type(value = TabularModificationInfos.class, name = "TABULAR_MODIFICATION"),
    @JsonSubTypes.Type(value = TabularCreationInfos.class, name = "TABULAR_CREATION"),
    @JsonSubTypes.Type(value = ByFormulaModificationInfos.class, name = "BY_FORMULA_MODIFICATION"),
    @JsonSubTypes.Type(value = ModificationByAssignmentInfos.class, name = "MODIFICATION_BY_ASSIGNMENT"),
    @JsonSubTypes.Type(value = CompositeModificationInfos.class, name = "COMPOSITE_MODIFICATION"),
    @JsonSubTypes.Type(value = LccConverterStationCreationInfos.class, name = "LCC_CONVERTER_STATION_CREATION"),
    @JsonSubTypes.Type(value = LccConverterStationModificationInfos.class, name = "LCC_CONVERTER_STATION_MODIFICATION"),
    @JsonSubTypes.Type(value = LccCreationInfos.class, name = "LCC_CREATION"),
    @JsonSubTypes.Type(value = LccModificationInfos.class, name = "LCC_MODIFICATION"),
    @JsonSubTypes.Type(value = VoltageLevelTopologyModificationInfos.class, name = "VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION"),
    @JsonSubTypes.Type(value = CreateCouplingDeviceInfos.class, name = "CREATE_COUPLING_DEVICE"),
    @JsonSubTypes.Type(value = CreateVoltageLevelTopologyInfos.class, name = "CREATE_VOLTAGE_LEVEL_TOPOLOGY"),
    @JsonSubTypes.Type(value = LimitSetsTabularModificationInfos.class, name = "LIMIT_SETS_TABULAR_MODIFICATION"),
    @JsonSubTypes.Type(value = CreateVoltageLevelSectionInfos.class, name = "CREATE_VOLTAGE_LEVEL_SECTION"),
    @JsonSubTypes.Type(value = MoveVoltageLevelFeederBaysInfos.class, name = "MOVE_VOLTAGE_LEVEL_FEEDER_BAYS")
})
public interface ModificationInfos {

    UUID getUuid();

    void setUuid(UUID uuid);

    Instant getDate();

    Boolean getActivated();

    void setActivated(Boolean activated);

    Boolean getStashed();

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    ModificationType getType();

    String getDescription();

    @JsonIgnore
    Map<String, String> getMapMessageValues();

    default String getMessageType() {
        return getType().name();
    }

    default String getMessageValues() {
        return getMapMessageValues().toString();
    }

    @JsonIgnore
    Type getErrorType();
}
