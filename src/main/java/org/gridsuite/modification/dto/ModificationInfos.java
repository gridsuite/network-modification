/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.tabular.LimitSetsTabularModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularCreationInfos;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.model.ModificationModel;

import java.time.Instant;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "type",
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    visible = true,
    defaultImpl = ModificationMetadataInfos.class
)
@JsonSubTypes({
    @JsonSubTypes.Type(SubstationCreationInfos.class),
    @JsonSubTypes.Type(VoltageLevelCreationInfos.class),
    @JsonSubTypes.Type(LineCreationInfos.class),
    @JsonSubTypes.Type(TwoWindingsTransformerCreationInfos.class),
    @JsonSubTypes.Type(GeneratorCreationInfos.class),
    @JsonSubTypes.Type(LoadCreationInfos.class),
    @JsonSubTypes.Type(BatteryCreationInfos.class),
    @JsonSubTypes.Type(ShuntCompensatorCreationInfos.class),
    @JsonSubTypes.Type(StaticVarCompensatorCreationInfos.class),
    @JsonSubTypes.Type(VscCreationInfos.class),
    @JsonSubTypes.Type(ConverterStationCreationInfos.class),
    @JsonSubTypes.Type(LccCreationInfos.class),
    @JsonSubTypes.Type(LccConverterStationCreationInfos.class),
    @JsonSubTypes.Type(CreateVoltageLevelSectionInfos.class),
    @JsonSubTypes.Type(SubstationModificationInfos.class),
    @JsonSubTypes.Type(VoltageLevelModificationInfos.class),
    @JsonSubTypes.Type(LineModificationInfos.class),
    @JsonSubTypes.Type(TwoWindingsTransformerModificationInfos.class),
    @JsonSubTypes.Type(GeneratorModificationInfos.class),
    @JsonSubTypes.Type(LoadModificationInfos.class),
    @JsonSubTypes.Type(BatteryModificationInfos.class),
    @JsonSubTypes.Type(ShuntCompensatorModificationInfos.class),
    @JsonSubTypes.Type(VscModificationInfos.class),
    @JsonSubTypes.Type(ConverterStationModificationInfos.class),
    @JsonSubTypes.Type(ByFormulaModificationInfos.class),
    @JsonSubTypes.Type(ModificationByAssignmentInfos.class),
    @JsonSubTypes.Type(EquipmentAttributeModificationInfos.class),
    @JsonSubTypes.Type(LccModificationInfos.class),
    @JsonSubTypes.Type(LccConverterStationModificationInfos.class),
    @JsonSubTypes.Type(VoltageLevelTopologyModificationInfos.class),
    @JsonSubTypes.Type(CreateCouplingDeviceInfos.class),
    @JsonSubTypes.Type(CreateVoltageLevelTopologyInfos.class),
    @JsonSubTypes.Type(MoveVoltageLevelFeederBaysInfos.class),
    @JsonSubTypes.Type(TabularCreationInfos.class),
    @JsonSubTypes.Type(TabularModificationInfos.class),
    @JsonSubTypes.Type(LimitSetsTabularModificationInfos.class),
    @JsonSubTypes.Type(LineAttachToVoltageLevelInfos.class),
    @JsonSubTypes.Type(LineSplitWithVoltageLevelInfos.class),
    @JsonSubTypes.Type(LinesAttachToSplitLinesInfos.class),
    @JsonSubTypes.Type(DeleteAttachingLineInfos.class),
    @JsonSubTypes.Type(DeleteVoltageLevelOnLineInfos.class),
    @JsonSubTypes.Type(GenerationDispatchInfos.class),
    @JsonSubTypes.Type(LoadScalingInfos.class),
    @JsonSubTypes.Type(GeneratorScalingInfos.class),
    @JsonSubTypes.Type(OperatingStatusModificationInfos.class),
    @JsonSubTypes.Type(CompositeModificationInfos.class),
    @JsonSubTypes.Type(VoltageInitModificationInfos.class),
    @JsonSubTypes.Type(GroovyScriptInfos.class),
    @JsonSubTypes.Type(BalancesAdjustmentModificationInfos.class),
    @JsonSubTypes.Type(ByFilterDeletionInfos.class),
    @JsonSubTypes.Type(EquipmentDeletionInfos.class)
})
public interface ModificationInfos {

    ModificationType getType();

    default void setType(ModificationType type) {
        if (type != getType()) {
            throw new IllegalArgumentException("Unexpected modification type: " + type);
        }
    }

    UUID getUuid();

    void setUuid(UUID uuid);

    Instant getDate();

    void setDate(Instant date);

    Boolean getStashed();

    void setStashed(Boolean stashed);

    @JsonIgnore(false)
    String getMessageType();

    void setMessageType(String messageType);

    @JsonIgnore(false)
    String getMessageValues();

    void setMessageValues(String messageValues);

    Boolean getActivated();

    void setActivated(Boolean activated);

    String getDescription();

    void setDescription(String description);

    default ModificationModel toModel() {
        return (ModificationModel) this;
    }
}
