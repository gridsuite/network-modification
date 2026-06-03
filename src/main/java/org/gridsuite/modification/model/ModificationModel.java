/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.model.tabular.LimitSetsTabularModificationModel;
import org.gridsuite.modification.model.tabular.TabularCreationModel;
import org.gridsuite.modification.model.tabular.TabularModificationModel;
import org.gridsuite.modification.modifications.AbstractModification;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "type",
    include = JsonTypeInfo.As.EXISTING_PROPERTY
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = GroovyScriptModel.class),
    @JsonSubTypes.Type(value = BatteryCreationModel.class),
    @JsonSubTypes.Type(value = BatteryModificationModel.class),
    @JsonSubTypes.Type(value = LoadCreationModel.class),
    @JsonSubTypes.Type(value = LoadModificationModel.class),
    @JsonSubTypes.Type(value = GeneratorCreationModel.class),
    @JsonSubTypes.Type(value = GeneratorModificationModel.class),
    @JsonSubTypes.Type(value = LineCreationModel.class),
    @JsonSubTypes.Type(value = LineModificationModel.class),
    @JsonSubTypes.Type(value = SubstationCreationModel.class),
    @JsonSubTypes.Type(value = SubstationModificationModel.class),
    @JsonSubTypes.Type(value = VoltageLevelCreationModel.class),
    @JsonSubTypes.Type(value = VoltageLevelModificationModel.class),
    @JsonSubTypes.Type(value = ShuntCompensatorCreationModel.class),
    @JsonSubTypes.Type(value = ShuntCompensatorModificationModel.class),
    @JsonSubTypes.Type(value = StaticVarCompensatorCreationModel.class),
    @JsonSubTypes.Type(value = TwoWindingsTransformerCreationModel.class),
    @JsonSubTypes.Type(value = TwoWindingsTransformerModificationModel.class),
    @JsonSubTypes.Type(value = EquipmentDeletionModel.class),
    @JsonSubTypes.Type(value = ByFilterDeletionModel.class),
    @JsonSubTypes.Type(value = LineSplitWithVoltageLevelModel.class),
    @JsonSubTypes.Type(value = LineAttachToVoltageLevelModel.class),
    @JsonSubTypes.Type(value = LinesAttachToSplitLinesModel.class),
    @JsonSubTypes.Type(value = OperatingStatusModificationModel.class),
    @JsonSubTypes.Type(value = EquipmentAttributeModificationModel.class),
    @JsonSubTypes.Type(value = GeneratorScalingModel.class),
    @JsonSubTypes.Type(value = LoadScalingModel.class),
    @JsonSubTypes.Type(value = DeleteVoltageLevelOnLineModel.class),
    @JsonSubTypes.Type(value = DeleteAttachingLineModel.class),
    @JsonSubTypes.Type(value = GenerationDispatchModel.class),
    @JsonSubTypes.Type(value = VoltageInitModificationModel.class),
    @JsonSubTypes.Type(value = VscCreationModel.class),
    @JsonSubTypes.Type(value = LccCreationModel.class),
    @JsonSubTypes.Type(value = LccConverterStationCreationModel.class),
    @JsonSubTypes.Type(value = ConverterStationCreationModel.class),
    @JsonSubTypes.Type(value = TabularModificationModel.class),
    @JsonSubTypes.Type(value = ByFormulaModificationModel.class),
    @JsonSubTypes.Type(value = ModificationByAssignmentModel.class),
    @JsonSubTypes.Type(value = VscModificationModel.class),
    @JsonSubTypes.Type(value = ConverterStationModificationModel.class),
    @JsonSubTypes.Type(value = TabularCreationModel.class),
    @JsonSubTypes.Type(value = CompositeModificationModel.class),
    @JsonSubTypes.Type(value = LccModificationModel.class),
    @JsonSubTypes.Type(value = LccConverterStationModificationModel.class),
    @JsonSubTypes.Type(value = VoltageLevelTopologyModificationModel.class),
    @JsonSubTypes.Type(value = CreateCouplingDeviceModel.class),
    @JsonSubTypes.Type(value = BalancesAdjustmentModificationModel.class),
    @JsonSubTypes.Type(value = CreateVoltageLevelTopologyModel.class),
    @JsonSubTypes.Type(value = LimitSetsTabularModificationModel.class),
    @JsonSubTypes.Type(value = CreateVoltageLevelSectionModel.class),
    @JsonSubTypes.Type(value = MoveVoltageLevelFeederBaysModel.class),
})
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString
@Schema(description = "Modification attributes")
public class ModificationModel {
    @Schema(description = "Modification id")
    private UUID uuid;

    @Schema(description = "Modification type")
    @Setter(AccessLevel.NONE)
    private final AtomicReference<ModificationType> type = new AtomicReference<>(null); // Only accessor (automatically initialized)

    @Schema(description = "Modification date")
    private Instant date;

    @Schema(description = "Modification flag")
    @Builder.Default
    private Boolean stashed = false;

    @Schema(description = "Message type")
    private String messageType;

    @Schema(description = "Message values")
    private String messageValues;

    @Schema(description = "Modification activated (defaults to true at creation when not provided)")
    private Boolean activated;

    @Schema(description = "User description")
    private String description;

    @JsonIgnore
    public ReportNode createSubReportNode(ReportNode reportNode) {
        throw new UnsupportedOperationException("Method createSubReportNode must be implemented in subclass " + this.getClass().getSimpleName());
    }

    @JsonIgnore
    public AbstractModification toModification() {
        throw new UnsupportedOperationException("Method toModification must be implemented in subclass " + this.getClass().getSimpleName());
    }

    @JsonIgnore
    public final NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.valueOf(this.getClass().getAnnotation(ModificationErrorTypeName.class).value());
    }

    public final ModificationType getType() {
        return type.get() != null ? type.get() : ModificationType.valueOf(this.getClass().getAnnotation(JsonTypeName.class).value());
    }

    public void setType(ModificationType type) {
        this.type.set(type);
    }

    @JsonIgnore
    public Map<String, String> getMapMessageValues() {
        return Map.of();
    }

    @JsonIgnore
    public void check() {
        // To check input DTO before hypothesis creation. Nothing to check here
    }
}
