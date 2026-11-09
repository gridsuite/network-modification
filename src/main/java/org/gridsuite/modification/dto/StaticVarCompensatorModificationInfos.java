/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.StaticVarCompensator;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.StaticVarCompensatorModification;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Static var compensator modification")
@JsonTypeName("STATIC_VAR_COMPENSATOR_MODIFICATION")
public class StaticVarCompensatorModificationInfos extends InjectionModificationInfos {
    @Schema(description = "Susceptance max")
    private AttributeModification<Double> maxSusceptance;

    @Schema(description = "Susceptance min")
    private AttributeModification<Double> minSusceptance;

    @Schema(description = "Q max at nominal voltage")
    private AttributeModification<Double> maxQAtNominalV;

    @Schema(description = "Q min at nominal voltage")
    private AttributeModification<Double> minQAtNominalV;

    @Schema(description = "Regulation mode")
    private AttributeModification<StaticVarCompensator.RegulationMode> regulationMode;

    @Schema(description = "Regulating")
    private AttributeModification<Boolean> regulating;

    @Schema(description = "Voltage set point")
    private AttributeModification<Double> voltageSetpoint;

    @Schema(description = "Reactive power set point")
    private AttributeModification<Double> reactivePowerSetpoint;

    @Schema(description = "Voltage Regulation type")
    private AttributeModification<VoltageRegulationType> voltageRegulationType;

    @Schema(description = "Regulating terminal equipment id")
    private AttributeModification<String> regulatingTerminalId;

    @Schema(description = "Regulating terminal equipment type")
    private AttributeModification<String> regulatingTerminalType;

    @Schema(description = "Regulating terminal voltage level id")
    private AttributeModification<String> regulatingTerminalVlId;

    @Schema(description = "Standby automaton on: true to add or keep the automaton, false to remove it")
    private AttributeModification<Boolean> standbyAutomatonOn;

    @Schema(description = "Standby")
    private AttributeModification<Boolean> standby;

    @Schema(description = "Fixed part of susceptance")
    private AttributeModification<Double> b0;

    @Schema(description = "Fixed part of Q at nominal voltage")
    private AttributeModification<Double> q0;

    @Schema(description = "Low voltage set point")
    private AttributeModification<Double> lowVoltageSetpoint;

    @Schema(description = "High voltage set point")
    private AttributeModification<Double> highVoltageSetpoint;

    @Schema(description = "Low voltage threshold")
    private AttributeModification<Double> lowVoltageThreshold;

    @Schema(description = "High voltage threshold")
    private AttributeModification<Double> highVoltageThreshold;

    @Override
    public AbstractModification toModification() {
        return StaticVarCompensatorModification.builder()
                .equipmentId(getEquipmentId())
                .properties(getProperties())
                .equipmentName(getEquipmentName())
                .voltageLevelId(getVoltageLevelId())
                .busOrBusbarSectionId(getBusOrBusbarSectionId())
                .connectionName(getConnectionName())
                .connectionDirection(getConnectionDirection())
                .connectionPosition(getConnectionPosition())
                .terminalConnected(getTerminalConnected())
                .pMeasurementValue(getPMeasurementValue())
                .pMeasurementValidity(getPMeasurementValidity())
                .qMeasurementValue(getQMeasurementValue())
                .qMeasurementValidity(getQMeasurementValidity())
                .maxSusceptance(getMaxSusceptance())
                .minSusceptance(getMinSusceptance())
                .maxQAtNominalV(getMaxQAtNominalV())
                .minQAtNominalV(getMinQAtNominalV())
                .regulationMode(getRegulationMode())
                .regulating(getRegulating())
                .voltageSetpoint(getVoltageSetpoint())
                .reactivePowerSetpoint(getReactivePowerSetpoint())
                .voltageRegulationType(getVoltageRegulationType())
                .regulatingTerminalId(getRegulatingTerminalId())
                .regulatingTerminalType(getRegulatingTerminalType())
                .regulatingTerminalVlId(getRegulatingTerminalVlId())
                .standbyAutomatonOn(getStandbyAutomatonOn())
                .standby(getStandby())
                .b0(getB0())
                .q0(getQ0())
                .lowVoltageSetpoint(getLowVoltageSetpoint())
                .highVoltageSetpoint(getHighVoltageSetpoint())
                .lowVoltageThreshold(getLowVoltageThreshold())
                .highVoltageThreshold(getHighVoltageThreshold())
                .build();
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.staticVarCompensatorModification")
                .withUntypedValue("id", this.getEquipmentId())
                .add();
    }
}
