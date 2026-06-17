package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.HvdcLine;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.VscModification;
import org.gridsuite.modification.modifications.data.VscConverterStationModification;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "VSC modification")
@JsonTypeName("VSC_MODIFICATION")
@ModificationErrorTypeName("MODIFY_VSC_ERROR")
public class VscModificationInfos extends BasicEquipmentModificationInfos {
    @Schema(description = "DC nominal voltage")
    private AttributeModification<Double> nominalV;

    @Schema(description = "DC resistance")
    private AttributeModification<Double> r;

    @Schema(description = "Maximum active power ")
    private AttributeModification<Double> maxP;

    @Schema(description = "Operator active power limit (Side1->Side2)")
    private AttributeModification<Float> operatorActivePowerLimitFromSide1ToSide2;

    @Schema(description = "Operator active power limit (Side2->Side1)")
    private AttributeModification<Float> operatorActivePowerLimitFromSide2ToSide1;

    @Schema(description = "Converters mode")
    private AttributeModification<HvdcLine.ConvertersMode> convertersMode;

    @Schema(description = "Active power setpoint")
    private AttributeModification<Double> activePowerSetpoint;

    @Schema(description = "Angle droop active power control ")
    private AttributeModification<Boolean> angleDroopActivePowerControl;

    @Schema(description = "p0")
    private AttributeModification<Float> p0;

    @Schema(description = "droop")
    private AttributeModification<Float> droop;

    @Schema(description = "Converter station 1")
    private ConverterStationModificationInfos converterStation1;

    @Schema(description = "Converter station 2")
    private ConverterStationModificationInfos converterStation2;

    @Override
    public AbstractModification toModification() {
        return VscModification.builder()
                .equipmentId(getEquipmentId())
                .properties(getProperties())
                .equipmentName(getEquipmentName())
                .nominalV(getNominalV())
                .r(getR())
                .maxP(getMaxP())
                .operatorActivePowerLimitFromSide1ToSide2(getOperatorActivePowerLimitFromSide1ToSide2())
                .operatorActivePowerLimitFromSide2ToSide1(getOperatorActivePowerLimitFromSide2ToSide1())
                .convertersMode(getConvertersMode())
                .activePowerSetpoint(getActivePowerSetpoint())
                .angleDroopActivePowerControl(getAngleDroopActivePowerControl())
                .p0(getP0())
                .droop(getDroop())
                .converterStation1(converterStationModification(converterStation1))
                .converterStation2(converterStationModification(converterStation2))
                .build();
    }

    private VscConverterStationModification converterStationModification(ConverterStationModificationInfos converterStationModificationInfos) {
        if (converterStationModificationInfos == null) {
            return null;
        }
        return VscConverterStationModification.builder().lossFactor(converterStationModificationInfos.getLossFactor())
            .reactivePowerSetpoint(converterStationModificationInfos.getReactivePowerSetpoint())
            .voltageRegulationOn(converterStationModificationInfos.getVoltageRegulationOn())
            .voltageSetpoint(converterStationModificationInfos.getVoltageSetpoint())
            .reactiveCapabilityCurve(converterStationModificationInfos.getReactiveCapabilityCurve())
            .minQ(converterStationModificationInfos.getMinQ()).maxQ(converterStationModificationInfos.getMaxQ())
            .reactiveCapabilityCurvePoints(converterStationModificationInfos.getReactiveCapabilityCurvePoints())
            .equipmentId(converterStationModificationInfos.getEquipmentId())
            .properties(converterStationModificationInfos.getProperties())
            .equipmentName(converterStationModificationInfos.getEquipmentName())
            .voltageLevelId(converterStationModificationInfos.getVoltageLevelId())
            .busOrBusbarSectionId(converterStationModificationInfos.getBusOrBusbarSectionId())
            .connectionName(converterStationModificationInfos.getConnectionName())
            .connectionDirection(converterStationModificationInfos.getConnectionDirection())
            .connectionPosition(converterStationModificationInfos.getConnectionPosition())
            .terminalConnected(converterStationModificationInfos.getTerminalConnected())
            .pMeasurementValue(converterStationModificationInfos.getPMeasurementValue())
            .pMeasurementValidity(converterStationModificationInfos.getPMeasurementValidity())
            .qMeasurementValue(converterStationModificationInfos.getQMeasurementValue())
            .qMeasurementValidity(converterStationModificationInfos.getQMeasurementValidity())
            .build();
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.vsc.modification")
                .withUntypedValue("vscId", getEquipmentId())
                .add();
    }
}
