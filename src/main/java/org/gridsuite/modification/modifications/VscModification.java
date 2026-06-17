/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControl;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRange;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRangeAdder;
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.modifications.data.VscConverterStationModification;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_VSC_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL;
import static org.gridsuite.modification.modifications.VscCreation.VSC_CHARACTERISTICS;
import static org.gridsuite.modification.modifications.VscCreation.VSC_SETPOINTS;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsPercentage;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@Getter
@Setter
public class VscModification extends AbstractEquipmentModification {
    public static final String NO_VALUE = "No value";
    public static final String ANGLE_DROOP_ACTIVE_POWER_CONTROL_FIELD = "AngleDroopActivePowerControl";
    public static final String DROOP_FIELD = "Droop";
    public static final String P0_FIELD = "P0";
    public static final String ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG = "Angle droop active power control, Droop and P0 must be all provided or none";

    private AttributeModification<Double> nominalV;
    private AttributeModification<Double> r;
    private AttributeModification<Double> maxP;
    private AttributeModification<Float> operatorActivePowerLimitFromSide1ToSide2;
    private AttributeModification<Float> operatorActivePowerLimitFromSide2ToSide1;
    private AttributeModification<HvdcLine.ConvertersMode> convertersMode;
    private AttributeModification<Double> activePowerSetpoint;
    private AttributeModification<Boolean> angleDroopActivePowerControl;
    private AttributeModification<Float> p0;
    private AttributeModification<Float> droop;
    private VscConverterStationModification converterStation1;
    private VscConverterStationModification converterStation2;

    @Builder
    public VscModification(String equipmentId, List<FreePropertyInfos> properties,
                           AttributeModification<String> equipmentName, AttributeModification<Double> nominalV,
                           AttributeModification<Double> r, AttributeModification<Double> maxP,
                           AttributeModification<Float> operatorActivePowerLimitFromSide1ToSide2,
                           AttributeModification<Float> operatorActivePowerLimitFromSide2ToSide1,
                           AttributeModification<HvdcLine.ConvertersMode> convertersMode,
                           AttributeModification<Double> activePowerSetpoint,
                           AttributeModification<Boolean> angleDroopActivePowerControl, AttributeModification<Float> p0,
                           AttributeModification<Float> droop, VscConverterStationModification converterStation1,
                           VscConverterStationModification converterStation2) {
        super(equipmentId, properties, equipmentName);
        this.nominalV = nominalV;
        this.r = r;
        this.maxP = maxP;
        this.operatorActivePowerLimitFromSide1ToSide2 = operatorActivePowerLimitFromSide1ToSide2;
        this.operatorActivePowerLimitFromSide2ToSide1 = operatorActivePowerLimitFromSide2ToSide1;
        this.convertersMode = convertersMode;
        this.activePowerSetpoint = activePowerSetpoint;
        this.angleDroopActivePowerControl = angleDroopActivePowerControl;
        this.p0 = p0;
        this.droop = droop;
        this.converterStation1 = converterStation1;
        this.converterStation2 = converterStation2;
    }

    public static boolean shouldCreateDroopActivePowerControlExtension(boolean isPresentAngleDroopActivePowerControl, boolean isPresentDroop, boolean isPresentP0) {
        return isPresentAngleDroopActivePowerControl && isPresentDroop && isPresentP0;
    }

    protected void checkConverterStation(@Nonnull VscConverterStationModification vscConverterStationModification, @Nonnull VscConverterStation vscConverterStation) {
        String errorMessage = "Converter station '" + vscConverterStationModification.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().checkReactiveLimit(vscConverterStation, vscConverterStationModification.getMinQ(), vscConverterStationModification.getMaxQ(),
                vscConverterStationModification.getReactiveCapabilityCurvePoints(), MODIFY_VSC_ERROR, errorMessage);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (converterStation1 == null || converterStation2 == null) {
            throw new NetworkModificationException(MODIFY_VSC_ERROR, "Missing required attributes to modify the equipment");
        }
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, equipmentId);
        String errorMessage = "HVDC vsc '" + equipmentId + "' : ";

        VscConverterStation converterStation1 = ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation1().getId());
        VscConverterStation converterStation2 = ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation2().getId());
        checkConverterStation(this.converterStation1, converterStation1);
        checkConverterStation(this.converterStation2, converterStation2);
        checkDroop(hvdcLine);
        if (r != null) {
            checkIsNotNegativeValue(errorMessage, r.getValue(), MODIFY_VSC_ERROR, "Resistance R");
        }
        if (nominalV != null) {
            checkIsNotNegativeValue(errorMessage, nominalV.getValue(), MODIFY_VSC_ERROR, "Nominal voltage");
        }
        if (this.converterStation1.getVoltageSetpoint() != null) {
            checkIsNotNegativeValue(errorMessage, this.converterStation1.getVoltageSetpoint().getValue(),
                MODIFY_VSC_ERROR, "voltage set point side 1");
        }
        if (this.converterStation2.getVoltageSetpoint() != null) {
            checkIsNotNegativeValue(errorMessage, this.converterStation2.getVoltageSetpoint().getValue(),
                MODIFY_VSC_ERROR, "voltage set point side 2");
        }
        if (this.converterStation1.getLossFactor() != null) {
            checkIsPercentage(errorMessage, this.converterStation1.getLossFactor().getValue(),
                MODIFY_VSC_ERROR, "loss factor side 1");
        }
        if (this.converterStation2.getLossFactor() != null) {
            checkIsPercentage(errorMessage, this.converterStation2.getLossFactor().getValue(),
                MODIFY_VSC_ERROR, "loss factor side 2");
        }
    }

    private void checkDroop(HvdcLine hvdcLine) {
        //--- the extension already exists ---//
        HvdcAngleDroopActivePowerControl hvdcAngleDroopActivePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        if (hvdcAngleDroopActivePowerControl != null) {
            return;
        }

        //--- the extension doesn't exist yet ---//
        boolean isPresentAngleDroopActivePowerControl = angleDroopActivePowerControl != null && angleDroopActivePowerControl.getValue() != null;
        boolean isPresentDroop = droop != null && droop.getValue() != null;
        boolean isPresentP0 = p0 != null && p0.getValue() != null;
        // all fields are provided => OK extension will be created
        if (isPresentAngleDroopActivePowerControl && isPresentDroop && isPresentP0) {
            return;
        }
        // at least one field is provided but not for the others => NOT OK
        if (isPresentAngleDroopActivePowerControl || isPresentDroop || isPresentP0) {
            throw new NetworkModificationException(WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL, ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG);
        }
        // otherwise, i.e. none of the fields is provided => OK extension will not be created
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, equipmentId);
        modifyVsc(network, hvdcLine, subReportNode);
    }

    @Override
    public String getName() {
        return "VscModification";
    }

    private void modifyVsc(@Nonnull Network network, @Nonnull HvdcLine hvdcLine, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.VscModification")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        // Characteristics
        characteristics(hvdcLine, subReportNode);

        // Set Points
        List<ReportNode> setPointsReports = setPoints(hvdcLine);
        // Hvdc droop
        List<ReportNode> droopReports = hvdcAngleDroopActivePowerControlAdder(hvdcLine);

        if (!setPointsReports.isEmpty() || !droopReports.isEmpty()) {
            ReportNode setPointsReport = null;
            if (!setPointsReports.isEmpty()) {
                setPointsReport = ModificationUtils.getInstance().reportModifications(subReportNode, setPointsReports, VSC_SETPOINTS);
            }
            if (!droopReports.isEmpty()) {
                if (setPointsReport == null) {
                    setPointsReport = subReportNode.newReportNode().withMessageTemplate(VSC_SETPOINTS).add();
                }
                // Hvdc droop logs are in a subReport of Set Points
                ModificationUtils.getInstance().reportModifications(setPointsReport, droopReports, "network.modification.vscAngleDroop");
            }
        }

        // limits
        operatorActivePowerLimit(hvdcLine, subReportNode);

        // stations
        modifyConverterStation(ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation1().getId()), converterStation1, subReportNode, "1");
        modifyConverterStation(ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation2().getId()), converterStation2, subReportNode, "2");

        PropertiesUtils.applyProperties(hvdcLine, subReportNode, properties, "network.modification.VscProperties");
    }

    private void characteristics(HvdcLine hvdcLine, ReportNode subReportNode) {
        List<ReportNode> characteristicsReportsContainer = new ArrayList<>();
        if (equipmentName != null && equipmentName.getValue() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setName,
                () -> hvdcLine.getOptionalName().orElse(NO_VALUE),
                equipmentName, "Name"));
        }
        if (nominalV != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setNominalV, hvdcLine::getNominalV, nominalV,
                    "DC nominal voltage"));
        }
        if (r != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setR, hvdcLine::getR, r, "DC resistance"));
        }
        if (maxP != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setMaxP, hvdcLine::getMaxP, maxP, "Power max"));
        }
        if (!characteristicsReportsContainer.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReportsContainer, VSC_CHARACTERISTICS);
        }
    }

    private List<ReportNode> setPoints(HvdcLine hvdcLine) {

        List<ReportNode> setPointsReports = new ArrayList<>();
        if (activePowerSetpoint != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setActivePowerSetpoint, hvdcLine::getActivePowerSetpoint,
                    activePowerSetpoint, "ActivePowerSetpoint"));
        }

        if (convertersMode != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setConvertersMode, hvdcLine::getConvertersMode, convertersMode,
                    "Converters mode"));
        }
        return setPointsReports;
    }

    private void operatorActivePowerLimit(HvdcLine hvdcLine, ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        if (operatorActivePowerLimitFromSide1ToSide2 != null ||
                operatorActivePowerLimitFromSide2ToSide1 != null) {
            var operatorActivePowerRange = hvdcLine.getExtension(HvdcOperatorActivePowerRange.class);
            if (operatorActivePowerRange != null) {
                modifyOperatorActiveRange(operatorActivePowerRange, reports);

            } else {
                createOperatorActiveRangeExt(hvdcLine, reports);
            }
        }
        if (!reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, reports, "network.modification.vscLimits");
        }
    }

    private void modifyOperatorActiveRange(HvdcOperatorActivePowerRange operatorActivePowerRange, List<ReportNode> reports) {
        var oldCs1ToCs2 = operatorActivePowerRange.getOprFromCS1toCS2();
        var oldCs2ToCs1 = operatorActivePowerRange.getOprFromCS2toCS1();
        Optional.ofNullable(operatorActivePowerLimitFromSide1ToSide2).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS1toCS2(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs1ToCs2, info.getValue(), "Operator active power limit (Side1 -> Side 2)"));
            }
        });
        Optional.ofNullable(operatorActivePowerLimitFromSide2ToSide1).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS2toCS1(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs2ToCs1, info.getValue(), "Operator active power limit (Side2 -> Side 1)"));
            }
        });
    }

    private void createOperatorActiveRangeExt(HvdcLine hvdcLine, List<ReportNode> reports) {
        var hvdcOperatorActivePowerRangeAddr = hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class);
        Optional.ofNullable(operatorActivePowerLimitFromSide1ToSide2).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS1toCS2(operatorActivePowerLimitFromSide1ToSide2.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "Operator active power limit (Side1 -> Side 2)"));
            }
        });
        Optional.ofNullable(operatorActivePowerLimitFromSide2ToSide1).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS2toCS1(operatorActivePowerLimitFromSide2ToSide1.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "Operator active power limit (Side2 -> Side 1)"));
            }
        });
        hvdcOperatorActivePowerRangeAddr.add();
    }

    private void modifyExistingHvdcAngleDroopActivePowerControl(HvdcAngleDroopActivePowerControl hvdcAngleDroopActivePowerControl, List<ReportNode> reports) {
        Optional.ofNullable(angleDroopActivePowerControl).ifPresent(modification ->
            reports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(
                hvdcAngleDroopActivePowerControl::setEnabled,
                hvdcAngleDroopActivePowerControl::isEnabled,
                modification,
                ANGLE_DROOP_ACTIVE_POWER_CONTROL_FIELD)));

        Optional.ofNullable(droop).ifPresent(modification ->
            reports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(
                hvdcAngleDroopActivePowerControl::setDroop,
                hvdcAngleDroopActivePowerControl::getDroop,
                modification,
                DROOP_FIELD)));

        Optional.ofNullable(p0).ifPresent(modification ->
            reports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(
                hvdcAngleDroopActivePowerControl::setP0,
                hvdcAngleDroopActivePowerControl::getP0,
                modification,
                P0_FIELD)));
    }

    private boolean shouldCreateDroopActivePowerControlExtension() {
        return shouldCreateDroopActivePowerControlExtension(
                angleDroopActivePowerControl != null && angleDroopActivePowerControl.getValue() != null,
                droop != null && droop.getValue() != null,
                p0 != null && p0.getValue() != null);
    }

    private List<ReportNode> hvdcAngleDroopActivePowerControlAdder(HvdcLine hvdcLine) {
        List<ReportNode> reports = new ArrayList<>();
        var hvdcAngleDroopActivePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        if (hvdcAngleDroopActivePowerControl != null) {
            modifyExistingHvdcAngleDroopActivePowerControl(hvdcAngleDroopActivePowerControl, reports);
        } else if (shouldCreateDroopActivePowerControlExtension()) {
            HvdcAngleDroopActivePowerControlAdder hvdcAngleDroopActivePowerControlAdder =
                hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class);

            Boolean isEnabled = angleDroopActivePowerControl.getValue();
            hvdcAngleDroopActivePowerControlAdder.withEnabled(isEnabled);
            reports.add(ModificationUtils.getInstance().buildModificationReport(null, isEnabled, ANGLE_DROOP_ACTIVE_POWER_CONTROL_FIELD));

            Float droopValue = droop.getValue();
            hvdcAngleDroopActivePowerControlAdder.withDroop(droopValue);
            reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, droopValue, DROOP_FIELD));

            Float p0Value = p0.getValue();
            hvdcAngleDroopActivePowerControlAdder.withP0(p0Value);
            reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, p0Value, P0_FIELD));

            hvdcAngleDroopActivePowerControlAdder.add();
        }
        return reports;
    }

    private void modifyConverterStation(VscConverterStation converterStation, VscConverterStationModification vscConverterStationModification, ReportNode subReportNode, String logFieldName) {
        if (vscConverterStationModification == null || !isConverterStationModified(vscConverterStationModification)) {
            return;
        }

        ReportNode converterStationReportNode = subReportNode.newReportNode()
            .withMessageTemplate("network.modification.vscConverterStationModified")
            .withUntypedValue("fieldName", logFieldName)
            .withUntypedValue("id", converterStation.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        // characteristic
        List<ReportNode> characteristicReports = new ArrayList<>();
        if (vscConverterStationModification.getEquipmentName() != null && vscConverterStationModification.getEquipmentName().getValue() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setName,
                () -> converterStation.getOptionalName().orElse(NO_VALUE), vscConverterStationModification.getEquipmentName(), "Name"));
        }

        if (vscConverterStationModification.getLossFactor() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setLossFactor,
                converterStation::getLossFactor, vscConverterStationModification.getLossFactor(), "LossFactor"));
        }

        if (!characteristicReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(converterStationReportNode,
                characteristicReports, "network.modification.Characteristics");
        }

        // set points
        List<ReportNode> setPointsReports = new ArrayList<>();
        if (vscConverterStationModification.getReactivePowerSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setReactivePowerSetpoint,
                converterStation::getReactivePowerSetpoint, vscConverterStationModification.getReactivePowerSetpoint(), "Reactive Power"));
        }

        if (vscConverterStationModification.getVoltageRegulationOn() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setVoltageRegulatorOn,
                converterStation::isVoltageRegulatorOn, vscConverterStationModification.getVoltageRegulationOn(), "VoltageRegulationOn"));
        }

        if (vscConverterStationModification.getVoltageSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setVoltageSetpoint,
                converterStation::getVoltageSetpoint, vscConverterStationModification.getVoltageSetpoint(), "Voltage"));
        }
        if (!setPointsReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(converterStationReportNode,
                setPointsReports, "network.modification.Setpoints");
        }

        // limits
        modifyVscReactiveLimitsAttributes(vscConverterStationModification, converterStation, converterStationReportNode, converterStationReportNode);
    }

    private static boolean isConverterStationModified(VscConverterStationModification vscConverterStationModification) {
        return vscConverterStationModification.getEquipmentName() != null && vscConverterStationModification.getEquipmentName().getValue() != null
                || vscConverterStationModification.getLossFactor() != null
                || vscConverterStationModification.getReactivePowerSetpoint() != null
                || vscConverterStationModification.getVoltageRegulationOn() != null
                || vscConverterStationModification.getVoltageSetpoint() != null || vscConverterStationModification.getReactiveCapabilityCurvePoints() != null
                || vscConverterStationModification.getMinQ() != null || vscConverterStationModification.getMaxQ() != null;
    }

    private void modifyVscReactiveCapabilityCurvePoints(VscConverterStationModification vscConverterStationModification,
                                                        VscConverterStation vscConverterStation, ReportNode subReporter, ReportNode subReportNodeLimits) {

        ReactiveCapabilityCurveAdder adder = vscConverterStation.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurvePointsInfos> modificationPoints = vscConverterStationModification.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = vscConverterStation.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? vscConverterStation.getReactiveLimits(
                ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReporter, subReportNodeLimits);
    }

    private void modifyVscReactiveLimitsAttributes(VscConverterStationModification vscConverterStationModification,
                                                   VscConverterStation vscConverterStation, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        if (vscConverterStationModification.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(vscConverterStationModification.getReactiveCapabilityCurve().getValue()
                && vscConverterStationModification.getReactiveCapabilityCurvePoints() != null
                && !vscConverterStationModification.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyVscReactiveCapabilityCurvePoints(vscConverterStationModification, vscConverterStation,
                    subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(vscConverterStationModification.getReactiveCapabilityCurve().getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(vscConverterStationModification.getMinQ(),
                    vscConverterStationModification.getMaxQ(), vscConverterStation, subReportNode, subReportNodeLimits);
            }
        }
    }
}
