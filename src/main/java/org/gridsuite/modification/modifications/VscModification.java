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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.ConverterStationModificationModel;
import org.gridsuite.modification.model.ReactiveCapabilityCurvePointsModel;
import org.gridsuite.modification.model.VscModificationModel;
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

public class VscModification extends AbstractModification {
    public static final String NO_VALUE = "No value";
    public static final String ANGLE_DROOP_ACTIVE_POWER_CONTROL_FIELD = "AngleDroopActivePowerControl";
    public static final String DROOP_FIELD = "Droop";
    public static final String P0_FIELD = "P0";
    public static final String ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG = "Angle droop active power control, Droop and P0 must be all provided or none";

    private final VscModificationModel modificationModel;

    public VscModification(VscModificationModel vscModificationModel) {
        this.modificationModel = vscModificationModel;
    }

    public static boolean shouldCreateDroopActivePowerControlExtension(boolean isPresentAngleDroopActivePowerControl, boolean isPresentDroop, boolean isPresentP0) {
        return isPresentAngleDroopActivePowerControl && isPresentDroop && isPresentP0;
    }

    protected void checkConverterStation(@Nonnull ConverterStationModificationModel converterStationModificationModel, @Nonnull VscConverterStation vscConverterStation) {
        String errorMessage = "Converter station '" + converterStationModificationModel.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().checkReactiveLimit(vscConverterStation, converterStationModificationModel.getMinQ(), converterStationModificationModel.getMaxQ(),
            converterStationModificationModel.getReactiveCapabilityCurvePoints(), MODIFY_VSC_ERROR, errorMessage);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationModel == null
            || modificationModel.getConverterStation1() == null
            || modificationModel.getConverterStation2() == null) {
            throw new NetworkModificationException(MODIFY_VSC_ERROR, "Missing required attributes to modify the equipment");
        }
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, modificationModel.getEquipmentId());
        String errorMessage = "HVDC vsc '" + modificationModel.getEquipmentId() + "' : ";

        VscConverterStation converterStation1 = ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation1().getId());
        VscConverterStation converterStation2 = ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation2().getId());
        checkConverterStation(modificationModel.getConverterStation1(), converterStation1);
        checkConverterStation(modificationModel.getConverterStation2(), converterStation2);
        checkDroop(hvdcLine);
        if (modificationModel.getR() != null) {
            checkIsNotNegativeValue(errorMessage, modificationModel.getR().getValue(), MODIFY_VSC_ERROR, "Resistance R");
        }
        if (modificationModel.getNominalV() != null) {
            checkIsNotNegativeValue(errorMessage, modificationModel.getNominalV().getValue(), MODIFY_VSC_ERROR, "Nominal voltage");
        }
        if (modificationModel.getConverterStation1().getVoltageSetpoint() != null) {
            checkIsNotNegativeValue(errorMessage, modificationModel.getConverterStation1().getVoltageSetpoint().getValue(),
                MODIFY_VSC_ERROR, "voltage set point side 1");
        }
        if (modificationModel.getConverterStation2().getVoltageSetpoint() != null) {
            checkIsNotNegativeValue(errorMessage, modificationModel.getConverterStation2().getVoltageSetpoint().getValue(),
                MODIFY_VSC_ERROR, "voltage set point side 2");
        }
        if (modificationModel.getConverterStation1().getLossFactor() != null) {
            checkIsPercentage(errorMessage, modificationModel.getConverterStation1().getLossFactor().getValue(),
                MODIFY_VSC_ERROR, "loss factor side 1");
        }
        if (modificationModel.getConverterStation2().getLossFactor() != null) {
            checkIsPercentage(errorMessage, modificationModel.getConverterStation2().getLossFactor().getValue(),
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
        boolean isPresentAngleDroopActivePowerControl = modificationModel.getAngleDroopActivePowerControl() != null && modificationModel.getAngleDroopActivePowerControl().getValue() != null;
        boolean isPresentDroop = modificationModel.getDroop() != null && modificationModel.getDroop().getValue() != null;
        boolean isPresentP0 = modificationModel.getP0() != null && modificationModel.getP0().getValue() != null;
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
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, modificationModel.getEquipmentId());
        modifyVsc(network, hvdcLine, modificationModel, subReportNode);
    }

    @Override
    public String getName() {
        return "VscModification";
    }

    private void modifyVsc(@Nonnull Network network, @Nonnull HvdcLine hvdcLine, VscModificationModel modificationModel, ReportNode subReportNode) {
        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.VscModification")
            .withUntypedValue("id", modificationModel.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
        // Characteristics
        characteristics(hvdcLine, modificationModel, subReportNode);

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
        operatorActivePowerLimit(hvdcLine, modificationModel, subReportNode);

        // stations
        modifyConverterStation(ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation1().getId()), modificationModel.getConverterStation1(), subReportNode, "1");
        modifyConverterStation(ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation2().getId()), modificationModel.getConverterStation2(), subReportNode, "2");

        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationModel.getProperties(), "network.modification.VscProperties");
    }

    private static void characteristics(HvdcLine hvdcLine, VscModificationModel modificationModel, ReportNode subReportNode) {
        List<ReportNode> characteristicsReportsContainer = new ArrayList<>();
        if (modificationModel.getEquipmentName() != null && modificationModel.getEquipmentName().getValue() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setName,
                () -> hvdcLine.getOptionalName().orElse(NO_VALUE),
                modificationModel.getEquipmentName(), "Name"));
        }
        if (modificationModel.getNominalV() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setNominalV, hvdcLine::getNominalV, modificationModel.getNominalV(), "DC nominal voltage"));
        }
        if (modificationModel.getR() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setR, hvdcLine::getR, modificationModel.getR(), "DC resistance"));
        }
        if (modificationModel.getMaxP() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setMaxP, hvdcLine::getMaxP, modificationModel.getMaxP(), "Power max"));
        }
        if (!characteristicsReportsContainer.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReportsContainer, VSC_CHARACTERISTICS);
        }
    }

    private List<ReportNode> setPoints(HvdcLine hvdcLine) {

        List<ReportNode> setPointsReports = new ArrayList<>();
        if (modificationModel.getActivePowerSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setActivePowerSetpoint, hvdcLine::getActivePowerSetpoint, modificationModel.getActivePowerSetpoint(), "ActivePowerSetpoint"));
        }

        if (modificationModel.getConvertersMode() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setConvertersMode, hvdcLine::getConvertersMode, modificationModel.getConvertersMode(), "Converters mode"));
        }
        return setPointsReports;
    }

    private static void operatorActivePowerLimit(HvdcLine hvdcLine, VscModificationModel modificationModel, ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        if (modificationModel.getOperatorActivePowerLimitFromSide1ToSide2() != null ||
            modificationModel.getOperatorActivePowerLimitFromSide2ToSide1() != null) {
            var operatorActivePowerRange = hvdcLine.getExtension(HvdcOperatorActivePowerRange.class);
            if (operatorActivePowerRange != null) {
                modifyOperatorActiveRange(modificationModel, operatorActivePowerRange, reports);

            } else {
                createOperatorActiveRangeExt(hvdcLine, modificationModel, reports);
            }
        }
        if (!reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, reports, "network.modification.vscLimits");
        }
    }

    private static void modifyOperatorActiveRange(VscModificationModel modificationModel, HvdcOperatorActivePowerRange operatorActivePowerRange, List<ReportNode> reports) {
        var oldCs1ToCs2 = operatorActivePowerRange.getOprFromCS1toCS2();
        var oldCs2ToCs1 = operatorActivePowerRange.getOprFromCS2toCS1();
        Optional.ofNullable(modificationModel.getOperatorActivePowerLimitFromSide1ToSide2()).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS1toCS2(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs1ToCs2, info.getValue(), "Operator active power limit (Side1 -> Side 2)"));
            }
        });
        Optional.ofNullable(modificationModel.getOperatorActivePowerLimitFromSide2ToSide1()).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS2toCS1(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs2ToCs1, info.getValue(), "Operator active power limit (Side2 -> Side 1)"));
            }
        });
    }

    private static void createOperatorActiveRangeExt(HvdcLine hvdcLine, VscModificationModel modificationModel, List<ReportNode> reports) {
        var hvdcOperatorActivePowerRangeAddr = hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class);
        Optional.ofNullable(modificationModel.getOperatorActivePowerLimitFromSide1ToSide2()).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS1toCS2(modificationModel.getOperatorActivePowerLimitFromSide1ToSide2().getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "Operator active power limit (Side1 -> Side 2)"));
            }
        });
        Optional.ofNullable(modificationModel.getOperatorActivePowerLimitFromSide2ToSide1()).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS2toCS1(modificationModel.getOperatorActivePowerLimitFromSide2ToSide1().getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "Operator active power limit (Side2 -> Side 1)"));
            }
        });
        hvdcOperatorActivePowerRangeAddr.add();
    }

    private void modifyExistingHvdcAngleDroopActivePowerControl(HvdcAngleDroopActivePowerControl hvdcAngleDroopActivePowerControl, List<ReportNode> reports) {
        Optional.ofNullable(modificationModel.getAngleDroopActivePowerControl()).ifPresent(modification ->
            reports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(
                hvdcAngleDroopActivePowerControl::setEnabled,
                hvdcAngleDroopActivePowerControl::isEnabled,
                modification,
                ANGLE_DROOP_ACTIVE_POWER_CONTROL_FIELD)));

        Optional.ofNullable(modificationModel.getDroop()).ifPresent(modification ->
            reports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(
                hvdcAngleDroopActivePowerControl::setDroop,
                hvdcAngleDroopActivePowerControl::getDroop,
                modification,
                DROOP_FIELD)));

        Optional.ofNullable(modificationModel.getP0()).ifPresent(modification ->
            reports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(
                hvdcAngleDroopActivePowerControl::setP0,
                hvdcAngleDroopActivePowerControl::getP0,
                modification,
                P0_FIELD)));
    }

    private boolean shouldCreateDroopActivePowerControlExtension() {
        return shouldCreateDroopActivePowerControlExtension(
            modificationModel.getAngleDroopActivePowerControl() != null &&
                modificationModel.getAngleDroopActivePowerControl().getValue() != null,
            modificationModel.getDroop() != null && modificationModel.getDroop().getValue() != null,
            modificationModel.getP0() != null && modificationModel.getP0().getValue() != null);
    }

    private List<ReportNode> hvdcAngleDroopActivePowerControlAdder(HvdcLine hvdcLine) {
        List<ReportNode> reports = new ArrayList<>();
        var hvdcAngleDroopActivePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        if (hvdcAngleDroopActivePowerControl != null) {
            modifyExistingHvdcAngleDroopActivePowerControl(hvdcAngleDroopActivePowerControl, reports);
        } else if (shouldCreateDroopActivePowerControlExtension()) {
            HvdcAngleDroopActivePowerControlAdder hvdcAngleDroopActivePowerControlAdder =
                hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class);

            Boolean isEnabled = modificationModel.getAngleDroopActivePowerControl().getValue();
            hvdcAngleDroopActivePowerControlAdder.withEnabled(isEnabled);
            reports.add(ModificationUtils.getInstance().buildModificationReport(null, isEnabled, ANGLE_DROOP_ACTIVE_POWER_CONTROL_FIELD));

            Float droop = modificationModel.getDroop().getValue();
            hvdcAngleDroopActivePowerControlAdder.withDroop(droop);
            reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, droop, DROOP_FIELD));

            Float p0 = modificationModel.getP0().getValue();
            hvdcAngleDroopActivePowerControlAdder.withP0(p0);
            reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, p0, P0_FIELD));

            hvdcAngleDroopActivePowerControlAdder.add();
        }
        return reports;
    }

    private void modifyConverterStation(VscConverterStation converterStation, ConverterStationModificationModel converterStationModificationModel, ReportNode subReportNode, String logFieldName) {
        if (converterStationModificationModel == null || !isConverterStationModified(converterStationModificationModel)) {
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
        if (converterStationModificationModel.getEquipmentName() != null && converterStationModificationModel.getEquipmentName().getValue() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setName,
                () -> converterStation.getOptionalName().orElse(NO_VALUE), converterStationModificationModel.getEquipmentName(), "Name"));
        }

        if (converterStationModificationModel.getLossFactor() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setLossFactor,
                converterStation::getLossFactor, converterStationModificationModel.getLossFactor(), "LossFactor"));
        }

        if (!characteristicReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(converterStationReportNode,
                characteristicReports, "network.modification.Characteristics");
        }

        // set points
        List<ReportNode> setPointsReports = new ArrayList<>();
        if (converterStationModificationModel.getReactivePowerSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setReactivePowerSetpoint,
                converterStation::getReactivePowerSetpoint, converterStationModificationModel.getReactivePowerSetpoint(), "Reactive Power"));
        }

        if (converterStationModificationModel.getVoltageRegulationOn() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setVoltageRegulatorOn,
                converterStation::isVoltageRegulatorOn, converterStationModificationModel.getVoltageRegulationOn(), "VoltageRegulationOn"));
        }

        if (converterStationModificationModel.getVoltageSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setVoltageSetpoint,
                converterStation::getVoltageSetpoint, converterStationModificationModel.getVoltageSetpoint(), "Voltage"));
        }
        if (!setPointsReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(converterStationReportNode,
                setPointsReports, "network.modification.Setpoints");
        }

        // limits
        modifyVscReactiveLimitsAttributes(converterStationModificationModel, converterStation, converterStationReportNode, converterStationReportNode);
    }

    private static boolean isConverterStationModified(ConverterStationModificationModel converterStationModificationModel) {
        return converterStationModificationModel.getEquipmentName() != null && converterStationModificationModel.getEquipmentName().getValue() != null || converterStationModificationModel.getLossFactor() != null
            || converterStationModificationModel.getReactivePowerSetpoint() != null
            || converterStationModificationModel.getVoltageRegulationOn() != null
            || converterStationModificationModel.getVoltageSetpoint() != null || converterStationModificationModel.getReactiveCapabilityCurvePoints() != null
            || converterStationModificationModel.getMinQ() != null || converterStationModificationModel.getMaxQ() != null;
    }

    private void modifyVscReactiveCapabilityCurvePoints(ConverterStationModificationModel modificationModel,
                                                        VscConverterStation vscConverterStation, ReportNode subReporter, ReportNode subReportNodeLimits) {

        ReactiveCapabilityCurveAdder adder = vscConverterStation.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurvePointsModel> modificationPoints = modificationModel.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = vscConverterStation.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? vscConverterStation.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReporter, subReportNodeLimits);
    }

    private void modifyVscReactiveLimitsAttributes(ConverterStationModificationModel modificationModel,
                                                   VscConverterStation vscConverterStation, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        if (modificationModel.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationModel.getReactiveCapabilityCurve().getValue()
                && modificationModel.getReactiveCapabilityCurvePoints() != null
                && !modificationModel.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyVscReactiveCapabilityCurvePoints(modificationModel, vscConverterStation, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(modificationModel.getReactiveCapabilityCurve().getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(modificationModel.getMinQ(), modificationModel.getMaxQ(), vscConverterStation, subReportNode, subReportNodeLimits);
            }
        }
    }
}
