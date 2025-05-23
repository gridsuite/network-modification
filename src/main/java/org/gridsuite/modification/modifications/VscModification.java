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
import org.gridsuite.modification.dto.ConverterStationModificationInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.dto.VscModificationInfos;
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

    private final VscModificationInfos modificationInfos;

    public VscModification(VscModificationInfos vscModificationInfos) {
        this.modificationInfos = vscModificationInfos;
    }

    public static boolean shouldCreateDroopActivePowerControlExtension(boolean isPresentAngleDroopActivePowerControl, boolean isPresentDroop, boolean isPresentP0) {
        return isPresentAngleDroopActivePowerControl && isPresentDroop && isPresentP0;
    }

    protected void checkConverterStation(@Nonnull ConverterStationModificationInfos converterStationModificationInfos, @Nonnull VscConverterStation vscConverterStation) {
        String errorMessage = "Converter station '" + converterStationModificationInfos.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().checkReactiveLimit(vscConverterStation, converterStationModificationInfos.getMinQ(), converterStationModificationInfos.getMaxQ(),
                converterStationModificationInfos.getReactiveCapabilityCurvePoints(), MODIFY_VSC_ERROR, errorMessage);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null
                || modificationInfos.getConverterStation1() == null
                || modificationInfos.getConverterStation2() == null) {
            throw new NetworkModificationException(MODIFY_VSC_ERROR, "Missing required attributes to modify the equipment");
        }
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, modificationInfos.getEquipmentId());
        String errorMessage = "HVDC vsc '" + modificationInfos.getEquipmentId() + "' : ";

        VscConverterStation converterStation1 = ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation1().getId());
        VscConverterStation converterStation2 = ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation2().getId());
        checkConverterStation(modificationInfos.getConverterStation1(), converterStation1);
        checkConverterStation(modificationInfos.getConverterStation2(), converterStation2);
        checkDroop(hvdcLine);
        if (modificationInfos.getR() != null) {
            checkIsNotNegativeValue(errorMessage, modificationInfos.getR().getValue(), MODIFY_VSC_ERROR, "Resistance R");
        }
        if (modificationInfos.getNominalV() != null) {
            checkIsNotNegativeValue(errorMessage, modificationInfos.getNominalV().getValue(), MODIFY_VSC_ERROR, "Nominal voltage");
        }
        if (modificationInfos.getConverterStation1().getVoltageSetpoint() != null) {
            checkIsNotNegativeValue(errorMessage, modificationInfos.getConverterStation1().getVoltageSetpoint().getValue(),
                MODIFY_VSC_ERROR, "voltage set point side 1");
        }
        if (modificationInfos.getConverterStation2().getVoltageSetpoint() != null) {
            checkIsNotNegativeValue(errorMessage, modificationInfos.getConverterStation2().getVoltageSetpoint().getValue(),
                MODIFY_VSC_ERROR, "voltage set point side 2");
        }
        if (modificationInfos.getConverterStation1().getLossFactor() != null) {
            checkIsPercentage(errorMessage, modificationInfos.getConverterStation1().getLossFactor().getValue(),
                MODIFY_VSC_ERROR, "loss factor side 1");
        }
        if (modificationInfos.getConverterStation2().getLossFactor() != null) {
            checkIsPercentage(errorMessage, modificationInfos.getConverterStation2().getLossFactor().getValue(),
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
        boolean isPresentAngleDroopActivePowerControl = modificationInfos.getAngleDroopActivePowerControl() != null;
        boolean isPresentDroop = modificationInfos.getDroop() != null;
        boolean isPresentP0 = modificationInfos.getP0() != null;
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
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, modificationInfos.getEquipmentId());
        modifyVsc(network, hvdcLine, modificationInfos, subReportNode);
    }

    @Override
    public String getName() {
        return "VscModification";
    }

    private void modifyVsc(@Nonnull Network network, @Nonnull HvdcLine hvdcLine, VscModificationInfos modificationInfos, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.VscModification")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        // Characteristics
        characteristics(hvdcLine, modificationInfos, subReportNode);

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
        operatorActivePowerLimit(hvdcLine, modificationInfos, subReportNode);

        // stations
        modifyConverterStation(ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation1().getId()), modificationInfos.getConverterStation1(), subReportNode, "1");
        modifyConverterStation(ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation2().getId()), modificationInfos.getConverterStation2(), subReportNode, "2");

        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationInfos.getProperties(), "network.modification.VscProperties");
    }

    private static void characteristics(HvdcLine hvdcLine, VscModificationInfos modificationInfos, ReportNode subReportNode) {
        List<ReportNode> characteristicsReportsContainer = new ArrayList<>();
        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setName,
                () -> hvdcLine.getOptionalName().orElse(NO_VALUE),
                modificationInfos.getEquipmentName(), "Name"));
        }
        if (modificationInfos.getNominalV() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setNominalV, hvdcLine::getNominalV, modificationInfos.getNominalV(), "DC nominal voltage"));
        }
        if (modificationInfos.getR() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setR, hvdcLine::getR, modificationInfos.getR(), "DC resistance"));
        }
        if (modificationInfos.getMaxP() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setMaxP, hvdcLine::getMaxP, modificationInfos.getMaxP(), "Power max"));
        }
        if (!characteristicsReportsContainer.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReportsContainer, VSC_CHARACTERISTICS);
        }
    }

    private List<ReportNode> setPoints(HvdcLine hvdcLine) {

        List<ReportNode> setPointsReports = new ArrayList<>();
        if (modificationInfos.getActivePowerSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setActivePowerSetpoint, hvdcLine::getActivePowerSetpoint, modificationInfos.getActivePowerSetpoint(), "ActivePowerSetpoint"));
        }

        if (modificationInfos.getConvertersMode() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setConvertersMode, hvdcLine::getConvertersMode, modificationInfos.getConvertersMode(), "Converters mode"));
        }
        return setPointsReports;
    }

    private static void operatorActivePowerLimit(HvdcLine hvdcLine, VscModificationInfos modificationInfos, ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        if (modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2() != null ||
                modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1() != null) {
            var operatorActivePowerRange = hvdcLine.getExtension(HvdcOperatorActivePowerRange.class);
            if (operatorActivePowerRange != null) {
                modifyOperatorActiveRange(modificationInfos, operatorActivePowerRange, reports);

            } else {
                createOperatorActiveRangeExt(hvdcLine, modificationInfos, reports);
            }
        }
        if (!reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, reports, "network.modification.vscLimits");
        }
    }

    private static void modifyOperatorActiveRange(VscModificationInfos modificationInfos, HvdcOperatorActivePowerRange operatorActivePowerRange, List<ReportNode> reports) {
        var oldCs1ToCs2 = operatorActivePowerRange.getOprFromCS1toCS2();
        var oldCs2ToCs1 = operatorActivePowerRange.getOprFromCS2toCS1();
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2()).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS1toCS2(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs1ToCs2, info.getValue(), "Operator active power limit (Side1 -> Side 2)"));
            }
        });
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1()).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS2toCS1(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs2ToCs1, info.getValue(), "Operator active power limit (Side2 -> Side 1)"));
            }
        });
    }

    private static void createOperatorActiveRangeExt(HvdcLine hvdcLine, VscModificationInfos modificationInfos, List<ReportNode> reports) {
        var hvdcOperatorActivePowerRangeAddr = hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class);
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2()).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS1toCS2(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2().getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "Operator active power limit (Side1 -> Side 2)"));
            }
        });
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1()).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS2toCS1(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1().getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "Operator active power limit (Side2 -> Side 1)"));
            }
        });
        hvdcOperatorActivePowerRangeAddr.add();
    }

    private void modifyExistingHvdcAngleDroopActivePowerControl(HvdcAngleDroopActivePowerControl hvdcAngleDroopActivePowerControl, List<ReportNode> reports) {
        Optional.ofNullable(modificationInfos.getAngleDroopActivePowerControl()).ifPresent(modification ->
            reports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(
                hvdcAngleDroopActivePowerControl::setEnabled,
                hvdcAngleDroopActivePowerControl::isEnabled,
                modification,
                ANGLE_DROOP_ACTIVE_POWER_CONTROL_FIELD)));

        Optional.ofNullable(modificationInfos.getDroop()).ifPresent(modification ->
            reports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(
                hvdcAngleDroopActivePowerControl::setDroop,
                hvdcAngleDroopActivePowerControl::getDroop,
                modification,
                DROOP_FIELD)));

        Optional.ofNullable(modificationInfos.getP0()).ifPresent(modification ->
            reports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(
                hvdcAngleDroopActivePowerControl::setP0,
                hvdcAngleDroopActivePowerControl::getP0,
                modification,
                P0_FIELD)));
    }

    private boolean shouldCreateDroopActivePowerControlExtension() {
        return shouldCreateDroopActivePowerControlExtension(
            modificationInfos.getAngleDroopActivePowerControl() != null, modificationInfos.getDroop() != null, modificationInfos.getP0() != null);
    }

    private List<ReportNode> hvdcAngleDroopActivePowerControlAdder(HvdcLine hvdcLine) {
        List<ReportNode> reports = new ArrayList<>();
        var hvdcAngleDroopActivePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        if (hvdcAngleDroopActivePowerControl != null) {
            modifyExistingHvdcAngleDroopActivePowerControl(hvdcAngleDroopActivePowerControl, reports);
        } else if (shouldCreateDroopActivePowerControlExtension()) {
            HvdcAngleDroopActivePowerControlAdder hvdcAngleDroopActivePowerControlAdder =
                hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class);

            Boolean isEnabled = modificationInfos.getAngleDroopActivePowerControl().getValue();
            hvdcAngleDroopActivePowerControlAdder.withEnabled(isEnabled);
            reports.add(ModificationUtils.getInstance().buildModificationReport(null, isEnabled, ANGLE_DROOP_ACTIVE_POWER_CONTROL_FIELD));

            Float droop = modificationInfos.getDroop().getValue();
            hvdcAngleDroopActivePowerControlAdder.withDroop(droop);
            reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, droop, DROOP_FIELD));

            Float p0 = modificationInfos.getP0().getValue();
            hvdcAngleDroopActivePowerControlAdder.withP0(p0);
            reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, p0, P0_FIELD));

            hvdcAngleDroopActivePowerControlAdder.add();
        }
        return reports;
    }

    private void modifyConverterStation(VscConverterStation converterStation, ConverterStationModificationInfos converterStationModificationInfos, ReportNode subReportNode, String logFieldName) {
        if (converterStationModificationInfos == null || !isConverterStationModified(converterStationModificationInfos)) {
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
        if (converterStationModificationInfos.getEquipmentName() != null && converterStationModificationInfos.getEquipmentName().getValue() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setName,
                () -> converterStation.getOptionalName().orElse(NO_VALUE), converterStationModificationInfos.getEquipmentName(), "Name"));
        }

        if (converterStationModificationInfos.getLossFactor() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setLossFactor,
                converterStation::getLossFactor, converterStationModificationInfos.getLossFactor(), "LossFactor"));
        }

        if (!characteristicReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(converterStationReportNode,
                characteristicReports, "network.modification.Characteristics");
        }

        // set points
        List<ReportNode> setPointsReports = new ArrayList<>();
        if (converterStationModificationInfos.getReactivePowerSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setReactivePowerSetpoint,
                converterStation::getReactivePowerSetpoint, converterStationModificationInfos.getReactivePowerSetpoint(), "Reactive Power"));
        }

        if (converterStationModificationInfos.getVoltageRegulationOn() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setVoltageRegulatorOn,
                converterStation::isVoltageRegulatorOn, converterStationModificationInfos.getVoltageRegulationOn(), "VoltageRegulationOn"));
        }

        if (converterStationModificationInfos.getVoltageSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setVoltageSetpoint,
                converterStation::getVoltageSetpoint, converterStationModificationInfos.getVoltageSetpoint(), "Voltage"));
        }
        if (!setPointsReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(converterStationReportNode,
                setPointsReports, "network.modification.Setpoints");
        }

        // limits
        modifyVscReactiveLimitsAttributes(converterStationModificationInfos, converterStation, converterStationReportNode, converterStationReportNode);
    }

    private static boolean isConverterStationModified(ConverterStationModificationInfos converterStationModificationInfos) {
        return converterStationModificationInfos.getEquipmentName() != null && converterStationModificationInfos.getEquipmentName().getValue() != null || converterStationModificationInfos.getLossFactor() != null
                || converterStationModificationInfos.getReactivePowerSetpoint() != null
                || converterStationModificationInfos.getVoltageRegulationOn() != null
                || converterStationModificationInfos.getVoltageSetpoint() != null || converterStationModificationInfos.getReactiveCapabilityCurvePoints() != null
                || converterStationModificationInfos.getMinQ() != null || converterStationModificationInfos.getMaxQ() != null;
    }

    private void modifyVscReactiveCapabilityCurvePoints(ConverterStationModificationInfos modificationInfos,
                                                        VscConverterStation vscConverterStation, ReportNode subReporter, ReportNode subReportNodeLimits) {

        ReactiveCapabilityCurveAdder adder = vscConverterStation.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurvePointsInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = vscConverterStation.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? vscConverterStation.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReporter, subReportNodeLimits);
    }

    private void modifyVscReactiveLimitsAttributes(ConverterStationModificationInfos modificationInfos,
                                                   VscConverterStation vscConverterStation, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        if (modificationInfos.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                    && modificationInfos.getReactiveCapabilityCurvePoints() != null
                    && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyVscReactiveCapabilityCurvePoints(modificationInfos, vscConverterStation, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(modificationInfos.getMinQ(), modificationInfos.getMaxQ(), vscConverterStation, subReportNode, subReportNodeLimits);
            }
        }
    }
}
