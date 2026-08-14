/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.error.NetworkModificationExceptionType;
import org.gridsuite.modification.modifications.data.VoltageRegulationModification;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.gridsuite.modification.error.NetworkModificationExceptionType.MODIFY_BATTERY_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Getter
@Setter
public class BatteryModification extends AbstractInjectionModification {

    public static final String ERROR_MESSAGE = "Battery '%s' : ";
    private static final String TARGET_VOLTAGE = "Target Voltage";

    private AttributeModification<Double> minP;
    private AttributeModification<Double> maxP;
    private AttributeModification<Double> targetP;
    private AttributeModification<Double> targetQ;
    private AttributeModification<Double> targetV;
    private AttributeModification<Boolean> voltageRegulationOn;
    private AttributeModification<Boolean> participate;
    private AttributeModification<Float> droop;
    private AttributeModification<Double> directTransX;
    private AttributeModification<Double> stepUpTransformerX;
    private AttributeModification<Double> minQ;
    private AttributeModification<Double> maxQ;
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;
    private AttributeModification<Boolean> reactiveCapabilityCurve;
    private AttributeModification<VoltageRegulationType> voltageRegulationType;
    private AttributeModification<String> regulatingTerminalId;
    private AttributeModification<String> regulatingTerminalType;
    private AttributeModification<String> regulatingTerminalVlId;

    @Builder
    public BatteryModification(String equipmentId, List<FreePropertyInfos> properties,
                               AttributeModification<String> equipmentName,
                               AttributeModification<String> voltageLevelId,
                               AttributeModification<String> busOrBusbarSectionId,
                               AttributeModification<String> connectionName,
                               AttributeModification<ConnectablePosition.Direction> connectionDirection,
                               AttributeModification<Integer> connectionPosition,
                               AttributeModification<Boolean> terminalConnected,
                               AttributeModification<Double> pMeasurementValue,
                               AttributeModification<Boolean> pMeasurementValidity,
                               AttributeModification<Double> qMeasurementValue,
                               AttributeModification<Boolean> qMeasurementValidity, AttributeModification<Double> minP,
                               AttributeModification<Double> maxP, AttributeModification<Double> targetP,
                               AttributeModification<Double> targetQ, AttributeModification<Boolean> participate,
                               AttributeModification<Float> droop, AttributeModification<Double> directTransX,
                               AttributeModification<Double> stepUpTransformerX, AttributeModification<Double> minQ,
                               AttributeModification<Double> maxQ,
                               List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints,
                               AttributeModification<Boolean> reactiveCapabilityCurve,
                               AttributeModification<VoltageRegulationType> voltageRegulationType,
                               AttributeModification<String> regulatingTerminalId,
                               AttributeModification<String> regulatingTerminalType,
                               AttributeModification<String> regulatingTerminalVlId,
                               AttributeModification<Double> targetV,
                               AttributeModification<Boolean> voltageRegulationOn) {
        super(equipmentId, properties, equipmentName, voltageLevelId, busOrBusbarSectionId, connectionName,
            connectionDirection, connectionPosition, terminalConnected, pMeasurementValue, pMeasurementValidity,
            qMeasurementValue, qMeasurementValidity);
        this.minP = minP;
        this.maxP = maxP;
        this.targetP = targetP;
        this.targetQ = targetQ;
        this.participate = participate;
        this.droop = droop;
        this.directTransX = directTransX;
        this.stepUpTransformerX = stepUpTransformerX;
        this.minQ = minQ;
        this.maxQ = maxQ;
        this.reactiveCapabilityCurvePoints = reactiveCapabilityCurvePoints;
        this.reactiveCapabilityCurve = reactiveCapabilityCurve;
        this.voltageRegulationType = voltageRegulationType;
        this.regulatingTerminalId = regulatingTerminalId;
        this.regulatingTerminalType = regulatingTerminalType;
        this.regulatingTerminalVlId = regulatingTerminalVlId;
        this.targetV = targetV;
        this.voltageRegulationOn = voltageRegulationOn;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (equipmentId == null) {
            throw new NetworkModificationException(MODIFY_BATTERY_ERROR, "Missing required attributes to modify the equipment");
        }

        Battery battery = ModificationUtils.getInstance().getBattery(network, equipmentId);
        String errorMessage = "Battery '" + equipmentId + "' : ";
        ModificationUtils.getInstance().checkVoltageLevelModification(network, voltageLevelId,
                busOrBusbarSectionId, battery.getTerminal());
        ModificationUtils.getInstance().checkReactiveLimit(battery, minQ, maxQ,
                reactiveCapabilityCurvePoints, MODIFY_BATTERY_ERROR, errorMessage);
        VoltageRegulation voltageRegulation = battery.getExtension(VoltageRegulation.class);
        Terminal regulatingTerminal = voltageRegulation != null ? voltageRegulation.getRegulatingTerminal() : null;
        ModificationUtils.getInstance().checkEnableRegulation(
                voltageRegulationType,
                regulatingTerminalId,
                regulatingTerminalType,
                regulatingTerminalVlId,
                battery.getTerminal(),
                regulatingTerminal,
                network,
                MODIFY_BATTERY_ERROR,
                errorMessage);
        checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(battery, MODIFY_BATTERY_ERROR, errorMessage);
        if (droop != null) {
            checkIsPercentage(errorMessage, droop.getValue(), MODIFY_BATTERY_ERROR, "Droop");
        }
        if (targetV != null) {
            checkIsNotNegativeValue(errorMessage, targetV.getValue(), MODIFY_BATTERY_ERROR, TARGET_VOLTAGE);
        }
        ModificationUtils.checkVoltageRegulation(errorMessage, voltageRegulation, voltageRegulationOn, targetV, MODIFY_BATTERY_ERROR);
    }

    private void checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(Battery battery, NetworkModificationExceptionType exceptionType,
            String errorMessage) {
        ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                targetP,
                minP,
                maxP,
                battery.getMinP(),
                battery.getMaxP(),
                battery.getTargetP(),
                exceptionType,
                errorMessage
        );
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Battery battery = ModificationUtils.getInstance().getBattery(network, equipmentId);
        // modify the battery in the network
        modifyBattery(battery, subReportNode);
    }

    @Override
    public String getName() {
        return "BatteryModification";
    }

    private void modifyBattery(Battery battery, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.batteryModification")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (equipmentName != null && equipmentName.getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(battery::setName, () -> battery.getOptionalName().orElse("No value"), equipmentName,
                    subReportNode, "Name");
        }
        modifyBatteryVoltageLevelBusOrBusBarSectionAttributes(battery, subReportNode);
        modifyBatteryLimitsAttributes(battery, subReportNode);
        modifyBatterySetpointsAttributes(
                targetP, targetQ,
                participate, droop,
                new VoltageRegulationModification(targetV, voltageRegulationOn, regulatingTerminalId,
                regulatingTerminalType, regulatingTerminalVlId, voltageRegulationType),
                battery, subReportNode);
        modifyBatteryConnectivityAttributes(battery, subReportNode);
        updateMeasurements(battery, subReportNode);
        ModificationUtils.getInstance().modifyShortCircuitExtension(directTransX,
                stepUpTransformerX,
                battery.getExtension(BatteryShortCircuit.class),
                () -> battery.newExtension(BatteryShortCircuitAdder.class),
                subReportNode);
        PropertiesUtils.applyProperties(battery, subReportNode, properties, "network.modification.BatteryProperties");
    }

    public static void modifyBatterySetpointsAttributes(AttributeModification<Double> targetP,
                                                        AttributeModification<Double> targetQ,
                                                        AttributeModification<Boolean> participate,
                                                        AttributeModification<Float> droop,
                                                        VoltageRegulationModification voltageRegulationModification,
                                                        Battery battery,
                                                        ReportNode subReportNode) {
        ReportNode reportActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setTargetP, battery::getTargetP, targetP, "Active power");
        ReportNode reportReactivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setTargetQ, battery::getTargetQ, targetQ, "Reactive power");
        List<ReportNode> voltageRegulationReports = modifyVoltageRegulation(battery, voltageRegulationModification);
        ReportNode subReporterSetpoints = null;
        if (subReportNode != null && (reportActivePower != null || reportReactivePower != null || !voltageRegulationReports.isEmpty())) {
            subReporterSetpoints = subReportNode.newReportNode().withMessageTemplate("network.modification.Setpoints").add();
            if (reportActivePower != null) {
                insertReportNode(subReporterSetpoints, reportActivePower);
            }
            if (reportReactivePower != null) {
                insertReportNode(subReporterSetpoints, reportReactivePower);
            }
            if (!voltageRegulationReports.isEmpty()) {
                ModificationUtils.getInstance().reportModifications(subReporterSetpoints, voltageRegulationReports, "network.modification.voltageRegulationModified");
            }
        }
        modifyBatteryActivePowerControlAttributes(participate, droop, battery, subReportNode, subReporterSetpoints);
    }

    public static List<ReportNode> modifyVoltageRegulation(Battery battery, VoltageRegulationModification voltageRegulationModification) {
        List<ReportNode> voltageRegulationReports = new ArrayList<>();
        boolean hasVoltageRegulationChange = voltageRegulationModification != null &&
                (voltageRegulationModification.getTargetV() != null
                        || voltageRegulationModification.getVoltageRegulationOn() != null && voltageRegulationModification.getVoltageRegulationOn().getValue() != null
                        || voltageRegulationModification.getVoltageRegulationType() != null ||
                            voltageRegulationModification.getRegulatingTerminalId() != null
                                    && voltageRegulationModification.getRegulatingTerminalType() != null
                                    && voltageRegulationModification.getRegulatingTerminalVlId() != null);
        if (!hasVoltageRegulationChange) {
            return voltageRegulationReports;
        }
        VoltageRegulation voltageRegulation = battery.getExtension(VoltageRegulation.class);
        if (voltageRegulation == null) {
            voltageRegulation = battery.newExtension(VoltageRegulationAdder.class)
                    .withVoltageRegulatorOn(false)
                    .add();
        }
        // target V
        if (voltageRegulationModification.getTargetV() != null) {
            setTargetV(voltageRegulation, voltageRegulationModification.getTargetV(), voltageRegulationReports);
        }
        // voltage Regulation On
        ReportNode voltageRegulationOnReportNode = ModificationUtils.getInstance()
                .applyElementaryModificationsAndReturnReport(voltageRegulation::setVoltageRegulatorOn, voltageRegulation::isVoltageRegulatorOn,
                voltageRegulationModification.getVoltageRegulationOn(), "VoltageRegulationOn");
        if (voltageRegulationOnReportNode != null) {
            voltageRegulationReports.add(voltageRegulationOnReportNode);
        }
        // regulating terminal
        setRegulatingTerminal(battery, voltageRegulation,
                voltageRegulationModification.getRegulatingTerminalId(), voltageRegulationModification.getRegulatingTerminalType(),
                voltageRegulationModification.getRegulatingTerminalVlId(), voltageRegulationModification.getVoltageRegulationType(),
                voltageRegulationReports);
        return voltageRegulationReports;
    }

    private static void setTargetV(VoltageRegulation voltageRegulation, AttributeModification<Double> targetV, List<ReportNode> voltageRegulationReports) {
        Double oldValue = voltageRegulation.getTargetV();
        Double newValue = Double.NaN;
        if (targetV.getOp() == OperationType.SET) {
            // we always keep the equivalent local target voltage in the network
            voltageRegulation.setTargetV(targetV.getValue());
            newValue = targetV.getValue();
        } else {
            voltageRegulation.setTargetV(Double.NaN);
        }
        voltageRegulationReports.add(ModificationUtils.getInstance()
                .buildModificationReport(oldValue, newValue, "Target V"));
    }

    private static void setRegulatingTerminal(Battery battery,
                                              VoltageRegulation voltageRegulation,
                                              AttributeModification<String> regulatingTerminalId,
                                              AttributeModification<String> regulatingTerminalType,
                                              AttributeModification<String> regulatingTerminalVlId,
                                              AttributeModification<VoltageRegulationType> voltageRegulationType,
                                              List<ReportNode> voltageRegulationReports) {
        Terminal regulatingTerminal = voltageRegulation.getRegulatingTerminal();
        String oldVoltageLevel = null;
        String oldEquipment = null;
        // If there is no regulating terminal in file, regulating terminal voltage level
        // is equal to generator voltage level
        if (regulatingTerminal != null
                && !regulatingTerminal.getVoltageLevel().equals(battery.getTerminal().getVoltageLevel())) {
            oldVoltageLevel = regulatingTerminal.getVoltageLevel().getId();
            oldEquipment = regulatingTerminal.getConnectable().getType().name() + ":"
                    + regulatingTerminal.getConnectable().getId();
        }
        if (regulatingTerminalId != null
                && regulatingTerminalType != null
                && regulatingTerminalVlId != null) {
            Terminal newRegulatingTerminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(battery.getNetwork(),
                    regulatingTerminalId.getValue(),
                    regulatingTerminalType.getValue(),
                    regulatingTerminalVlId.getValue());
            voltageRegulation.setRegulatingTerminal(newRegulatingTerminal);
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                    regulatingTerminalVlId.getValue(),
                    "Voltage level"));
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    regulatingTerminalType.getValue() + ":"
                            + regulatingTerminalId.getValue(),
                    "Equipment"));
        }
        if (voltageRegulationType != null
                && voltageRegulationType.getValue() == VoltageRegulationType.LOCAL
                && oldEquipment != null && oldVoltageLevel != null) {
            // setting regulating terminal to null set to local terminal
            voltageRegulation.setRegulatingTerminal(null);
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                    null,
                    "Voltage level"));
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    null,
                    "Equipment"));
        }
    }

    private void modifyBatteryVoltageLevelBusOrBusBarSectionAttributes(Battery battery, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                battery, battery.getTerminal(),
                voltageLevelId,
                busOrBusbarSectionId,
                subReportNode
        );
    }

    private void modifyBatteryLimitsAttributes(Battery battery, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = modifyBatteryActiveLimitsAttributes(maxP, minP, battery, subReportNode);
        modifyBatteryReactiveLimitsAttributes(battery, subReportNode, subReportNodeLimits);
    }

    private void modifyBatteryReactiveCapabilityCurvePoints(Battery battery, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        ReactiveCapabilityCurveAdder adder = battery.newReactiveCapabilityCurve();
        Collection<ReactiveCapabilityCurve.Point> points = battery.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? battery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints(
                ) : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, reactiveCapabilityCurvePoints, adder, subReportNode, subReportNodeLimits);
    }

    public static ReportNode modifyBatteryActiveLimitsAttributes(AttributeModification<Double> maxP,
                                                                 AttributeModification<Double> minP,
                                                                 Battery battery, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = null;
        ReportNode reportMaxActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setMaxP, battery::getMaxP, maxP, "Max active power");
        ReportNode reportMinActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setMinP, battery::getMinP, minP, "Min active power");
        if (subReportNode != null && (reportMaxActivePower != null || reportMinActivePower != null)) {
            subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
            ReportNode subReporterActiveLimits = subReportNodeLimits.newReportNode().withMessageTemplate("network.modification.activelimits").add();
            if (reportMaxActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMaxActivePower);
            }
            if (reportMinActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMinActivePower);
            }
        }
        return subReportNodeLimits;
    }

    private void modifyBatteryReactiveLimitsAttributes(Battery battery, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        if (reactiveCapabilityCurve != null) {
            if (Boolean.TRUE.equals(reactiveCapabilityCurve.getValue()
                    && reactiveCapabilityCurvePoints != null
                    && !reactiveCapabilityCurvePoints.isEmpty())) {
                modifyBatteryReactiveCapabilityCurvePoints(battery, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(reactiveCapabilityCurve.getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(minQ, maxQ, battery, subReportNode, subReportNodeLimits);
            }
        }
    }

    public static ReportNode modifyBatteryActivePowerControlAttributes(AttributeModification<Boolean> participate,
                                                                       AttributeModification<Float> droop,
                                                                       Battery battery,
                                                                       ReportNode subReportNode,
                                                                       ReportNode subReportNodeSetpoints) {
        ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
        ActivePowerControlAdder<Battery> activePowerControlAdder = battery.newExtension(ActivePowerControlAdder.class);
        return ModificationUtils.getInstance().modifyActivePowerControlAttributes(activePowerControl, activePowerControlAdder,
            participate, droop, subReportNode, subReportNodeSetpoints, MODIFY_BATTERY_ERROR, String.format(ERROR_MESSAGE, battery.getId()));
    }

    private ReportNode modifyBatteryConnectivityAttributes(Battery battery, ReportNode subReportNode) {
        ConnectablePosition<Battery> connectablePosition = battery.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Battery> connectablePositionAdder = battery.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, battery, this, subReportNode);
    }
}
