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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.Collection;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_BATTERY_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsPercentage;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Getter
@Setter
public class BatteryModification extends AbstractInjectionModification {

    public static final String ERROR_MESSAGE = "Battery '%s' : ";

    private AttributeModification<Double> minP;
    private AttributeModification<Double> maxP;
    private AttributeModification<Double> targetP;
    private AttributeModification<Double> targetQ;
    private AttributeModification<Boolean> participate;
    private AttributeModification<Float> droop;
    private AttributeModification<Double> directTransX;
    private AttributeModification<Double> stepUpTransformerX;
    private AttributeModification<Double> minQ;
    private AttributeModification<Double> maxQ;
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;
    private AttributeModification<Boolean> reactiveCapabilityCurve;

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
                               AttributeModification<Boolean> reactiveCapabilityCurve) {
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
        checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(battery, MODIFY_BATTERY_ERROR, errorMessage);
        if (droop != null) {
            checkIsPercentage(errorMessage, droop.getValue(), MODIFY_BATTERY_ERROR, "Droop");
        }
    }

    private void checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(Battery battery, NetworkModificationException.Type exceptionType,
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
                                                        Battery battery,
                                                        ReportNode subReportNode) {
        ReportNode reportActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setTargetP, battery::getTargetP, targetP, "Active power");
        ReportNode reportReactivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setTargetQ, battery::getTargetQ, targetQ, "Reactive power");
        ReportNode subReporterSetpoints = null;
        if (subReportNode != null && (reportActivePower != null || reportReactivePower != null)) {
            subReporterSetpoints = subReportNode.newReportNode().withMessageTemplate("network.modification.Setpoints").add();
            if (reportActivePower != null) {
                insertReportNode(subReporterSetpoints, reportActivePower);
            }
            if (reportReactivePower != null) {
                insertReportNode(subReporterSetpoints, reportReactivePower);
            }
        }
        modifyBatteryActivePowerControlAttributes(participate, droop, battery, subReportNode, subReporterSetpoints);
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
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, battery,
                equipmentId, voltageLevelId, busOrBusbarSectionId, connectionName, connectionDirection, connectionPosition, terminalConnected, subReportNode);
    }
}
