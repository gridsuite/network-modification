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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.AttributeModification;
import org.gridsuite.modification.model.BatteryModificationModel;
import org.gridsuite.modification.model.ReactiveCapabilityCurvePointsModel;
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
public class BatteryModification extends AbstractInjectionModification {

    public static final String ERROR_MESSAGE = "Battery '%s' : ";

    public BatteryModification(BatteryModificationModel modificationModel) {
        super(modificationModel);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationModel == null) {
            throw new NetworkModificationException(MODIFY_BATTERY_ERROR, "Missing required attributes to modify the equipment");
        }

        BatteryModificationModel batteryModificationModel = (BatteryModificationModel) modificationModel;
        Battery battery = ModificationUtils.getInstance().getBattery(network, batteryModificationModel.getEquipmentId());
        String errorMessage = "Battery '" + batteryModificationModel.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().checkVoltageLevelModification(network, batteryModificationModel.getVoltageLevelId(),
                batteryModificationModel.getBusOrBusbarSectionId(), battery.getTerminal());
        ModificationUtils.getInstance().checkReactiveLimit(battery, batteryModificationModel.getMinQ(), batteryModificationModel.getMaxQ(),
                batteryModificationModel.getReactiveCapabilityCurvePoints(), MODIFY_BATTERY_ERROR, errorMessage);
        checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(batteryModificationModel, battery, MODIFY_BATTERY_ERROR, errorMessage);
        if (batteryModificationModel.getDroop() != null) {
            checkIsPercentage(errorMessage, batteryModificationModel.getDroop().getValue(), MODIFY_BATTERY_ERROR, "Droop");
        }
    }

    private void checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(BatteryModificationModel modificationModel, Battery battery, NetworkModificationException.Type exceptionType, String errorMessage) {
        ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                modificationModel.getTargetP(),
                modificationModel.getMinP(),
                modificationModel.getMaxP(),
                battery.getMinP(),
                battery.getMaxP(),
                battery.getTargetP(),
                exceptionType,
                errorMessage
        );
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Battery battery = ModificationUtils.getInstance().getBattery(network, modificationModel.getEquipmentId());
        // modify the battery in the network
        modifyBattery(battery, subReportNode);
    }

    @Override
    public String getName() {
        return "BatteryModification";
    }

    private void modifyBattery(Battery battery, ReportNode subReportNode) {
        BatteryModificationModel batteryModificationModel = (BatteryModificationModel) modificationModel;
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.batteryModification")
                .withUntypedValue("id", batteryModificationModel.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (batteryModificationModel.getEquipmentName() != null && batteryModificationModel.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(battery::setName, () -> battery.getOptionalName().orElse("No value"), batteryModificationModel.getEquipmentName(), subReportNode, "Name");
        }
        modifyBatteryVoltageLevelBusOrBusBarSectionAttributes(batteryModificationModel, battery, subReportNode);
        modifyBatteryLimitsAttributes(batteryModificationModel, battery, subReportNode);
        modifyBatterySetpointsAttributes(
                batteryModificationModel.getTargetP(), batteryModificationModel.getTargetQ(),
                batteryModificationModel.getParticipate(), batteryModificationModel.getDroop(),
                battery, subReportNode);
        modifyBatteryConnectivityAttributes(batteryModificationModel, battery, subReportNode);
        updateMeasurements(battery, batteryModificationModel, subReportNode);
        ModificationUtils.getInstance().modifyShortCircuitExtension(batteryModificationModel.getDirectTransX(),
                batteryModificationModel.getStepUpTransformerX(),
                battery.getExtension(BatteryShortCircuit.class),
                () -> battery.newExtension(BatteryShortCircuitAdder.class),
                subReportNode);
        PropertiesUtils.applyProperties(battery, subReportNode, batteryModificationModel.getProperties(), "network.modification.BatteryProperties");
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

    private void modifyBatteryVoltageLevelBusOrBusBarSectionAttributes(BatteryModificationModel modificationModel,
                                                                       Battery battery, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                battery, battery.getTerminal(),
                modificationModel.getVoltageLevelId(),
                modificationModel.getBusOrBusbarSectionId(),
                subReportNode
        );
    }

    private void modifyBatteryLimitsAttributes(BatteryModificationModel modificationModel,
                                               Battery battery, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = modifyBatteryActiveLimitsAttributes(modificationModel.getMaxP(), modificationModel.getMinP(), battery, subReportNode);
        modifyBatteryReactiveLimitsAttributes(modificationModel, battery, subReportNode, subReportNodeLimits);
    }

    private void modifyBatteryReactiveCapabilityCurvePoints(BatteryModificationModel modificationModel,
                                                            Battery battery, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        ReactiveCapabilityCurveAdder adder = battery.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurvePointsModel> modificationPoints = modificationModel.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = battery.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? battery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReportNode, subReportNodeLimits);
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

    private void modifyBatteryReactiveLimitsAttributes(BatteryModificationModel modificationModel,
                                                       Battery battery, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        if (modificationModel.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationModel.getReactiveCapabilityCurve().getValue()
                    && modificationModel.getReactiveCapabilityCurvePoints() != null
                    && !modificationModel.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyBatteryReactiveCapabilityCurvePoints(modificationModel, battery, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(modificationModel.getReactiveCapabilityCurve().getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(modificationModel.getMinQ(), modificationModel.getMaxQ(), battery, subReportNode, subReportNodeLimits);
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

    private ReportNode modifyBatteryConnectivityAttributes(BatteryModificationModel modificationModel,
                                                                 Battery battery, ReportNode subReportNode) {
        ConnectablePosition<Battery> connectablePosition = battery.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Battery> connectablePositionAdder = battery.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, battery, modificationModel, subReportNode);
    }
}

