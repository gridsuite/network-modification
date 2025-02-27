/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.RemoveFeederBay;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import com.powsybl.network.store.iidm.impl.MinMaxReactiveLimitsImpl;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.BatteryCreationInfos;
import org.gridsuite.modification.dto.BatteryModificationInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_BATTERY_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;
import static org.gridsuite.modification.utils.ModificationUtils.nanIfNull;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class BatteryModification extends AbstractModification {

    private final BatteryModificationInfos modificationInfos;

    private static final String LIMITS = "Limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String SETPOINTS = "Setpoints";
    public static final String ERROR_MESSAGE = "Battery '%s' : ";

    public BatteryModification(BatteryModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(MODIFY_BATTERY_ERROR, "Missing required attributes to modify the equipment");
        }
        Battery battery = ModificationUtils.getInstance().getBattery(network, modificationInfos.getEquipmentId());
        String errorMessage = "Battery '" + modificationInfos.getEquipmentId() + "' : ";
        // check voltageLevel
        ModificationUtils.getInstance().checkVoltageLevelInjectionModification(network, modificationInfos, battery);
        ModificationUtils.getInstance().checkReactiveLimit(battery, modificationInfos.getMinQ(), modificationInfos.getMaxQ(),
                modificationInfos.getReactiveCapabilityCurvePoints(), MODIFY_BATTERY_ERROR, errorMessage);
        checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(modificationInfos, battery, MODIFY_BATTERY_ERROR, errorMessage);
    }

    private void checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(BatteryModificationInfos modificationInfos, Battery battery, NetworkModificationException.Type exceptionType, String errorMessage) {
        ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                modificationInfos.getTargetP(),
                modificationInfos.getMinP(),
                modificationInfos.getMaxP(),
                battery.getMinP(),
                battery.getMaxP(),
                battery.getTargetP(),
                exceptionType,
                errorMessage
        );
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Battery battery = ModificationUtils.getInstance().getBattery(network, modificationInfos.getEquipmentId());
        // modify the battery in the network
        modifyBattery(battery, modificationInfos, subReportNode);
    }

    @Override
    public String getName() {
        return "BatteryModification";
    }

    private void modifyBattery(Battery battery, BatteryModificationInfos modificationInfos, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("batteryModification", "Battery with id=${id} modified :")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(battery::setName, () -> battery.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");
        }
        modifyBatteryVoltageLevelBusOrBusBarSectionAttributes(modificationInfos, battery, subReportNode);
        modifyBatteryLimitsAttributes(modificationInfos, battery, subReportNode);
        modifyBatterySetpointsAttributes(
                modificationInfos.getTargetP(), modificationInfos.getTargetQ(),
                modificationInfos.getParticipate(), modificationInfos.getDroop(),
                battery, subReportNode);
        modifyBatteryConnectivityAttributes(modificationInfos, battery, subReportNode);
        PropertiesUtils.applyProperties(battery, subReportNode, modificationInfos.getProperties(), "BatteryProperties");
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
            subReporterSetpoints = subReportNode.newReportNode().withMessageTemplate(SETPOINTS, SETPOINTS).add();
            if (reportActivePower != null) {
                insertReportNode(subReporterSetpoints, reportActivePower);
            }
            if (reportReactivePower != null) {
                insertReportNode(subReporterSetpoints, reportReactivePower);
            }
        }
        modifyBatteryActivePowerControlAttributes(participate, droop, battery, subReportNode, subReporterSetpoints);
    }

    private void modifyBatteryLimitsAttributes(BatteryModificationInfos modificationInfos,
                                               Battery battery, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = modifyBatteryActiveLimitsAttributes(modificationInfos.getMaxP(), modificationInfos.getMinP(), battery, subReportNode);
        modifyBatteryReactiveLimitsAttributes(modificationInfos, battery, subReportNode, subReportNodeLimits);
    }

    private void modifyBatteryReactiveCapabilityCurvePoints(BatteryModificationInfos modificationInfos,
                                                            Battery battery, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        ReactiveCapabilityCurveAdder adder = battery.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurvePointsInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
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
            subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate(LIMITS, LIMITS).add();
            ReportNode subReporterActiveLimits = subReportNodeLimits.newReportNode().withMessageTemplate(ACTIVE_LIMITS, ACTIVE_LIMITS).add();
            if (reportMaxActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMaxActivePower);
            }
            if (reportMinActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMinActivePower);
            }
        }
        return subReportNodeLimits;
    }

    private void modifyBatteryReactiveLimitsAttributes(BatteryModificationInfos modificationInfos,
                                                       Battery battery, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        if (modificationInfos.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                    && modificationInfos.getReactiveCapabilityCurvePoints() != null
                    && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyBatteryReactiveCapabilityCurvePoints(modificationInfos, battery, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(modificationInfos.getMinQ(), modificationInfos.getMaxQ(), battery, subReportNode, subReportNodeLimits);
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

    private void modifyBatteryVoltageLevelBusOrBusBarSectionAttributes(BatteryModificationInfos modificationInfos,
                                                                         Battery battery, ReportNode subReportNode) {
        if (ModificationUtils.getInstance().isNotModificationVoltageLevelBusOrBusBarInfos(modificationInfos)) {
            return;
        }
        Network network = battery.getNetwork();
        Map<String, String> properties = !battery.hasProperty()
                ? null
                : battery.getPropertyNames().stream().collect(Collectors.toMap(name -> name, battery::getProperty));
        BatteryCreationInfos batteryCreationInfos = createBatteryCreationInfos(modificationInfos, battery, subReportNode);
        new RemoveFeederBay(battery.getId()).apply(network, true, subReportNode);
        createBattery(batteryCreationInfos, subReportNode, network);
        var newBattery = ModificationUtils.getInstance().getBattery(network, modificationInfos.getEquipmentId());
        if (properties != null) {
            properties.forEach(newBattery::setProperty);
        }
    }

    private BatteryCreationInfos createBatteryCreationInfos(BatteryModificationInfos modificationInfos, Battery battery, ReportNode subReportNode) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevelInfos(modificationInfos.getVoltageLevelId(), battery.getTerminal(),
                battery.getNetwork(), ModificationUtils.FeederSide.INJECTION_SINGLE_SIDE, subReportNode);
        String busOrBusbarSectionId = ModificationUtils.getInstance().getBusOrBusBarSectionInfos(modificationInfos.getBusOrBusbarSectionId(),
                battery.getTerminal(), ModificationUtils.FeederSide.INJECTION_SINGLE_SIDE, subReportNode);
        BatteryCreationInfos batteryCreationInfos = BatteryCreationInfos.builder().equipmentId(battery.getId())
                .equipmentName(battery.getNameOrId())
                .voltageLevelId(voltageLevel.getId())
                .busOrBusbarSectionId(busOrBusbarSectionId)
                .connectionName(battery.getExtension(ConnectablePosition.class) != null && battery.getExtension(ConnectablePosition.class).getFeeder() != null ?
                        battery.getExtension(ConnectablePosition.class).getFeeder().getName().orElse(modificationInfos.getEquipmentId()) : modificationInfos.getEquipmentId())
                .connectionDirection(battery.getExtension(ConnectablePosition.class) != null && battery.getExtension(ConnectablePosition.class).getFeeder() != null ?
                        battery.getExtension(ConnectablePosition.class).getFeeder().getDirection() : ConnectablePosition.Direction.UNDEFINED)
                .connectionPosition(null)
                .terminalConnected(battery.getTerminal().isConnected())
                .minP(battery.getMinP())
                .maxP(battery.getMaxP())
                .targetP(battery.getTargetP())
                .targetQ(battery.getTargetQ())
                .build();
        var activePowerControl = battery.getExtension(ActivePowerControl.class);
        if (activePowerControl != null) {
            batteryCreationInfos.setParticipate(activePowerControl.isParticipate());
            batteryCreationInfos.setDroop((float) activePowerControl.getDroop());
        }
        var reactiveLimits = battery.getReactiveLimits();
        if (reactiveLimits != null) {
            ReactiveLimitsKind limitsKind = reactiveLimits.getKind();
            if (limitsKind == ReactiveLimitsKind.MIN_MAX) {
                MinMaxReactiveLimits minMaxReactiveLimits = battery.getReactiveLimits(MinMaxReactiveLimitsImpl.class);
                batteryCreationInfos.setMaxQ(minMaxReactiveLimits.getMaxQ());
                batteryCreationInfos.setMinQ(minMaxReactiveLimits.getMinQ());
            } else if (limitsKind == ReactiveLimitsKind.CURVE) {
                ReactiveCapabilityCurve capabilityCurve = battery.getReactiveLimits(ReactiveCapabilityCurve.class);
                batteryCreationInfos.setReactiveCapabilityCurvePoints(capabilityCurve.getPoints().stream().map(point ->
                        new ReactiveCapabilityCurvePointsInfos(point.getMinQ(), point.getMaxQ(), point.getP())).toList());
            }
        }
        return batteryCreationInfos;
    }

    private void createBattery(BatteryCreationInfos modificationInfos, ReportNode subReportNode, Network network) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            BatteryAdder batteryAdder = ModificationUtils.getInstance().createBatteryAdderInNodeBreaker(voltageLevel, modificationInfos);
            ModificationUtils.getInstance().createInjectionInNodeBreaker(voltageLevel, modificationInfos.getBusOrBusbarSectionId(), modificationInfos.getConnectionPosition(),
                    modificationInfos.getConnectionDirection(), modificationInfos.getConnectionName() != null ?
                            modificationInfos.getConnectionName() : modificationInfos.getEquipmentId(),
                    network, batteryAdder, subReportNode);
        } else {
            Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, modificationInfos.getBusOrBusbarSectionId());
            voltageLevel.newBattery()
                    .setBus(bus.getId())
                    .setConnectableBus(bus.getId())
                    .setId(modificationInfos.getEquipmentId())
                    .setName(modificationInfos.getEquipmentName())
                    .setMinP(modificationInfos.getMinP())
                    .setMaxP(modificationInfos.getMaxP())
                    .setTargetP(modificationInfos.getTargetP())
                    .setTargetQ(nanIfNull(modificationInfos.getTargetQ()))
                    .add();
        }
        var battery = ModificationUtils.getInstance().getBattery(network, modificationInfos.getEquipmentId());
        ModificationUtils.getInstance().createReactiveLimits(modificationInfos, battery);
        createActivePowerControl(modificationInfos, battery);
    }

    private void createActivePowerControl(BatteryCreationInfos batteryCreationInfos, Battery battery) {
        if (batteryCreationInfos.getParticipate() != null && batteryCreationInfos.getDroop() != null) {
            battery.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(batteryCreationInfos.getParticipate())
                        .withDroop(batteryCreationInfos.getDroop())
                        .add();
        }
    }

    private ReportNode modifyBatteryConnectivityAttributes(BatteryModificationInfos modificationInfos,
                                                                 Battery battery, ReportNode subReportNode) {
        ConnectablePosition<Battery> connectablePosition = battery.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Battery> connectablePositionAdder = battery.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, battery, modificationInfos, subReportNode);
    }
}

