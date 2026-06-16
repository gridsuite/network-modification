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
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.BatteryShortCircuitAdder;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.BATTERY_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_BATTERY_ERROR;
import static org.gridsuite.modification.modifications.BatteryModification.ERROR_MESSAGE;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@AllArgsConstructor
@Builder
public class BatteryCreation extends AbstractModification implements ReactiveLimitsHolderInfos {

    private String equipmentId;
    private List<FreePropertyInfos> properties;
    private String equipmentName;
    private String voltageLevelId;
    private String busOrBusbarSectionId;
    private String connectionName;
    private ConnectablePosition.Direction connectionDirection;
    private Integer connectionPosition;
    private boolean terminalConnected;
    private double minP;
    private double maxP;
    private Double minQ;
    private Double maxQ;
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;
    private double targetP;
    private Double targetQ;
    private Boolean participate;
    private Float droop;
    private Double directTransX;
    private Double stepUpTransformerX;
    private Boolean reactiveCapabilityCurve;

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getBattery(equipmentId) != null) {
            throw new NetworkModificationException(BATTERY_ALREADY_EXISTS, equipmentId);
        }
        String errorMessage = "Battery '" + equipmentId + "' : ";

        // check connectivity
        ModificationUtils.getInstance()
            .controlConnectivity(network, voltageLevelId, busOrBusbarSectionId);

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(this,
                CREATE_BATTERY_ERROR,
                equipmentId,
                "Battery");

        ModificationUtils.getInstance().checkActivePowerControl(participate,
            droop, CREATE_BATTERY_ERROR, String.format(ERROR_MESSAGE, equipmentId));
        checkIsPercentage(errorMessage, droop, CREATE_BATTERY_ERROR, "Droop");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the battery in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId);
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createBatteryInNodeBreaker(voltageLevel, network, subReportNode);
        } else {
            createBatteryInBusBreaker(voltageLevel, subReportNode);
        }
        if (!terminalConnected) {
            network.getBattery(equipmentId).getTerminal().disconnect();
        }
        // properties
        Battery battery = network.getBattery(equipmentId);
        PropertiesUtils.applyProperties(battery, subReportNode, properties, "network.modification.BatteryProperties");
    }

    @Override
    public String getName() {
        return "BatteryCreation";
    }

    private void createBatteryInNodeBreaker(VoltageLevel voltageLevel, Network network, ReportNode subReportNode) {
        BatteryAdder batteryAdder = createBatteryAdderInNodeBreaker(voltageLevel);
        createInjectionInNodeBreaker(voltageLevel, equipmentId, busOrBusbarSectionId, connectionName, connectionDirection, connectionPosition, network, batteryAdder, subReportNode);
        var battery = ModificationUtils.getInstance().getBattery(network, equipmentId);
        addExtensionsToBattery(battery, subReportNode);
    }

    private BatteryAdder createBatteryAdderInNodeBreaker(VoltageLevel voltageLevel) {

        return voltageLevel.newBattery()
                .setId(equipmentId)
                .setName(equipmentName)
                .setMinP(minP)
                .setMaxP(maxP)
                .setTargetP(targetP)
                .setTargetQ(nanIfNull(targetQ));
    }

    private void createBatteryInBusBreaker(VoltageLevel voltageLevel, ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, busOrBusbarSectionId);

        // creating the battery
        Battery battery = voltageLevel.newBattery()
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .setId(equipmentId)
                .setName(equipmentName)
                .setMinP(minP)
                .setMaxP(maxP)
                .setTargetP(targetP)
                .setTargetQ(nanIfNull(targetQ))
                .add();

        addExtensionsToBattery(battery, subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.batteryCreated")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void addExtensionsToBattery(Battery battery, ReportNode subReportNode) {
        if (equipmentName != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, equipmentName, "Name");
        }
        reportInjectionCreationConnectivity(voltageLevelId, busOrBusbarSectionId, connectionName, connectionDirection, connectionPosition, terminalConnected, equipmentId, subReportNode);
        ReportNode subReportNodeLimits = reportBatteryActiveLimits(subReportNode);
        ModificationUtils.getInstance().createReactiveLimits(this, battery, subReportNodeLimits);
        ReportNode subReportNodeSetpoints = reportBatterySetPoints(subReportNode);
        ModificationUtils.getInstance().createNewActivePowerControlForInjectionCreation(battery.newExtension(ActivePowerControlAdder.class),
                participate,
                droop,
                subReportNodeSetpoints);
        ModificationUtils.getInstance().createShortCircuitExtension(stepUpTransformerX,
                directTransX, equipmentId,
                battery.newExtension(BatteryShortCircuitAdder.class), subReportNode, "battery");
    }

    private ReportNode reportBatterySetPoints(ReportNode subReportNode) {
        List<ReportNode> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(targetP, "Active power"));
        if (targetQ != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(targetQ, "Reactive power"));
        }
        return ModificationUtils.getInstance().reportModifications(subReportNode, setPointReports, "network.modification.SetPointCreated");
    }

    private ReportNode reportBatteryActiveLimits(ReportNode subReportNode) {
        ReportNode subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
        List<ReportNode> limitsReports = new ArrayList<>();
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            minP, "Min active power"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            maxP, "Max active power"));
        ModificationUtils.getInstance().reportModifications(subReportNodeLimits, limitsReports, "network.modification.ActiveLimitsCreated");
        return subReportNodeLimits;
    }
}
