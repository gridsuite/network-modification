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
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.modifications.BatteryModification.ERROR_MESSAGE;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Setter
@Getter
public class BatteryCreation extends AbstractInjectionCreation implements ReactiveLimitsHolderInfos {

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
    private boolean voltageRegulationOn;
    private Double targetV;
    private String regulatingTerminalId;
    private String regulatingTerminalType;
    private String regulatingTerminalVlId;

    @Builder
    public BatteryCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName,
                           String voltageLevelId, String busOrBusbarSectionId, String connectionName,
                           ConnectablePosition.Direction connectionDirection, Integer connectionPosition,
                           boolean terminalConnected, double minP, double maxP, Double minQ, Double maxQ,
                           List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints,
                           double targetP, Double targetQ, Boolean participate, Float droop, Double directTransX,
                           Double stepUpTransformerX, Boolean reactiveCapabilityCurve, Double targetV, boolean voltageRegulationOn,
                           String regulatingTerminalId, String regulatingTerminalType, String regulatingTerminalVlId) {
        super(equipmentId, properties, equipmentName, voltageLevelId, busOrBusbarSectionId, connectionName, connectionDirection, connectionPosition, terminalConnected);
        this.minP = minP;
        this.maxP = maxP;
        this.minQ = minQ;
        this.maxQ = maxQ;
        this.reactiveCapabilityCurvePoints = reactiveCapabilityCurvePoints;
        this.targetP = targetP;
        this.targetQ = targetQ;
        this.targetV = targetV;
        this.participate = participate;
        this.droop = droop;
        this.directTransX = directTransX;
        this.stepUpTransformerX = stepUpTransformerX;
        this.voltageRegulationOn = voltageRegulationOn;
        this.reactiveCapabilityCurve = reactiveCapabilityCurve;
        this.regulatingTerminalId = regulatingTerminalId;
        this.regulatingTerminalType = regulatingTerminalType;
        this.regulatingTerminalVlId = regulatingTerminalVlId;
    }

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

        // check regulated terminal
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId);
        ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                regulatingTerminalId,
                regulatingTerminalType,
                regulatingTerminalVlId);
        checkIsNotNegativeValue(errorMessage, targetV, CREATE_BATTERY_ERROR, "Target Voltage");

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
        createInjectionInNodeBreaker(voltageLevel, this, network, batteryAdder, subReportNode);
        var battery = ModificationUtils.getInstance().getBattery(network, equipmentId);
        addExtensionsToBattery(battery, voltageLevel, subReportNode);
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

        addExtensionsToBattery(battery, voltageLevel, subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.batteryCreated")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void addExtensionsToBattery(Battery battery, VoltageLevel voltageLevel, ReportNode subReportNode) {
        if (equipmentName != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, equipmentName, "Name");
        }
        reportInjectionCreationConnectivity(this, subReportNode);
        createBatteryVoltageRegulation(battery, voltageLevel, subReportNode);
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

    private void createBatteryVoltageRegulation(Battery battery, VoltageLevel voltageLevel, ReportNode subReportNode) {
        Terminal regulatingTerminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                regulatingTerminalId,
                regulatingTerminalType,
                regulatingTerminalVlId);
        List<ReportNode> voltageReports = new ArrayList<>();
        VoltageRegulationAdder voltageRegulationAdder = battery.newExtension(VoltageRegulationAdder.class)
                .withRegulatingTerminal(regulatingTerminal)
                .withVoltageRegulatorOn(voltageRegulationOn);
        if (targetV != null) {
            voltageRegulationAdder.withTargetV(targetV);
        }
        voltageRegulationAdder.add();
        voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                regulatingTerminalVlId,
                "Voltage level"));
        voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                regulatingTerminalType + ":" + regulatingTerminalId,
                "Equipment"));
        ModificationUtils.getInstance().reportModifications(subReportNode, voltageReports, "network.modification.VoltageRegulationCreated");
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
