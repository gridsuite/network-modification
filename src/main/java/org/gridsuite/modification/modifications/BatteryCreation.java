/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.BatteryCreationInfos;
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
public class BatteryCreation extends AbstractModification {

    private final BatteryCreationInfos modificationInfos;

    public BatteryCreation(BatteryCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getBattery(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(BATTERY_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }
        String errorMessage = "Battery '" + modificationInfos.getEquipmentId() + "' : ";

        // check connectivity
        ModificationUtils.getInstance()
                .controlConnectivity(network, modificationInfos.getVoltageLevelId(),
                modificationInfos.getBusOrBusbarSectionId(), modificationInfos.getConnectionPosition());

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(modificationInfos,
                modificationInfos.getErrorType(),
                modificationInfos.getEquipmentId(),
                "Battery");

        ModificationUtils.getInstance().checkActivePowerControl(modificationInfos.getParticipate(),
            modificationInfos.getDroop(), CREATE_BATTERY_ERROR, String.format(ERROR_MESSAGE, modificationInfos.getEquipmentId()));
        checkIsPercentage(errorMessage, modificationInfos.getDroop(), CREATE_BATTERY_ERROR, "Droop");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the battery in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createBatteryInNodeBreaker(voltageLevel, modificationInfos, network, subReportNode);
        } else {
            createBatteryInBusBreaker(voltageLevel, modificationInfos, subReportNode);
        }
        if (!modificationInfos.isTerminalConnected()) {
            network.getBattery(modificationInfos.getEquipmentId()).getTerminal().disconnect();
        }
        // properties
        Battery battery = network.getBattery(modificationInfos.getEquipmentId());
        PropertiesUtils.applyProperties(battery, subReportNode, modificationInfos.getProperties(), "network.modification.BatteryProperties");
    }

    @Override
    public String getName() {
        return "BatteryCreation";
    }

    private void createBatteryInNodeBreaker(VoltageLevel voltageLevel, BatteryCreationInfos batteryCreationInfos, Network network, ReportNode subReportNode) {
        BatteryAdder batteryAdder = createBatteryAdderInNodeBreaker(voltageLevel, batteryCreationInfos);
        createInjectionInNodeBreaker(voltageLevel, batteryCreationInfos, network, batteryAdder, subReportNode);
        var battery = ModificationUtils.getInstance().getBattery(network, batteryCreationInfos.getEquipmentId());
        addExtensionsToBattery(batteryCreationInfos, battery, subReportNode);
    }

    private BatteryAdder createBatteryAdderInNodeBreaker(VoltageLevel voltageLevel, BatteryCreationInfos batteryCreationInfos) {

        return voltageLevel.newBattery()
                .setId(batteryCreationInfos.getEquipmentId())
                .setName(batteryCreationInfos.getEquipmentName())
                .setMinP(batteryCreationInfos.getMinP())
                .setMaxP(batteryCreationInfos.getMaxP())
                .setTargetP(batteryCreationInfos.getTargetP())
                .setTargetQ(nanIfNull(batteryCreationInfos.getTargetQ()));
    }

    private void createBatteryInBusBreaker(VoltageLevel voltageLevel, BatteryCreationInfos batteryCreationInfos, ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, batteryCreationInfos.getBusOrBusbarSectionId());

        // creating the battery
        Battery battery = voltageLevel.newBattery()
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .setId(batteryCreationInfos.getEquipmentId())
                .setName(batteryCreationInfos.getEquipmentName())
                .setMinP(batteryCreationInfos.getMinP())
                .setMaxP(batteryCreationInfos.getMaxP())
                .setTargetP(batteryCreationInfos.getTargetP())
                .setTargetQ(nanIfNull(batteryCreationInfos.getTargetQ()))
                .add();

        addExtensionsToBattery(batteryCreationInfos, battery, subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.batteryCreated")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void addExtensionsToBattery(BatteryCreationInfos batteryCreationInfos, Battery battery, ReportNode subReportNode) {
        if (batteryCreationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, batteryCreationInfos.getEquipmentName(), "Name");
        }
        reportInjectionCreationConnectivity(batteryCreationInfos, subReportNode);
        ReportNode subReportNodeLimits = reportBatteryActiveLimits(batteryCreationInfos, subReportNode);
        ModificationUtils.getInstance().createReactiveLimits(batteryCreationInfos, battery, subReportNodeLimits);
        ReportNode subReportNodeSetpoints = reportBatterySetPoints(batteryCreationInfos, subReportNode);
        createBatteryActivePowerControl(batteryCreationInfos, battery, subReportNodeSetpoints);
    }

    private ReportNode reportBatterySetPoints(BatteryCreationInfos batteryCreationInfos, ReportNode subReportNode) {
        List<ReportNode> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationInfos.getTargetP(), "Active power"));
        if (batteryCreationInfos.getTargetQ() != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationInfos.getTargetQ(), "Reactive power"));
        }
        return ModificationUtils.getInstance().reportModifications(subReportNode, setPointReports, "network.modification.SetPointCreated");
    }

    private ReportNode reportBatteryActiveLimits(BatteryCreationInfos batteryCreationInfos, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
        List<ReportNode> limitsReports = new ArrayList<>();
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            batteryCreationInfos.getMinP(), "Min active power"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            batteryCreationInfos.getMaxP(), "Max active power"));
        ModificationUtils.getInstance().reportModifications(subReportNodeLimits, limitsReports, "network.modification.ActiveLimitsCreated");
        return subReportNodeLimits;
    }

    private void createBatteryActivePowerControl(BatteryCreationInfos batteryCreationInfos, Battery battery, ReportNode subReporter) {
        if (batteryCreationInfos.getParticipate() != null && batteryCreationInfos.getDroop() != null) {
            List<ReportNode> activePowerRegulationReports = new ArrayList<>();
            try {
                battery.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(batteryCreationInfos.getParticipate())
                        .withDroop(batteryCreationInfos.getDroop())
                        .add();
                activePowerRegulationReports.add(ModificationUtils.getInstance().buildCreationReport(
                        batteryCreationInfos.getParticipate(),
                        "Participate"));
                activePowerRegulationReports.add(ModificationUtils.getInstance().buildCreationReport(
                        batteryCreationInfos.getDroop(),
                        "Droop"));
            } catch (PowsyblException e) {
                activePowerRegulationReports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.activePowerExtensionAddError.battery")
                        .withUntypedValue("id", batteryCreationInfos.getEquipmentId())
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReporter, activePowerRegulationReports, "network.modification.ActivePowerRegulationCreated");
        }
    }
}
