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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.BatteryCreationModel;
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

    private final BatteryCreationModel modificationModel;

    public BatteryCreation(BatteryCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getBattery(modificationModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(BATTERY_ALREADY_EXISTS, modificationModel.getEquipmentId());
        }
        String errorMessage = "Battery '" + modificationModel.getEquipmentId() + "' : ";

        // check connectivity
        ModificationUtils.getInstance()
                .controlConnectivity(network, modificationModel.getVoltageLevelId(),
                modificationModel.getBusOrBusbarSectionId());

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(modificationModel,
                modificationModel.getErrorType(),
                modificationModel.getEquipmentId(),
                "Battery");

        ModificationUtils.getInstance().checkActivePowerControl(modificationModel.getParticipate(),
            modificationModel.getDroop(), CREATE_BATTERY_ERROR, String.format(ERROR_MESSAGE, modificationModel.getEquipmentId()));
        checkIsPercentage(errorMessage, modificationModel.getDroop(), CREATE_BATTERY_ERROR, "Droop");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the battery in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createBatteryInNodeBreaker(voltageLevel, modificationModel, network, subReportNode);
        } else {
            createBatteryInBusBreaker(voltageLevel, modificationModel, subReportNode);
        }
        if (!modificationModel.isTerminalConnected()) {
            network.getBattery(modificationModel.getEquipmentId()).getTerminal().disconnect();
        }
        // properties
        Battery battery = network.getBattery(modificationModel.getEquipmentId());
        PropertiesUtils.applyProperties(battery, subReportNode, modificationModel.getProperties(), "network.modification.BatteryProperties");
    }

    @Override
    public String getName() {
        return "BatteryCreation";
    }

    private void createBatteryInNodeBreaker(VoltageLevel voltageLevel, BatteryCreationModel batteryCreationModel, Network network, ReportNode subReportNode) {
        BatteryAdder batteryAdder = createBatteryAdderInNodeBreaker(voltageLevel, batteryCreationModel);
        createInjectionInNodeBreaker(voltageLevel, batteryCreationModel, network, batteryAdder, subReportNode);
        var battery = ModificationUtils.getInstance().getBattery(network, batteryCreationModel.getEquipmentId());
        addExtensionsToBattery(batteryCreationModel, battery, subReportNode);
    }

    private BatteryAdder createBatteryAdderInNodeBreaker(VoltageLevel voltageLevel, BatteryCreationModel batteryCreationModel) {

        return voltageLevel.newBattery()
                .setId(batteryCreationModel.getEquipmentId())
                .setName(batteryCreationModel.getEquipmentName())
                .setMinP(batteryCreationModel.getMinP())
                .setMaxP(batteryCreationModel.getMaxP())
                .setTargetP(batteryCreationModel.getTargetP())
                .setTargetQ(nanIfNull(batteryCreationModel.getTargetQ()));
    }

    private void createBatteryInBusBreaker(VoltageLevel voltageLevel, BatteryCreationModel batteryCreationModel, ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, batteryCreationModel.getBusOrBusbarSectionId());

        // creating the battery
        Battery battery = voltageLevel.newBattery()
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .setId(batteryCreationModel.getEquipmentId())
                .setName(batteryCreationModel.getEquipmentName())
                .setMinP(batteryCreationModel.getMinP())
                .setMaxP(batteryCreationModel.getMaxP())
                .setTargetP(batteryCreationModel.getTargetP())
                .setTargetQ(nanIfNull(batteryCreationModel.getTargetQ()))
                .add();

        addExtensionsToBattery(batteryCreationModel, battery, subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.batteryCreated")
                .withUntypedValue("id", modificationModel.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void addExtensionsToBattery(BatteryCreationModel batteryCreationModel, Battery battery, ReportNode subReportNode) {
        if (batteryCreationModel.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, batteryCreationModel.getEquipmentName(), "Name");
        }
        reportInjectionCreationConnectivity(batteryCreationModel, subReportNode);
        ReportNode subReportNodeLimits = reportBatteryActiveLimits(batteryCreationModel, subReportNode);
        ModificationUtils.getInstance().createReactiveLimits(batteryCreationModel, battery, subReportNodeLimits);
        ReportNode subReportNodeSetpoints = reportBatterySetPoints(batteryCreationModel, subReportNode);
        ModificationUtils.getInstance().createNewActivePowerControlForInjectionCreation(battery.newExtension(ActivePowerControlAdder.class),
                batteryCreationModel.getParticipate(),
                batteryCreationModel.getDroop(),
                subReportNodeSetpoints);
        ModificationUtils.getInstance().createShortCircuitExtension(batteryCreationModel.getStepUpTransformerX(),
                batteryCreationModel.getDirectTransX(), batteryCreationModel.getEquipmentId(),
                battery.newExtension(BatteryShortCircuitAdder.class), subReportNode, "battery");
    }

    private ReportNode reportBatterySetPoints(BatteryCreationModel batteryCreationModel, ReportNode subReportNode) {
        List<ReportNode> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationModel.getTargetP(), "Active power"));
        if (batteryCreationModel.getTargetQ() != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationModel.getTargetQ(), "Reactive power"));
        }
        return ModificationUtils.getInstance().reportModifications(subReportNode, setPointReports, "network.modification.SetPointCreated");
    }

    private ReportNode reportBatteryActiveLimits(BatteryCreationModel batteryCreationModel, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
        List<ReportNode> limitsReports = new ArrayList<>();
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            batteryCreationModel.getMinP(), "Min active power"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            batteryCreationModel.getMaxP(), "Max active power"));
        ModificationUtils.getInstance().reportModifications(subReportNodeLimits, limitsReports, "network.modification.ActiveLimitsCreated");
        return subReportNodeLimits;
    }
}
