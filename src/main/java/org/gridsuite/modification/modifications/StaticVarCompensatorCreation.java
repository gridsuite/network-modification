/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.StandbyAutomatonAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.StaticVarCompensatorCreationModel;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_STATIC_VAR_COMPENSATOR_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.STATIC_VAR_COMPENSATOR_ALREADY_EXISTS;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class StaticVarCompensatorCreation extends AbstractModification {

    private final StaticVarCompensatorCreationModel modificationModel;

    public StaticVarCompensatorCreation(StaticVarCompensatorCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getStaticVarCompensator(modificationModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(STATIC_VAR_COMPENSATOR_ALREADY_EXISTS, modificationModel.getEquipmentId());
        }
        String errorMessage = "Static var compensator '" + modificationModel.getEquipmentId() + "' : ";

        // check connectivity
        ModificationUtils.getInstance()
            .controlConnectivity(network, modificationModel.getVoltageLevelId(),
                modificationModel.getBusOrBusbarSectionId());

        // check reactive power limits and set points
        ModificationUtils.getInstance().checkReactivePowerLimitsAndSetPointsCreation(modificationModel);

        // check regulated terminal
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId());
        ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(), modificationModel.getRegulatingTerminalId(),
            modificationModel.getRegulatingTerminalType(), modificationModel.getRegulatingTerminalVlId());

        // check standby automaton
        ModificationUtils.getInstance().checkStandbyAutomatonCreation(modificationModel);
        checkIsNotNegativeValue(errorMessage, modificationModel.getVoltageSetpoint(), CREATE_STATIC_VAR_COMPENSATOR_ERROR, "voltage set point");
        checkIsNotNegativeValue(errorMessage, modificationModel.getHighVoltageSetpoint(), CREATE_STATIC_VAR_COMPENSATOR_ERROR, "high voltage set point");
        checkIsNotNegativeValue(errorMessage, modificationModel.getLowVoltageSetpoint(), CREATE_STATIC_VAR_COMPENSATOR_ERROR, "low voltage set point");
        checkIsNotNegativeValue(errorMessage, modificationModel.getHighVoltageThreshold(), CREATE_STATIC_VAR_COMPENSATOR_ERROR, "high voltage threshold");
        checkIsNotNegativeValue(errorMessage, modificationModel.getLowVoltageThreshold(), CREATE_STATIC_VAR_COMPENSATOR_ERROR, "low voltage threshold");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the static var compensator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createStaticVarCompensatorInNodeBreaker(voltageLevel, modificationModel, network, subReportNode);
        } else {
            createStaticVarCompensatorInBusBreaker(voltageLevel, modificationModel, subReportNode);
        }
        ModificationUtils.getInstance().disconnectCreatedInjection(modificationModel, network.getStaticVarCompensator(modificationModel.getEquipmentId()), subReportNode);
        // properties
        StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(modificationModel.getEquipmentId());
        PropertiesUtils.applyProperties(staticVarCompensator, subReportNode, modificationModel.getProperties(), "network.modification.StaticVarCompensatorProperties");
        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.staticVarCompensatorCreated")
            .withUntypedValue("id", modificationModel.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
    }

    @Override
    public String getName() {
        return "StaticVarCompensatorCreation";
    }

    private void createStaticVarCompensatorInNodeBreaker(VoltageLevel voltageLevel, StaticVarCompensatorCreationModel staticVarCompensatorCreationModel,
                                                         Network network, ReportNode subReportNode) {
        StaticVarCompensatorAdder staticVarCompensatorAdder = createStaticVarCompensatorAdderInNodeBreaker(voltageLevel, staticVarCompensatorCreationModel);
        createInjectionInNodeBreaker(voltageLevel, staticVarCompensatorCreationModel, network, staticVarCompensatorAdder, subReportNode);
        var staticVarCompensator = ModificationUtils.getInstance().getStaticVarCompensator(network, staticVarCompensatorCreationModel.getEquipmentId());
        addExtensionsToStaticVarCompensator(staticVarCompensatorCreationModel, staticVarCompensator, voltageLevel, subReportNode);
    }

    private StaticVarCompensatorAdder createStaticVarCompensatorAdderInNodeBreaker(VoltageLevel voltageLevel, StaticVarCompensatorCreationModel staticVarCompensatorCreationModel) {
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
            staticVarCompensatorCreationModel.getRegulatingTerminalId(),
            staticVarCompensatorCreationModel.getRegulatingTerminalType(),
            staticVarCompensatorCreationModel.getRegulatingTerminalVlId());
        double bMax = Objects.isNull(staticVarCompensatorCreationModel.getMaxSusceptance()) && Objects.nonNull(staticVarCompensatorCreationModel.getMaxQAtNominalV()) ?
            (staticVarCompensatorCreationModel.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2) : staticVarCompensatorCreationModel.getMaxSusceptance();
        double bMin = Objects.isNull(staticVarCompensatorCreationModel.getMinSusceptance()) && Objects.nonNull(staticVarCompensatorCreationModel.getMinQAtNominalV()) ?
            (staticVarCompensatorCreationModel.getMinQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2) : staticVarCompensatorCreationModel.getMinSusceptance();
        StaticVarCompensatorAdder staticVarCompensatorAdder = voltageLevel.newStaticVarCompensator()
            .setId(staticVarCompensatorCreationModel.getEquipmentId())
            .setName(staticVarCompensatorCreationModel.getEquipmentName())
            .setBmax(bMax)
            .setBmin(bMin)
            .setVoltageSetpoint(nanIfNull(staticVarCompensatorCreationModel.getVoltageSetpoint()))
            .setReactivePowerSetpoint(nanIfNull(staticVarCompensatorCreationModel.getReactivePowerSetpoint()))
            .setRegulating(staticVarCompensatorCreationModel.isRegulating());
        if (staticVarCompensatorCreationModel.isRegulating()) {
            staticVarCompensatorAdder.setRegulationMode(staticVarCompensatorCreationModel.getRegulationMode());
        }

        if (terminal != null) {
            staticVarCompensatorAdder.setRegulatingTerminal(terminal);
        }

        return staticVarCompensatorAdder;
    }

    private void addExtensionsToStaticVarCompensator(StaticVarCompensatorCreationModel staticVarCompensatorCreationModel,
                                                     StaticVarCompensator staticVarCompensator,
                                                     VoltageLevel voltageLevel,
                                                     ReportNode subReportNode) {
        if (staticVarCompensatorCreationModel.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, staticVarCompensatorCreationModel.getEquipmentName(), "Name");
        }

        reportInjectionCreationConnectivity(staticVarCompensatorCreationModel, subReportNode);
        reportStaticVarCompensatorLimitsAndSetpoints(staticVarCompensatorCreationModel, staticVarCompensator, voltageLevel, subReportNode);
        reportStaticVarCompensatorStandbyAutomaton(staticVarCompensatorCreationModel, staticVarCompensator, voltageLevel, subReportNode);
    }

    private void reportStaticVarCompensatorStandbyAutomaton(StaticVarCompensatorCreationModel staticVarCompensatorCreationModel,
                                                            StaticVarCompensator staticVarCompensator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        if (Boolean.TRUE.equals(staticVarCompensatorCreationModel.isStandbyAutomatonOn())) {
            List<ReportNode> standbyAutomatonReports = new ArrayList<>();
            double b0 = Objects.isNull(staticVarCompensatorCreationModel.getB0()) && Objects.nonNull(staticVarCompensatorCreationModel.getQ0()) ?
                (staticVarCompensatorCreationModel.getQ0()) / Math.pow(voltageLevel.getNominalV(), 2) : staticVarCompensatorCreationModel.getB0();
            try {
                staticVarCompensator.newExtension(StandbyAutomatonAdder.class)
                    .withStandbyStatus(staticVarCompensatorCreationModel.isStandby())
                    .withB0(b0)
                    .withLowVoltageSetpoint(staticVarCompensatorCreationModel.getLowVoltageSetpoint())
                    .withHighVoltageSetpoint(staticVarCompensatorCreationModel.getHighVoltageSetpoint())
                    .withLowVoltageThreshold(staticVarCompensatorCreationModel.getLowVoltageThreshold())
                    .withHighVoltageThreshold(staticVarCompensatorCreationModel.getHighVoltageThreshold())
                    .add();
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                    staticVarCompensatorCreationModel.isStandby(),
                    "Standby"));
                if (Objects.nonNull(staticVarCompensatorCreationModel.getB0())) {
                    standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationModel.getB0(),
                        "Fixed part of susceptance"));
                }
                if (Objects.nonNull(staticVarCompensatorCreationModel.getQ0())) {
                    standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationModel.getQ0(),
                        "Fixed part of Q at nominal voltage"));
                }
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                    staticVarCompensatorCreationModel.getLowVoltageSetpoint(),
                    "Low voltage set point"));
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                    staticVarCompensatorCreationModel.getHighVoltageSetpoint(),
                    "High voltage set point"));
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                    staticVarCompensatorCreationModel.getHighVoltageThreshold(),
                    "High voltage threshold"));
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                    staticVarCompensatorCreationModel.getLowVoltageThreshold(),
                    "Low voltage threshold"));
            } catch (PowsyblException e) {
                standbyAutomatonReports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.StandbyAutomatonExtensionAddError")
                    .withUntypedValue("message", e.getMessage())
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .build());
            }
            ModificationUtils.getInstance().reportModifications(subReportNode, standbyAutomatonReports,
                "network.modification.StandbyAutomatonCreated");
        }
    }

    private void createStaticVarCompensatorInBusBreaker(VoltageLevel voltageLevel, StaticVarCompensatorCreationModel staticVarCompensatorCreationModel,
                                                        ReportNode subReportNode) {

        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, staticVarCompensatorCreationModel.getBusOrBusbarSectionId());
        double bMax = Objects.isNull(staticVarCompensatorCreationModel.getMaxSusceptance()) && Objects.nonNull(staticVarCompensatorCreationModel.getMaxQAtNominalV()) ?
            (staticVarCompensatorCreationModel.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2) : staticVarCompensatorCreationModel.getMaxSusceptance();
        double bMin = Objects.isNull(staticVarCompensatorCreationModel.getMinSusceptance()) && Objects.nonNull(staticVarCompensatorCreationModel.getMinQAtNominalV()) ?
            (staticVarCompensatorCreationModel.getMinQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2) : staticVarCompensatorCreationModel.getMinSusceptance();
        /* creating the static var compensator */
        StaticVarCompensatorAdder staticVarCompensatorAdder = voltageLevel.newStaticVarCompensator()
            .setId(staticVarCompensatorCreationModel.getEquipmentId())
            .setName(staticVarCompensatorCreationModel.getEquipmentName())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setBmax(bMax)
            .setBmin(bMin)
            .setVoltageSetpoint(staticVarCompensatorCreationModel.getVoltageSetpoint())
            .setReactivePowerSetpoint(staticVarCompensatorCreationModel.getReactivePowerSetpoint())
            .setRegulating(staticVarCompensatorCreationModel.isRegulating());
        if (staticVarCompensatorCreationModel.isRegulating()) {
            staticVarCompensatorAdder.setRegulationMode(staticVarCompensatorCreationModel.getRegulationMode());
        }
        StaticVarCompensator staticVarCompensator = staticVarCompensatorAdder.add();

        addExtensionsToStaticVarCompensator(staticVarCompensatorCreationModel, staticVarCompensator, voltageLevel, subReportNode);
    }

    private void reportStaticVarCompensatorLimitsAndSetpoints(StaticVarCompensatorCreationModel staticVarCompensatorCreationModel,
                                                              StaticVarCompensator staticVarCompensator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        List<ReportNode> voltageReports = new ArrayList<>();
        if (Objects.nonNull(staticVarCompensatorCreationModel.getMinSusceptance())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationModel.getMinSusceptance(), "Susceptance min"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationModel.getMaxSusceptance())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationModel.getMaxSusceptance(), "Susceptance max"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationModel.getMinQAtNominalV())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationModel.getMinQAtNominalV(), "Q min at nominal voltage"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationModel.getMaxQAtNominalV())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationModel.getMaxQAtNominalV(), "Q max at nominal voltage"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationModel.getRegulationMode())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationModel.getRegulationMode(), "regulation mode"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationModel.getVoltageSetpoint())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationModel.getVoltageSetpoint(), "Voltage set point"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationModel.getReactivePowerSetpoint())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationModel.getReactivePowerSetpoint(), "Reactive power set point"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationModel.getVoltageRegulationType())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationModel.getVoltageRegulationType(), "Voltage Regulation type"));
        }
        if (staticVarCompensatorCreationModel.getRegulatingTerminalVlId() != null && staticVarCompensatorCreationModel.getRegulatingTerminalId() != null &&
            staticVarCompensatorCreationModel.getRegulatingTerminalType() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                staticVarCompensatorCreationModel.getRegulatingTerminalId(),
                staticVarCompensatorCreationModel.getRegulatingTerminalType(),
                staticVarCompensatorCreationModel.getRegulatingTerminalVlId());
            if (terminal != null) {
                updateCompensatorRegulatingTerminal(staticVarCompensatorCreationModel, staticVarCompensator, terminal, voltageReports);
            }
        }
        ModificationUtils.getInstance().reportModifications(subReportNode, voltageReports, "network.modification.LimitsAndSetpointsCreated");
    }

    private void updateCompensatorRegulatingTerminal(StaticVarCompensatorCreationModel staticVarCompensatorCreationModel, StaticVarCompensator staticVarCompensator,
                                                     Terminal terminal, List<ReportNode> voltageReports) {
        if (staticVarCompensatorCreationModel.getRegulatingTerminalId() != null
            && staticVarCompensatorCreationModel.getRegulatingTerminalType() != null
            && staticVarCompensatorCreationModel.getRegulatingTerminalVlId() != null) {
            staticVarCompensator.setRegulatingTerminal(terminal);
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                staticVarCompensatorCreationModel.getRegulatingTerminalVlId(),
                "Voltage level"));
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                staticVarCompensatorCreationModel.getRegulatingTerminalType() + ":"
                    + staticVarCompensatorCreationModel.getRegulatingTerminalId(),
                "Equipment"));
        }
    }

}
