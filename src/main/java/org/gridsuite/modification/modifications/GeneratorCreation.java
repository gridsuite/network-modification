/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
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
import com.powsybl.iidm.network.extensions.CoordinatedReactiveControlAdder;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuitAdder;
import com.powsybl.iidm.network.extensions.GeneratorStartupAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.GeneratorCreationModel;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_GENERATOR_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.GENERATOR_ALREADY_EXISTS;
import static org.gridsuite.modification.modifications.GeneratorModification.ERROR_MESSAGE;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class GeneratorCreation extends AbstractModification {

    private final GeneratorCreationModel modificationModel;

    public GeneratorCreation(GeneratorCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getGenerator(modificationModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(GENERATOR_ALREADY_EXISTS, modificationModel.getEquipmentId());
        }
        String errorMessage = "Generator '" + modificationModel.getEquipmentId() + "' : ";

        // check connectivity
        ModificationUtils.getInstance().controlConnectivity(network, modificationModel.getVoltageLevelId(),
            modificationModel.getBusOrBusbarSectionId());

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(modificationModel,
            modificationModel.getErrorType(),
            modificationModel.getEquipmentId(),
            "Generator");

        // check regulated terminal
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId());
        ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
            modificationModel.getRegulatingTerminalId(),
            modificationModel.getRegulatingTerminalType(),
            modificationModel.getRegulatingTerminalVlId());

        ModificationUtils.getInstance().checkActivePowerControl(modificationModel.getParticipate(),
            modificationModel.getDroop(), CREATE_GENERATOR_ERROR, String.format(ERROR_MESSAGE, modificationModel.getEquipmentId()));

        checkIsNotNegativeValue(errorMessage, modificationModel.getTargetV(), CREATE_GENERATOR_ERROR, "Target Voltage");
        checkIsPercentage(errorMessage, modificationModel.getDroop(), CREATE_GENERATOR_ERROR, "Droop");
        checkIsNotNegativeValue(errorMessage, modificationModel.getRatedS(), CREATE_GENERATOR_ERROR, "Rated apparent power");
        checkPowerValues(errorMessage, modificationModel.getMinP(), modificationModel.getMaxP(), modificationModel.getTargetP(),
            modificationModel.getPlannedActivePowerSetPoint(), CREATE_GENERATOR_ERROR);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the generator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createGeneratorInNodeBreaker(voltageLevel, modificationModel, network, subReportNode);
        } else {
            createGeneratorInBusBreaker(voltageLevel, modificationModel, subReportNode);
        }
        if (!modificationModel.isTerminalConnected()) {
            network.getGenerator(modificationModel.getEquipmentId()).getTerminal().disconnect();
        }
        // apply the properties
        Generator generator = network.getGenerator(modificationModel.getEquipmentId());
        PropertiesUtils.applyProperties(generator, subReportNode, modificationModel.getProperties(), "network.modification.GeneratorProperties");
    }

    @Override
    public String getName() {
        return "GeneratorCreation";
    }

    private void createGeneratorInNodeBreaker(VoltageLevel voltageLevel, GeneratorCreationModel generatorCreationModel, Network network, ReportNode subReportNode) {
        GeneratorAdder generatorAdder = createGeneratorAdderInNodeBreaker(voltageLevel, generatorCreationModel);
        createInjectionInNodeBreaker(voltageLevel, generatorCreationModel, network, generatorAdder, subReportNode);

        // CreateFeederBayBuilder already create the generator using
        // (withInjectionAdder(generatorAdder)) so then we can add the additional informations and extensions
        var generator = ModificationUtils.getInstance().getGenerator(network, generatorCreationModel.getEquipmentId());
        addExtensionsToGenerator(generatorCreationModel, generator, voltageLevel, subReportNode);
    }

    private GeneratorAdder createGeneratorAdderInNodeBreaker(VoltageLevel voltageLevel, GeneratorCreationModel generatorCreationModel) {
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
            generatorCreationModel.getRegulatingTerminalId(),
            generatorCreationModel.getRegulatingTerminalType(),
            generatorCreationModel.getRegulatingTerminalVlId());

        // creating the generator adder
        GeneratorAdder generatorAdder = voltageLevel.newGenerator()
            .setId(generatorCreationModel.getEquipmentId())
            .setName(generatorCreationModel.getEquipmentName())
            .setEnergySource(generatorCreationModel.getEnergySource())
            .setMinP(generatorCreationModel.getMinP())
            .setMaxP(generatorCreationModel.getMaxP())
            .setRatedS(nanIfNull(generatorCreationModel.getRatedS()))
            .setTargetP(generatorCreationModel.getTargetP())
            .setTargetQ(nanIfNull(generatorCreationModel.getTargetQ()))
            .setVoltageRegulatorOn(generatorCreationModel.isVoltageRegulationOn())
            .setTargetV(nanIfNull(generatorCreationModel.getTargetV()));

        if (terminal != null) {
            generatorAdder.setRegulatingTerminal(terminal);
        }

        return generatorAdder;
    }

    private void addExtensionsToGenerator(GeneratorCreationModel generatorCreationModel, Generator generator,
                                          VoltageLevel voltageLevel, ReportNode subReportNode) {
        if (generatorCreationModel.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, generatorCreationModel.getEquipmentName(), "Name");
        }
        if (generatorCreationModel.getEnergySource() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, generatorCreationModel.getEnergySource(), "Energy source");
        }
        reportInjectionCreationConnectivity(generatorCreationModel, subReportNode);
        ReportNode subReporterLimits = reportGeneratorActiveLimits(generatorCreationModel, subReportNode);
        ModificationUtils.getInstance().createReactiveLimits(generatorCreationModel, generator, subReporterLimits);
        ReportNode subReporterSetpoints = reportGeneratorSetPoints(generatorCreationModel, subReportNode);
        createGeneratorVoltageRegulation(generatorCreationModel, generator, voltageLevel, subReporterSetpoints);
        ModificationUtils.getInstance().createNewActivePowerControlForInjectionCreation(generator.newExtension(ActivePowerControlAdder.class),
            generatorCreationModel.getParticipate(),
            generatorCreationModel.getDroop(),
            subReporterSetpoints);
        ModificationUtils.getInstance().createShortCircuitExtension(generatorCreationModel.getStepUpTransformerX(),
            generatorCreationModel.getDirectTransX(), generatorCreationModel.getEquipmentId(),
            generator.newExtension(GeneratorShortCircuitAdder.class), subReportNode, "generator");
        createGeneratorStartUp(generatorCreationModel, generator, subReportNode);
    }

    private void createGeneratorInBusBreaker(VoltageLevel voltageLevel, GeneratorCreationModel generatorCreationModel, ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, generatorCreationModel.getBusOrBusbarSectionId());

        // creating the generator
        Generator generator = voltageLevel.newGenerator()
            .setId(generatorCreationModel.getEquipmentId())
            .setName(generatorCreationModel.getEquipmentName())
            .setEnergySource(generatorCreationModel.getEnergySource())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setMinP(generatorCreationModel.getMinP())
            .setMaxP(generatorCreationModel.getMaxP())
            .setRatedS(nanIfNull(generatorCreationModel.getRatedS()))
            .setTargetP(generatorCreationModel.getTargetP())
            .setTargetQ(nanIfNull(generatorCreationModel.getTargetQ()))
            .setVoltageRegulatorOn(generatorCreationModel.isVoltageRegulationOn())
            .setTargetV(nanIfNull(generatorCreationModel.getTargetV()))
            .add();

        addExtensionsToGenerator(generatorCreationModel, generator, voltageLevel, subReportNode);

        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.generatorCreated")
            .withUntypedValue("id", modificationModel.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
    }

    private ReportNode reportGeneratorSetPoints(GeneratorCreationModel generatorCreationModel, ReportNode subReportNode) {
        List<ReportNode> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
            .buildCreationReport(generatorCreationModel.getTargetP(), "Active power"));
        if (generatorCreationModel.getTargetQ() != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(generatorCreationModel.getTargetQ(), "Reactive power"));
        }
        return ModificationUtils.getInstance().reportModifications(subReportNode, setPointReports, "network.modification.SetPointCreated");
    }

    private void createGeneratorVoltageRegulation(GeneratorCreationModel generatorCreationModel, Generator generator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        List<ReportNode> voltageReports = new ArrayList<>();
        voltageReports.add(ModificationUtils.getInstance()
            .createEnabledDisabledReport("network.modification.VoltageRegulationOn", modificationModel.isVoltageRegulationOn()));
        voltageReports.add(ModificationUtils.getInstance().buildCreationReport(generatorCreationModel.getTargetV(), "Voltage"));
        if (generatorCreationModel.getRegulatingTerminalVlId() != null && generatorCreationModel.getRegulatingTerminalId() != null &&
            generatorCreationModel.getRegulatingTerminalType() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                generatorCreationModel.getRegulatingTerminalId(),
                generatorCreationModel.getRegulatingTerminalType(),
                generatorCreationModel.getRegulatingTerminalVlId());
            if (terminal != null) {
                updateGeneratorRegulatingTerminal(generatorCreationModel, generator, terminal, voltageReports);
            }
        }
        if (generatorCreationModel.getQPercent() != null) {
            try {
                generator.newExtension(CoordinatedReactiveControlAdder.class)
                    .withQPercent(generatorCreationModel.getQPercent()).add();
                voltageReports.add(ModificationUtils.getInstance().buildCreationReport(generatorCreationModel.getQPercent(), "Reactive percentage"));
            } catch (PowsyblException e) {
                voltageReports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.ReactivePercentageError")
                    .withUntypedValue("id", generatorCreationModel.getEquipmentId())
                    .withUntypedValue("message", e.getMessage())
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .build());
            }
        }
        ModificationUtils.getInstance().reportModifications(subReportNode, voltageReports, "network.modification.VoltageRegulationCreated");

    }

    private void updateGeneratorRegulatingTerminal(GeneratorCreationModel generatorCreationModel, Generator generator,
                                                   Terminal terminal, List<ReportNode> voltageReports) {
        if (generatorCreationModel.getRegulatingTerminalId() != null
            && generatorCreationModel.getRegulatingTerminalType() != null
            && generatorCreationModel.getRegulatingTerminalVlId() != null) {
            generator.setRegulatingTerminal(terminal);
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                generatorCreationModel.getRegulatingTerminalVlId(),
                "Voltage level"));
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                generatorCreationModel.getRegulatingTerminalType() + ":"
                    + generatorCreationModel.getRegulatingTerminalId(),
                "Equipment"));
        }
    }

    private ReportNode reportGeneratorActiveLimits(GeneratorCreationModel generatorCreationModel, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
        List<ReportNode> limitsReports = new ArrayList<>();
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            generatorCreationModel.getMinP(), "Min active power"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            generatorCreationModel.getMaxP(), "Max active power"));
        if (generatorCreationModel.getRatedS() != null) {
            limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                generatorCreationModel.getRatedS(), "Rated nominal power"));
        }
        ModificationUtils.getInstance().reportModifications(subReportNodeLimits, limitsReports, "network.modification.ActiveLimitsCreated");
        return subReportNodeLimits;
    }

    private void createGeneratorStartUp(GeneratorCreationModel generatorCreationModel, Generator generator, ReportNode subReportNode) {
        if (generatorCreationModel.getPlannedActivePowerSetPoint() != null
            || generatorCreationModel.getMarginalCost() != null
            || generatorCreationModel.getPlannedOutageRate() != null
            || generatorCreationModel.getForcedOutageRate() != null) {
            List<ReportNode> startupReports = new ArrayList<>();
            try {
                generator.newExtension(GeneratorStartupAdder.class)
                    .withPlannedActivePowerSetpoint(nanIfNull(generatorCreationModel.getPlannedActivePowerSetPoint()))
                    .withMarginalCost(nanIfNull(generatorCreationModel.getMarginalCost()))
                    .withPlannedOutageRate(nanIfNull(generatorCreationModel.getPlannedOutageRate()))
                    .withForcedOutageRate(nanIfNull(generatorCreationModel.getForcedOutageRate()))
                    .add();
                if (generatorCreationModel.getPlannedActivePowerSetPoint() != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationModel.getPlannedActivePowerSetPoint(), "Planning active power set point"));
                }
                if (generatorCreationModel.getMarginalCost() != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationModel.getMarginalCost(), "Marginal cost"));
                }
                if (generatorCreationModel.getPlannedOutageRate() != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationModel.getPlannedOutageRate(), "Planning outage rate"));
                }
                if (generatorCreationModel.getForcedOutageRate() != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationModel.getForcedOutageRate(), "Forced outage rate"));
                }
            } catch (PowsyblException e) {
                startupReports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.StartupExtensionAddError")
                    .withUntypedValue("id", generatorCreationModel.getEquipmentId())
                    .withUntypedValue("message", e.getMessage())
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .build());
            }
            ModificationUtils.getInstance().reportModifications(subReportNode, startupReports, "network.modification.startUpAttributesCreated");
        }
    }
}
