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
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.CoordinatedReactiveControlAdder;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuitAdder;
import com.powsybl.iidm.network.extensions.GeneratorStartupAdder;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
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
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Getter
public class GeneratorCreation extends AbstractModification implements ReactiveLimitsHolderInfos {

    private String equipmentId;
    private List<FreePropertyInfos> properties;
    private String equipmentName;
    private String voltageLevelId;
    private String busOrBusbarSectionId;
    private String connectionName;
    private ConnectablePosition.Direction connectionDirection;
    private Integer connectionPosition;
    private boolean terminalConnected;
    private EnergySource energySource;
    private double minP;
    private double maxP;
    private Double ratedS;
    private double targetP;
    private Double targetQ;
    private boolean voltageRegulationOn;
    private Double targetV;
    private Double plannedActivePowerSetPoint;
    private Double marginalCost;
    private Double plannedOutageRate;
    private Double forcedOutageRate;
    private Double minQ;
    private Double maxQ;
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;
    private Boolean participate;
    private Float droop;
    private Double directTransX;
    private Double stepUpTransformerX;
    private String regulatingTerminalId;
    private String regulatingTerminalType;
    private String regulatingTerminalVlId;
    private Double qPercent;
    private Boolean reactiveCapabilityCurve;

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getGenerator(equipmentId) != null) {
            throw new NetworkModificationException(GENERATOR_ALREADY_EXISTS, equipmentId);
        }
        String errorMessage = "Generator '" + equipmentId + "' : ";

        // check connectivity
        ModificationUtils.getInstance().controlConnectivity(network, voltageLevelId, busOrBusbarSectionId);

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(this,
                CREATE_GENERATOR_ERROR,
                equipmentId,
                "Generator");

        // check regulated terminal
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId);
        ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
            regulatingTerminalId,
            regulatingTerminalType,
            regulatingTerminalVlId);

        ModificationUtils.getInstance().checkActivePowerControl(participate,
            droop, CREATE_GENERATOR_ERROR, String.format(ERROR_MESSAGE, equipmentId));

        checkIsNotNegativeValue(errorMessage, targetV, CREATE_GENERATOR_ERROR, "Target Voltage");
        checkIsPercentage(errorMessage, droop, CREATE_GENERATOR_ERROR, "Droop");
        checkIsNotNegativeValue(errorMessage, ratedS, CREATE_GENERATOR_ERROR, "Rated apparent power");
        checkPowerValues(errorMessage, minP, maxP, targetP, plannedActivePowerSetPoint, CREATE_GENERATOR_ERROR);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the generator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId);
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createGeneratorInNodeBreaker(voltageLevel, network, subReportNode);
        } else {
            createGeneratorInBusBreaker(voltageLevel, subReportNode);
        }
        if (!terminalConnected) {
            network.getGenerator(equipmentId).getTerminal().disconnect();
        }
        // apply the properties
        Generator generator = network.getGenerator(equipmentId);
        PropertiesUtils.applyProperties(generator, subReportNode, properties, "network.modification.GeneratorProperties");
    }

    @Override
    public String getName() {
        return "GeneratorCreation";
    }

    private void createGeneratorInNodeBreaker(VoltageLevel voltageLevel, Network network, ReportNode subReportNode) {
        GeneratorAdder generatorAdder = createGeneratorAdderInNodeBreaker(voltageLevel);
        createInjectionInNodeBreaker(voltageLevel, equipmentId, busOrBusbarSectionId, connectionName, connectionDirection, connectionPosition, network, generatorAdder, subReportNode);

        // CreateFeederBayBuilder already create the generator using
        // (withInjectionAdder(generatorAdder)) so then we can add the additional informations and extensions
        var generator = ModificationUtils.getInstance().getGenerator(network, equipmentId);
        addExtensionsToGenerator(generator, voltageLevel, subReportNode);
    }

    private GeneratorAdder createGeneratorAdderInNodeBreaker(VoltageLevel voltageLevel) {
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
            regulatingTerminalId,
            regulatingTerminalType,
            regulatingTerminalVlId);

        // creating the generator adder
        GeneratorAdder generatorAdder = voltageLevel.newGenerator()
            .setId(equipmentId)
            .setName(equipmentName)
            .setEnergySource(energySource)
            .setMinP(minP)
            .setMaxP(maxP)
            .setRatedS(nanIfNull(ratedS))
            .setTargetP(targetP)
            .setTargetQ(nanIfNull(targetQ))
            .setVoltageRegulatorOn(voltageRegulationOn)
            .setTargetV(nanIfNull(targetV));

        if (terminal != null) {
            generatorAdder.setRegulatingTerminal(terminal);
        }

        return generatorAdder;
    }

    private void addExtensionsToGenerator(Generator generator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        if (equipmentName != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, equipmentName, "Name");
        }
        if (energySource != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, energySource, "Energy source");
        }
        reportInjectionCreationConnectivity(voltageLevelId, busOrBusbarSectionId, connectionName, connectionDirection, connectionPosition, terminalConnected, equipmentId, subReportNode);
        ReportNode subReporterLimits = reportGeneratorActiveLimits(subReportNode);
        ModificationUtils.getInstance().createReactiveLimits(this, generator, subReporterLimits);
        ReportNode subReporterSetpoints = reportGeneratorSetPoints(subReportNode);
        createGeneratorVoltageRegulation(generator, voltageLevel, subReporterSetpoints);
        ModificationUtils.getInstance().createNewActivePowerControlForInjectionCreation(generator.newExtension(ActivePowerControlAdder.class),
                participate,
                droop,
                subReporterSetpoints);
        ModificationUtils.getInstance().createShortCircuitExtension(stepUpTransformerX,
                directTransX, equipmentId,
                generator.newExtension(GeneratorShortCircuitAdder.class), subReportNode, "generator");
        createGeneratorStartUp(generator, subReportNode);
    }

    private void createGeneratorInBusBreaker(VoltageLevel voltageLevel, ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, busOrBusbarSectionId);

        // creating the generator
        Generator generator = voltageLevel.newGenerator()
            .setId(equipmentId)
            .setName(equipmentName)
            .setEnergySource(energySource)
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setMinP(minP)
            .setMaxP(maxP)
            .setRatedS(nanIfNull(ratedS))
            .setTargetP(targetP)
            .setTargetQ(nanIfNull(targetQ))
            .setVoltageRegulatorOn(voltageRegulationOn)
            .setTargetV(nanIfNull(targetV))
            .add();

        addExtensionsToGenerator(generator, voltageLevel, subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.generatorCreated")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private ReportNode reportGeneratorSetPoints(ReportNode subReportNode) {
        List<ReportNode> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(targetP, "Active power"));
        if (targetQ != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(targetQ, "Reactive power"));
        }
        return ModificationUtils.getInstance().reportModifications(subReportNode, setPointReports, "network.modification.SetPointCreated");
    }

    private void createGeneratorVoltageRegulation(Generator generator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        List<ReportNode> voltageReports = new ArrayList<>();
        voltageReports.add(ModificationUtils.getInstance()
                .createEnabledDisabledReport("network.modification.VoltageRegulationOn", voltageRegulationOn));
        voltageReports.add(ModificationUtils.getInstance().buildCreationReport(targetV, "Voltage"));
        if (regulatingTerminalVlId != null && regulatingTerminalId != null && regulatingTerminalType != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                    regulatingTerminalId,
                    regulatingTerminalType,
                    regulatingTerminalVlId);
            if (terminal != null) {
                updateGeneratorRegulatingTerminal(generator, terminal, voltageReports);
            }
        }
        if (qPercent != null) {
            try {
                generator.newExtension(CoordinatedReactiveControlAdder.class)
                        .withQPercent(qPercent).add();
                voltageReports.add(ModificationUtils.getInstance().buildCreationReport(qPercent, "Reactive percentage"));
            } catch (PowsyblException e) {
                voltageReports.add(ReportNode.newRootReportNode()
                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                        .withMessageTemplate("network.modification.ReactivePercentageError")
                        .withUntypedValue("id", equipmentId)
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
        }
        ModificationUtils.getInstance().reportModifications(subReportNode, voltageReports, "network.modification.VoltageRegulationCreated");

    }

    private void updateGeneratorRegulatingTerminal(Generator generator, Terminal terminal, List<ReportNode> voltageReports) {
        if (regulatingTerminalId != null && regulatingTerminalType != null && regulatingTerminalVlId != null) {
            generator.setRegulatingTerminal(terminal);
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                    regulatingTerminalVlId,
                    "Voltage level"));
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                    regulatingTerminalType + ":" + regulatingTerminalId,
                    "Equipment"));
        }
    }

    private ReportNode reportGeneratorActiveLimits(ReportNode subReportNode) {
        ReportNode subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
        List<ReportNode> limitsReports = new ArrayList<>();
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            minP, "Min active power"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            maxP, "Max active power"));
        if (ratedS != null) {
            limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                ratedS, "Rated nominal power"));
        }
        ModificationUtils.getInstance().reportModifications(subReportNodeLimits, limitsReports, "network.modification.ActiveLimitsCreated");
        return subReportNodeLimits;
    }

    private void createGeneratorStartUp(Generator generator, ReportNode subReportNode) {
        if (plannedActivePowerSetPoint != null || marginalCost != null || plannedOutageRate != null || forcedOutageRate != null) {
            List<ReportNode> startupReports = new ArrayList<>();
            try {
                generator.newExtension(GeneratorStartupAdder.class)
                        .withPlannedActivePowerSetpoint(nanIfNull(plannedActivePowerSetPoint))
                        .withMarginalCost(nanIfNull(marginalCost))
                        .withPlannedOutageRate(nanIfNull(plannedOutageRate))
                        .withForcedOutageRate(nanIfNull(forcedOutageRate))
                        .add();
                if (plannedActivePowerSetPoint != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        plannedActivePowerSetPoint, "Planning active power set point"));
                }
                if (marginalCost != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        marginalCost, "Marginal cost"));
                }
                if (plannedOutageRate != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        plannedOutageRate, "Planning outage rate"));
                }
                if (forcedOutageRate != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        forcedOutageRate, "Forced outage rate"));
                }
            } catch (PowsyblException e) {
                startupReports.add(ReportNode.newRootReportNode()
                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                        .withMessageTemplate("network.modification.StartupExtensionAddError")
                        .withUntypedValue("id", equipmentId)
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReportNode, startupReports, "network.modification.startUpAttributesCreated");
        }
    }

}
