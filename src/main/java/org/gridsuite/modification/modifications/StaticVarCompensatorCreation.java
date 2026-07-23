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
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.StandbyAutomatonAdder;
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.VoltageRegulationType;
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
@Getter
@Setter
public class StaticVarCompensatorCreation extends AbstractInjectionCreation {

    private Double maxSusceptance;
    private Double minSusceptance;
    private Double maxQAtNominalV;
    private Double minQAtNominalV;
    private StaticVarCompensator.RegulationMode regulationMode;
    private Double voltageSetpoint;
    private Double reactivePowerSetpoint;
    private VoltageRegulationType voltageRegulationType;
    private String regulatingTerminalId;
    private String regulatingTerminalType;
    private String regulatingTerminalVlId;
    private boolean regulating;
    private boolean standbyAutomatonOn;
    private boolean standby;
    private Double b0;
    private Double q0;
    private Double lowVoltageSetpoint;
    private Double highVoltageSetpoint;
    private Double lowVoltageThreshold;
    private Double highVoltageThreshold;

    @Builder
    public StaticVarCompensatorCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName,
                                        String voltageLevelId, String busOrBusbarSectionId, String connectionName,
                                        ConnectablePosition.Direction connectionDirection, Integer connectionPosition,
                                        boolean terminalConnected, Double maxSusceptance, Double minSusceptance,
                                        Double maxQAtNominalV, Double minQAtNominalV,
                                        StaticVarCompensator.RegulationMode regulationMode, Double voltageSetpoint,
                                        Double reactivePowerSetpoint, VoltageRegulationType voltageRegulationType,
                                        String regulatingTerminalId, String regulatingTerminalType,
                                        String regulatingTerminalVlId, boolean regulating, boolean standbyAutomatonOn,
                                        boolean standby, Double b0, Double q0, Double lowVoltageSetpoint,
                                        Double highVoltageSetpoint, Double lowVoltageThreshold,
                                        Double highVoltageThreshold) {
        super(equipmentId, properties, equipmentName, voltageLevelId, busOrBusbarSectionId, connectionName,
            connectionDirection, connectionPosition, terminalConnected);
        this.maxSusceptance = maxSusceptance;
        this.minSusceptance = minSusceptance;
        this.maxQAtNominalV = maxQAtNominalV;
        this.minQAtNominalV = minQAtNominalV;
        this.regulationMode = regulationMode;
        this.voltageSetpoint = voltageSetpoint;
        this.reactivePowerSetpoint = reactivePowerSetpoint;
        this.voltageRegulationType = voltageRegulationType;
        this.regulatingTerminalId = regulatingTerminalId;
        this.regulatingTerminalType = regulatingTerminalType;
        this.regulatingTerminalVlId = regulatingTerminalVlId;
        this.regulating = regulating;
        this.standbyAutomatonOn = standbyAutomatonOn;
        this.standby = standby;
        this.b0 = b0;
        this.q0 = q0;
        this.lowVoltageSetpoint = lowVoltageSetpoint;
        this.highVoltageSetpoint = highVoltageSetpoint;
        this.lowVoltageThreshold = lowVoltageThreshold;
        this.highVoltageThreshold = highVoltageThreshold;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getStaticVarCompensator(equipmentId) != null) {
            throw new NetworkModificationException(STATIC_VAR_COMPENSATOR_ALREADY_EXISTS, equipmentId);
        }
        String errorMessage = "Static var compensator '" + equipmentId + "' : ";

        // check connectivity
        ModificationUtils.getInstance()
                .controlConnectivity(network, voltageLevelId,
                busOrBusbarSectionId);

        // check reactive power limits and set points
        ModificationUtils.getInstance().checkReactivePowerLimitsAndSetPointsCreation(this);

        // check regulated terminal
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId);
        ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(), regulatingTerminalId,
                regulatingTerminalType, regulatingTerminalVlId);

        // check standby automaton
        ModificationUtils.getInstance().checkStandbyAutomatonCreation(this);
        checkIsNotNegativeValue(errorMessage, voltageSetpoint, CREATE_STATIC_VAR_COMPENSATOR_ERROR, "voltage set point");
        checkIsNotNegativeValue(errorMessage, highVoltageSetpoint, CREATE_STATIC_VAR_COMPENSATOR_ERROR, "high voltage set point");
        checkIsNotNegativeValue(errorMessage, lowVoltageSetpoint, CREATE_STATIC_VAR_COMPENSATOR_ERROR, "low voltage set point");
        checkIsNotNegativeValue(errorMessage, highVoltageThreshold, CREATE_STATIC_VAR_COMPENSATOR_ERROR, "high voltage threshold");
        checkIsNotNegativeValue(errorMessage, lowVoltageThreshold, CREATE_STATIC_VAR_COMPENSATOR_ERROR, "low voltage threshold");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the static var compensator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId);
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createStaticVarCompensatorInNodeBreaker(voltageLevel, network, subReportNode);
        } else {
            createStaticVarCompensatorInBusBreaker(voltageLevel, subReportNode);
        }
        ModificationUtils.getInstance().disconnectCreatedInjection(this, network.getStaticVarCompensator(equipmentId), subReportNode);
        // properties
        StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(equipmentId);
        PropertiesUtils.applyProperties(staticVarCompensator, subReportNode, properties, "network.modification.StaticVarCompensatorProperties");
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.staticVarCompensatorCreated")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    @Override
    public String getName() {
        return "StaticVarCompensatorCreation";
    }

    private void createStaticVarCompensatorInNodeBreaker(VoltageLevel voltageLevel, Network network, ReportNode subReportNode) {
        StaticVarCompensatorAdder staticVarCompensatorAdder = createStaticVarCompensatorAdderInNodeBreaker(voltageLevel);
        createInjectionInNodeBreaker(voltageLevel, this, network, staticVarCompensatorAdder, subReportNode);
        var staticVarCompensator = ModificationUtils.getInstance().getStaticVarCompensator(network, equipmentId);
        addExtensionsToStaticVarCompensator(staticVarCompensator, voltageLevel, subReportNode);
    }

    private StaticVarCompensatorAdder createStaticVarCompensatorAdderInNodeBreaker(VoltageLevel voltageLevel) {
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                regulatingTerminalId,
                regulatingTerminalType,
                regulatingTerminalVlId);
        double bMax = Objects.isNull(maxSusceptance) && Objects.nonNull(maxQAtNominalV) ?
                maxQAtNominalV / Math.pow(voltageLevel.getNominalV(), 2) : maxSusceptance;
        double bMin = Objects.isNull(minSusceptance) && Objects.nonNull(minQAtNominalV) ?
                minQAtNominalV / Math.pow(voltageLevel.getNominalV(), 2) : minSusceptance;
        StaticVarCompensatorAdder staticVarCompensatorAdder = voltageLevel.newStaticVarCompensator()
                .setId(equipmentId)
                .setName(equipmentName)
                .setBmax(bMax)
                .setBmin(bMin)
                .setVoltageSetpoint(nanIfNull(voltageSetpoint))
                .setReactivePowerSetpoint(nanIfNull(reactivePowerSetpoint))
                .setRegulating(regulating);
        if (regulating) {
            staticVarCompensatorAdder.setRegulationMode(regulationMode);
        }

        if (terminal != null) {
            staticVarCompensatorAdder.setRegulatingTerminal(terminal);
        }

        return staticVarCompensatorAdder;
    }

    private void addExtensionsToStaticVarCompensator(StaticVarCompensator staticVarCompensator,
                                                     VoltageLevel voltageLevel,
                                                     ReportNode subReportNode) {
        if (equipmentName != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, equipmentName, "Name");
        }

        reportInjectionCreationConnectivity(this, subReportNode);
        reportStaticVarCompensatorLimitsAndSetpoints(staticVarCompensator, voltageLevel, subReportNode);
        reportStaticVarCompensatorStandbyAutomaton(staticVarCompensator, voltageLevel, subReportNode);
    }

    private void reportStaticVarCompensatorStandbyAutomaton(StaticVarCompensator staticVarCompensator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        if (standbyAutomatonOn) {
            List<ReportNode> standbyAutomatonReports = new ArrayList<>();
            double computedB0 = Objects.isNull(b0) && Objects.nonNull(q0) ?
                    q0 / Math.pow(voltageLevel.getNominalV(), 2) : b0;
            try {
                staticVarCompensator.newExtension(StandbyAutomatonAdder.class)
                        .withStandbyStatus(standby)
                        .withB0(computedB0)
                        .withLowVoltageSetpoint(lowVoltageSetpoint)
                        .withHighVoltageSetpoint(highVoltageSetpoint)
                        .withLowVoltageThreshold(lowVoltageThreshold)
                        .withHighVoltageThreshold(highVoltageThreshold)
                        .add();
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        standby,
                        "Standby"));
                if (Objects.nonNull(b0)) {
                    standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                            b0,
                            "Fixed part of susceptance"));
                }
                if (Objects.nonNull(q0)) {
                    standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                            q0,
                            "Fixed part of Q at nominal voltage"));
                }
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        lowVoltageSetpoint,
                        "Low voltage set point"));
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        highVoltageSetpoint,
                        "High voltage set point"));
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        highVoltageThreshold,
                        "High voltage threshold"));
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        lowVoltageThreshold,
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

    private void createStaticVarCompensatorInBusBreaker(VoltageLevel voltageLevel, ReportNode subReportNode) {

        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, busOrBusbarSectionId);
        double bMax = Objects.isNull(maxSusceptance) && Objects.nonNull(maxQAtNominalV) ?
                maxQAtNominalV / Math.pow(voltageLevel.getNominalV(), 2) : maxSusceptance;
        double bMin = Objects.isNull(minSusceptance) && Objects.nonNull(minQAtNominalV) ?
                minQAtNominalV / Math.pow(voltageLevel.getNominalV(), 2) : minSusceptance;
        /* creating the static var compensator */
        StaticVarCompensatorAdder staticVarCompensatorAdder = voltageLevel.newStaticVarCompensator()
                .setId(equipmentId)
                .setName(equipmentName)
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .setBmax(bMax)
                .setBmin(bMin)
                .setVoltageSetpoint(voltageSetpoint)
                .setReactivePowerSetpoint(reactivePowerSetpoint)
                .setRegulating(regulating);
        if (regulating) {
            staticVarCompensatorAdder.setRegulationMode(regulationMode);
        }
        StaticVarCompensator staticVarCompensator = staticVarCompensatorAdder.add();

        addExtensionsToStaticVarCompensator(staticVarCompensator, voltageLevel, subReportNode);
    }

    private void reportStaticVarCompensatorLimitsAndSetpoints(StaticVarCompensator staticVarCompensator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        List<ReportNode> voltageReports = new ArrayList<>();
        if (Objects.nonNull(minSusceptance)) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(minSusceptance, "Susceptance min"));
        }
        if (Objects.nonNull(maxSusceptance)) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(maxSusceptance, "Susceptance max"));
        }
        if (Objects.nonNull(minQAtNominalV)) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(minQAtNominalV, "Q min at nominal voltage"));
        }
        if (Objects.nonNull(maxQAtNominalV)) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(maxQAtNominalV, "Q max at nominal voltage"));
        }
        if (Objects.nonNull(regulationMode)) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(regulationMode, "regulation mode"));
        }
        if (Objects.nonNull(voltageSetpoint)) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(voltageSetpoint, "Voltage set point"));
        }
        if (Objects.nonNull(reactivePowerSetpoint)) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(reactivePowerSetpoint, "Reactive power set point"));
        }
        if (Objects.nonNull(voltageRegulationType)) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(voltageRegulationType, "Voltage Regulation type"));
        }
        if (regulatingTerminalVlId != null && regulatingTerminalId != null &&
                regulatingTerminalType != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                    regulatingTerminalId,
                    regulatingTerminalType,
                    regulatingTerminalVlId);
            if (terminal != null) {
                updateCompensatorRegulatingTerminal(staticVarCompensator, terminal, voltageReports);
            }
        }
        ModificationUtils.getInstance().reportModifications(subReportNode, voltageReports, "network.modification.LimitsAndSetpointsCreated");
    }

    private void updateCompensatorRegulatingTerminal(StaticVarCompensator staticVarCompensator,
                                                   Terminal terminal, List<ReportNode> voltageReports) {
        if (regulatingTerminalId != null
                && regulatingTerminalType != null
                && regulatingTerminalVlId != null) {
            staticVarCompensator.setRegulatingTerminal(terminal);
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                    regulatingTerminalVlId,
                    "Voltage level"));
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                    regulatingTerminalType + ":"
                            + regulatingTerminalId,
                    "Equipment"));
        }
    }

}
