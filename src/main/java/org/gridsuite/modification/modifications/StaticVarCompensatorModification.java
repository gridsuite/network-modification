/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import com.powsybl.iidm.network.extensions.StandbyAutomaton;
import com.powsybl.iidm.network.extensions.StandbyAutomatonAdder;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.dto.VoltageRegulationType;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.error.NetworkModificationExceptionType.MODIFY_STATIC_VAR_COMPENSATOR_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;
import static org.gridsuite.modification.utils.ModificationUtils.isRegulatingTerminalModified;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class StaticVarCompensatorModification extends AbstractInjectionModification {

    private AttributeModification<Double> maxSusceptance;
    private AttributeModification<Double> minSusceptance;
    private AttributeModification<Double> maxQAtNominalV;
    private AttributeModification<Double> minQAtNominalV;
    private AttributeModification<StaticVarCompensator.RegulationMode> regulationMode;
    private AttributeModification<Boolean> regulating;
    private AttributeModification<Double> voltageSetpoint;
    private AttributeModification<Double> reactivePowerSetpoint;
    private AttributeModification<VoltageRegulationType> voltageRegulationType;
    private AttributeModification<String> regulatingTerminalId;
    private AttributeModification<String> regulatingTerminalType;
    private AttributeModification<String> regulatingTerminalVlId;
    private AttributeModification<Boolean> standbyAutomatonOn;
    private AttributeModification<Boolean> standby;
    private AttributeModification<Double> b0;
    private AttributeModification<Double> q0;
    private AttributeModification<Double> lowVoltageSetpoint;
    private AttributeModification<Double> highVoltageSetpoint;
    private AttributeModification<Double> lowVoltageThreshold;
    private AttributeModification<Double> highVoltageThreshold;

    @Builder
    public StaticVarCompensatorModification(String equipmentId, List<FreePropertyInfos> properties,
                                            AttributeModification<String> equipmentName,
                                            AttributeModification<String> voltageLevelId,
                                            AttributeModification<String> busOrBusbarSectionId,
                                            AttributeModification<String> connectionName,
                                            AttributeModification<ConnectablePosition.Direction> connectionDirection,
                                            AttributeModification<Integer> connectionPosition,
                                            AttributeModification<Boolean> terminalConnected,
                                            AttributeModification<Double> pMeasurementValue,
                                            AttributeModification<Boolean> pMeasurementValidity,
                                            AttributeModification<Double> qMeasurementValue,
                                            AttributeModification<Boolean> qMeasurementValidity,
                                            AttributeModification<Double> maxSusceptance,
                                            AttributeModification<Double> minSusceptance,
                                            AttributeModification<Double> maxQAtNominalV,
                                            AttributeModification<Double> minQAtNominalV,
                                            AttributeModification<StaticVarCompensator.RegulationMode> regulationMode,
                                            AttributeModification<Boolean> regulating,
                                            AttributeModification<Double> voltageSetpoint,
                                            AttributeModification<Double> reactivePowerSetpoint,
                                            AttributeModification<VoltageRegulationType> voltageRegulationType,
                                            AttributeModification<String> regulatingTerminalId,
                                            AttributeModification<String> regulatingTerminalType,
                                            AttributeModification<String> regulatingTerminalVlId,
                                            AttributeModification<Boolean> standbyAutomatonOn,
                                            AttributeModification<Boolean> standby,
                                            AttributeModification<Double> b0,
                                            AttributeModification<Double> q0,
                                            AttributeModification<Double> lowVoltageSetpoint,
                                            AttributeModification<Double> highVoltageSetpoint,
                                            AttributeModification<Double> lowVoltageThreshold,
                                            AttributeModification<Double> highVoltageThreshold) {
        super(equipmentId, properties, equipmentName, voltageLevelId, busOrBusbarSectionId, connectionName,
            connectionDirection, connectionPosition, terminalConnected, pMeasurementValue, pMeasurementValidity,
            qMeasurementValue, qMeasurementValidity);
        this.maxSusceptance = maxSusceptance;
        this.minSusceptance = minSusceptance;
        this.maxQAtNominalV = maxQAtNominalV;
        this.minQAtNominalV = minQAtNominalV;
        this.regulationMode = regulationMode;
        this.regulating = regulating;
        this.voltageSetpoint = voltageSetpoint;
        this.reactivePowerSetpoint = reactivePowerSetpoint;
        this.voltageRegulationType = voltageRegulationType;
        this.regulatingTerminalId = regulatingTerminalId;
        this.regulatingTerminalType = regulatingTerminalType;
        this.regulatingTerminalVlId = regulatingTerminalVlId;
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
        if (equipmentId == null) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, "Missing required attributes to modify the equipment");
        }
        StaticVarCompensator staticVarCompensator = ModificationUtils.getInstance().getStaticVarCompensator(network, equipmentId);
        String errorMessage = "Static var compensator '" + equipmentId + "' : ";

        ModificationUtils.getInstance().checkVoltageLevelModification(network, voltageLevelId, busOrBusbarSectionId, staticVarCompensator.getTerminal());
        checkSusceptanceLimits(network, staticVarCompensator, errorMessage);
        checkRegulation(staticVarCompensator, errorMessage);
        checkStandbyAutomaton(network, staticVarCompensator, errorMessage);
        ModificationUtils.getInstance().checkEnableRegulation(
                voltageRegulationType,
                regulatingTerminalId,
                regulatingTerminalType,
                regulatingTerminalVlId,
                staticVarCompensator.getTerminal(),
                staticVarCompensator.getRegulatingTerminal(),
                network,
                MODIFY_STATIC_VAR_COMPENSATOR_ERROR,
                errorMessage);
    }

    private void checkSusceptanceLimits(Network network, StaticVarCompensator staticVarCompensator, String errorMessage) {
        double nominalV = getFinalNominalV(network, staticVarCompensator);
        Double finalMinSusceptance = getFinalValue(toSusceptanceModification(minSusceptance, minQAtNominalV, nominalV), staticVarCompensator.getBmin());
        Double finalMaxSusceptance = getFinalValue(toSusceptanceModification(maxSusceptance, maxQAtNominalV, nominalV), staticVarCompensator.getBmax());
        if (isNotSet(finalMinSusceptance)) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "minimum susceptance is not set");
        }
        if (isNotSet(finalMaxSusceptance)) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "maximum susceptance is not set");
        }
        if (finalMaxSusceptance < finalMinSusceptance) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "maximum susceptance is expected to be greater than or equal to minimum susceptance");
        }
    }

    private void checkRegulation(StaticVarCompensator staticVarCompensator, String errorMessage) {
        if (voltageSetpoint != null) {
            checkIsNotNegativeValue(errorMessage, voltageSetpoint.getValue(), MODIFY_STATIC_VAR_COMPENSATOR_ERROR, "voltage set point");
        }
        if (regulationMode != null && regulationMode.getValue() == null) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "Regulation mode can not be unset");
        }
        StaticVarCompensator.RegulationMode finalRegulationMode = getFinalRegulationMode(staticVarCompensator);
        if (!getFinalRegulating(staticVarCompensator)) {
            return;
        }
        if (finalRegulationMode == null) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "Regulation mode is not set");
        }
        if (finalRegulationMode == StaticVarCompensator.RegulationMode.VOLTAGE
                && isNotSet(getFinalValue(voltageSetpoint, staticVarCompensator.getVoltageSetpoint()))) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "Voltage setpoint is not set");
        }
        if (finalRegulationMode == StaticVarCompensator.RegulationMode.REACTIVE_POWER
                && isNotSet(getFinalValue(reactivePowerSetpoint, staticVarCompensator.getReactivePowerSetpoint()))) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "Reactive power setpoint is not set");
        }
    }

    private void checkStandbyAutomaton(Network network, StaticVarCompensator staticVarCompensator, String errorMessage) {
        if (isStandbyAutomatonRemoved()) {
            return;
        }
        StandbyAutomaton standbyAutomaton = staticVarCompensator.getExtension(StandbyAutomaton.class);
        boolean isStandbyAutomatonAdded = standbyAutomaton == null && standbyAutomatonOn != null && Boolean.TRUE.equals(standbyAutomatonOn.getValue());
        if (standbyAutomaton == null && !isStandbyAutomatonAdded) {
            if (isStandbyAutomatonFieldModified()) {
                throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR,
                        errorMessage + "Standby automaton does not exist : it must be activated to modify its attributes");
            }
            return;
        }

        double nominalV = getFinalNominalV(network, staticVarCompensator);
        Double finalB0 = getFinalValue(toSusceptanceModification(b0, q0, nominalV), standbyAutomaton != null ? standbyAutomaton.getB0() : Double.NaN);
        Double finalLowVoltageSetpoint = getFinalValue(lowVoltageSetpoint, standbyAutomaton != null ? standbyAutomaton.getLowVoltageSetpoint() : Double.NaN);
        Double finalHighVoltageSetpoint = getFinalValue(highVoltageSetpoint, standbyAutomaton != null ? standbyAutomaton.getHighVoltageSetpoint() : Double.NaN);
        Double finalLowVoltageThreshold = getFinalValue(lowVoltageThreshold, standbyAutomaton != null ? standbyAutomaton.getLowVoltageThreshold() : Double.NaN);
        Double finalHighVoltageThreshold = getFinalValue(highVoltageThreshold, standbyAutomaton != null ? standbyAutomaton.getHighVoltageThreshold() : Double.NaN);
        boolean finalStandby = getFinalStandby(standbyAutomaton);

        checkIsSet(finalB0, errorMessage, "fixed part of susceptance");
        checkIsSet(finalLowVoltageSetpoint, errorMessage, "low voltage set point");
        checkIsSet(finalHighVoltageSetpoint, errorMessage, "high voltage set point");
        checkIsSet(finalLowVoltageThreshold, errorMessage, "low voltage threshold");
        checkIsSet(finalHighVoltageThreshold, errorMessage, "high voltage threshold");
        checkIsNotNegativeValue(errorMessage, finalLowVoltageSetpoint, MODIFY_STATIC_VAR_COMPENSATOR_ERROR, "low voltage set point");
        checkIsNotNegativeValue(errorMessage, finalHighVoltageSetpoint, MODIFY_STATIC_VAR_COMPENSATOR_ERROR, "high voltage set point");
        checkIsNotNegativeValue(errorMessage, finalLowVoltageThreshold, MODIFY_STATIC_VAR_COMPENSATOR_ERROR, "low voltage threshold");
        checkIsNotNegativeValue(errorMessage, finalHighVoltageThreshold, MODIFY_STATIC_VAR_COMPENSATOR_ERROR, "high voltage threshold");

        if (finalStandby && finalLowVoltageThreshold >= finalHighVoltageThreshold) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR,
                    errorMessage + "Low voltage threshold is expected to be lower than high voltage threshold when standby is on");
        }

        boolean isSusceptanceRangeModified = minSusceptance != null || maxSusceptance != null || minQAtNominalV != null || maxQAtNominalV != null;
        if (isStandbyAutomatonAdded || b0 != null || q0 != null || isSusceptanceRangeModified) {
            double finalMinSusceptance = getFinalValue(toSusceptanceModification(minSusceptance, minQAtNominalV, nominalV), staticVarCompensator.getBmin());
            double finalMaxSusceptance = getFinalValue(toSusceptanceModification(maxSusceptance, maxQAtNominalV, nominalV), staticVarCompensator.getBmax());
            if (finalB0 < finalMinSusceptance || finalB0 > finalMaxSusceptance) {
                throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR,
                        errorMessage + "b0 must be within the range of minimum susceptance and maximum susceptance");
            }
        }

        boolean isStandbyOrRegulationModeModified = isStandbyAutomatonAdded || standby != null || regulationMode != null;
        if (isStandbyOrRegulationModeModified && finalStandby
                && getFinalRegulationMode(staticVarCompensator) != StaticVarCompensator.RegulationMode.VOLTAGE) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "Standby is only supported in Voltage Regulation mode");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        StaticVarCompensator staticVarCompensator = ModificationUtils.getInstance().getStaticVarCompensator(network, equipmentId);
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.staticVarCompensatorModification")
                .withUntypedValue("id", equipmentId)
                .add();

        if (equipmentName != null && equipmentName.getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(staticVarCompensator::setName,
                    () -> staticVarCompensator.getOptionalName().orElse("No value"), equipmentName, subReportNode, "Name");
        }
        ModificationUtils.getInstance().moveFeederBay(staticVarCompensator, staticVarCompensator.getTerminal(), voltageLevelId, busOrBusbarSectionId, subReportNode);
        modifyLimits(staticVarCompensator, subReportNode);
        modifySetpoints(staticVarCompensator, subReportNode);
        modifyStandbyAutomaton(staticVarCompensator, subReportNode);
        modifyConnectivity(staticVarCompensator, subReportNode);
        updateMeasurements(staticVarCompensator, subReportNode);
        PropertiesUtils.applyProperties(staticVarCompensator, subReportNode, properties, "network.modification.StaticVarCompensatorProperties");
    }

    @Override
    public String getName() {
        return ModificationType.STATIC_VAR_COMPENSATOR_MODIFICATION.name();
    }

    private void modifyLimits(StaticVarCompensator staticVarCompensator, ReportNode subReportNode) {
        double nominalV = staticVarCompensator.getTerminal().getVoltageLevel().getNominalV();
        List<ReportNode> reports = new ArrayList<>();
        addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(staticVarCompensator::setBmin, staticVarCompensator::getBmin,
                toSusceptanceModification(minSusceptance, minQAtNominalV, nominalV), "Susceptance min"));
        addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(staticVarCompensator::setBmax, staticVarCompensator::getBmax,
                toSusceptanceModification(maxSusceptance, maxQAtNominalV, nominalV), "Susceptance max"));
        insertReports(reports, subReportNode, "network.modification.limits");
    }

    private void modifySetpoints(StaticVarCompensator staticVarCompensator, ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        modifyRegulation(staticVarCompensator, reports);
        modifyRegulatingTerminal(staticVarCompensator, reports);
        insertReports(reports, subReportNode, "network.modification.Setpoints");
    }

    private void modifyRegulation(StaticVarCompensator staticVarCompensator, List<ReportNode> reports) {
        if (voltageSetpoint == null && reactivePowerSetpoint == null && regulationMode == null && regulating == null) {
            return;
        }
        boolean oldRegulating = staticVarCompensator.isRegulating();
        boolean finalRegulating = getFinalRegulating(staticVarCompensator);

        staticVarCompensator.setRegulating(false);
        addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                value -> staticVarCompensator.setVoltageSetpoint(value != null ? value : Double.NaN),
                staticVarCompensator::getVoltageSetpoint, voltageSetpoint, "Voltage"));
        addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                value -> staticVarCompensator.setReactivePowerSetpoint(value != null ? value : Double.NaN),
                staticVarCompensator::getReactivePowerSetpoint, reactivePowerSetpoint, "Reactive power"));
        addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(staticVarCompensator::setRegulationMode,
                staticVarCompensator::getRegulationMode, regulationMode, "Regulation mode"));
        staticVarCompensator.setRegulating(finalRegulating);
        if (regulating != null) {
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldRegulating, finalRegulating, "Regulating"));
        }
    }

    private void modifyRegulatingTerminal(StaticVarCompensator staticVarCompensator, List<ReportNode> reports) {
        if (!isRegulatingTerminalModified(voltageRegulationType)) {
            return;
        }
        String oldVoltageLevel = null;
        String oldEquipment = null;
        Terminal oldRegulatingTerminal = staticVarCompensator.getRegulatingTerminal();
        if (oldRegulatingTerminal != null && !oldRegulatingTerminal.equals(staticVarCompensator.getTerminal())) {
            oldVoltageLevel = oldRegulatingTerminal.getVoltageLevel().getId();
            oldEquipment = oldRegulatingTerminal.getConnectable().getType().name() + ":" + oldRegulatingTerminal.getConnectable().getId();
        }

        if (voltageRegulationType.getValue() == VoltageRegulationType.LOCAL) {
            if (oldEquipment != null) {
                staticVarCompensator.setRegulatingTerminal(null);
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel, null, "Voltage level"));
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment, null, "Equipment"));
            }
            return;
        }

        if (regulatingTerminalId != null && regulatingTerminalType != null && regulatingTerminalVlId != null) {
            Terminal newRegulatingTerminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(staticVarCompensator.getNetwork(),
                    regulatingTerminalId.getValue(), regulatingTerminalType.getValue(), regulatingTerminalVlId.getValue());
            staticVarCompensator.setRegulatingTerminal(newRegulatingTerminal);
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel, regulatingTerminalVlId.getValue(), "Voltage level"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    regulatingTerminalType.getValue() + ":" + regulatingTerminalId.getValue(), "Equipment"));
        }
    }

    private void modifyStandbyAutomaton(StaticVarCompensator staticVarCompensator, ReportNode subReportNode) {
        StandbyAutomaton standbyAutomaton = staticVarCompensator.getExtension(StandbyAutomaton.class);
        if (isStandbyAutomatonRemoved()) {
            if (standbyAutomaton != null) {
                staticVarCompensator.removeExtension(StandbyAutomaton.class);
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.StandbyAutomatonRemoved")
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
            }
            return;
        }
        double nominalV = staticVarCompensator.getTerminal().getVoltageLevel().getNominalV();
        List<ReportNode> reports = new ArrayList<>();
        if (standbyAutomaton == null) {
            if (standbyAutomatonOn != null && Boolean.TRUE.equals(standbyAutomatonOn.getValue())) {
                addStandbyAutomaton(staticVarCompensator, nominalV, reports);
            }
        } else {
            modifyExistingStandbyAutomaton(standbyAutomaton, nominalV, reports);
        }
        insertReports(reports, subReportNode, "network.modification.StandbyAutomatonModification");
    }

    private void addStandbyAutomaton(StaticVarCompensator staticVarCompensator, double nominalV, List<ReportNode> reports) {
        boolean newStandby = getFinalStandby(null);
        double newB0 = toSusceptanceModification(b0, q0, nominalV).getValue();
        staticVarCompensator.newExtension(StandbyAutomatonAdder.class)
                .withStandbyStatus(newStandby)
                .withB0(newB0)
                .withLowVoltageSetpoint(lowVoltageSetpoint.getValue())
                .withHighVoltageSetpoint(highVoltageSetpoint.getValue())
                .withLowVoltageThreshold(lowVoltageThreshold.getValue())
                .withHighVoltageThreshold(highVoltageThreshold.getValue())
                .add();
        reports.add(ModificationUtils.getInstance().buildModificationReport(null, newStandby, "Standby"));
        if (b0 != null) {
            reports.add(ModificationUtils.getInstance().buildModificationReport(null, newB0, "Fixed part of susceptance"));
        } else {
            reports.add(ModificationUtils.getInstance().buildModificationReport(null, q0.getValue(), "Fixed part of Q at nominal voltage"));
        }
        reports.add(ModificationUtils.getInstance().buildModificationReport(null, lowVoltageSetpoint.getValue(), "Low voltage set point"));
        reports.add(ModificationUtils.getInstance().buildModificationReport(null, highVoltageSetpoint.getValue(), "High voltage set point"));
        reports.add(ModificationUtils.getInstance().buildModificationReport(null, lowVoltageThreshold.getValue(), "Low voltage threshold"));
        reports.add(ModificationUtils.getInstance().buildModificationReport(null, highVoltageThreshold.getValue(), "High voltage threshold"));
    }

    private void modifyExistingStandbyAutomaton(StandbyAutomaton standbyAutomaton, double nominalV, List<ReportNode> reports) {
        boolean oldStandby = standbyAutomaton.isStandby();
        boolean newStandby = getFinalStandby(standbyAutomaton);
        boolean isVoltageConfigModified = lowVoltageSetpoint != null || highVoltageSetpoint != null
                || lowVoltageThreshold != null || highVoltageThreshold != null;

        if (isVoltageConfigModified && oldStandby) {
            standbyAutomaton.setStandby(false);
        }
        addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(standbyAutomaton::setLowVoltageSetpoint,
                standbyAutomaton::getLowVoltageSetpoint, lowVoltageSetpoint, "Low voltage set point"));
        addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(standbyAutomaton::setHighVoltageSetpoint,
                standbyAutomaton::getHighVoltageSetpoint, highVoltageSetpoint, "High voltage set point"));
        addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(standbyAutomaton::setLowVoltageThreshold,
                standbyAutomaton::getLowVoltageThreshold, lowVoltageThreshold, "Low voltage threshold"));
        addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(standbyAutomaton::setHighVoltageThreshold,
                standbyAutomaton::getHighVoltageThreshold, highVoltageThreshold, "High voltage threshold"));
        if (standbyAutomaton.isStandby() != newStandby) {
            standbyAutomaton.setStandby(newStandby);
        }
        if (standby != null) {
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldStandby, newStandby, "Standby"));
        }

        if (b0 != null) {
            addReport(reports, ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(standbyAutomaton::setB0,
                    standbyAutomaton::getB0, b0, "Fixed part of susceptance"));
        } else if (q0 != null) {
            double oldQ0 = standbyAutomaton.getB0() * Math.pow(nominalV, 2);
            standbyAutomaton.setB0(toSusceptanceModification(null, q0, nominalV).getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldQ0, q0.getValue(), "Fixed part of Q at nominal voltage"));
        }
    }

    private boolean isStandbyAutomatonRemoved() {
        return standbyAutomatonOn != null && Boolean.FALSE.equals(standbyAutomatonOn.getValue());
    }

    private boolean isStandbyAutomatonFieldModified() {
        return standby != null || b0 != null || q0 != null
                || lowVoltageSetpoint != null || highVoltageSetpoint != null
                || lowVoltageThreshold != null || highVoltageThreshold != null;
    }

    private boolean getFinalStandby(StandbyAutomaton standbyAutomaton) {
        if (standby != null && standby.getValue() != null) {
            return standby.getValue();
        }
        return standbyAutomaton != null && standbyAutomaton.isStandby();
    }

    private StaticVarCompensator.RegulationMode getFinalRegulationMode(StaticVarCompensator staticVarCompensator) {
        return regulationMode != null ? regulationMode.getValue() : staticVarCompensator.getRegulationMode();
    }

    private double getFinalNominalV(Network network, StaticVarCompensator staticVarCompensator) {
        if (voltageLevelId != null && voltageLevelId.getValue() != null) {
            return ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId.getValue()).getNominalV();
        }
        return staticVarCompensator.getTerminal().getVoltageLevel().getNominalV();
    }

    private static void checkIsSet(Double value, String errorMessage, String fieldName) {
        if (isNotSet(value)) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "Standby automaton " + fieldName + " is not set");
        }
    }

    private void modifyConnectivity(StaticVarCompensator staticVarCompensator, ReportNode subReportNode) {
        ConnectablePosition<StaticVarCompensator> connectablePosition = staticVarCompensator.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<StaticVarCompensator> connectablePositionAdder = staticVarCompensator.newExtension(ConnectablePositionAdder.class);
        ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, staticVarCompensator, this, subReportNode);
    }

    private boolean getFinalRegulating(StaticVarCompensator staticVarCompensator) {
        if (regulating != null && regulating.getValue() != null) {
            return regulating.getValue();
        }
        return staticVarCompensator.isRegulating();
    }

    /**
     * The susceptance takes precedence over the Q at nominal voltage, as in the creation.
     */
    private static AttributeModification<Double> toSusceptanceModification(AttributeModification<Double> susceptance,
                                                                          AttributeModification<Double> qAtNominalV,
                                                                          double nominalV) {
        if (susceptance != null) {
            return susceptance;
        }
        if (qAtNominalV != null && qAtNominalV.getValue() != null) {
            return new AttributeModification<>(qAtNominalV.getValue() / Math.pow(nominalV, 2), OperationType.SET);
        }
        return null;
    }

    private static Double getFinalValue(AttributeModification<Double> modification, double currentValue) {
        return modification != null ? modification.applyModification(currentValue) : Double.valueOf(currentValue);
    }

    private static boolean isNotSet(Double value) {
        return value == null || Double.isNaN(value);
    }

    private static void addReport(List<ReportNode> reports, ReportNode report) {
        if (report != null) {
            reports.add(report);
        }
    }

    private static void insertReports(List<ReportNode> reports, ReportNode subReportNode, String messageTemplate) {
        if (!reports.isEmpty()) {
            ReportNode reportNode = subReportNode.newReportNode().withMessageTemplate(messageTemplate).add();
            for (ReportNode report : reports) {
                insertReportNode(reportNode, report);
            }
        }
    }
}
