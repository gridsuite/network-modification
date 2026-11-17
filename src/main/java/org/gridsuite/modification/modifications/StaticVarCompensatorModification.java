/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import com.powsybl.iidm.network.extensions.StandbyAutomaton;
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
import static org.gridsuite.modification.error.NetworkModificationExceptionType.STATIC_VAR_COMPENSATOR_NOT_FOUND;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.getFinalValue;
import static org.gridsuite.modification.utils.ModificationUtils.hasValue;
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
                                            AttributeModification<String> regulatingTerminalVlId) {
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
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(equipmentId);
        if (staticVarCompensator == null) {
            throw new NetworkModificationException(STATIC_VAR_COMPENSATOR_NOT_FOUND,
                    "Static var compensator " + equipmentId + " does not exist in network");
        }
        String errorMessage = "Static var compensator '" + equipmentId + "' : ";

        ModificationUtils.getInstance().checkVoltageLevelModification(network, voltageLevelId, busOrBusbarSectionId, staticVarCompensator.getTerminal());
        if (voltageSetpoint != null) {
            checkIsNotNegativeValue(errorMessage, voltageSetpoint.getValue(), MODIFY_STATIC_VAR_COMPENSATOR_ERROR, "voltage set point");
        }
        checkRegulationMode(staticVarCompensator, errorMessage);
        checkLimitsAndSetpoints(network, staticVarCompensator);
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

    private void checkRegulationMode(StaticVarCompensator staticVarCompensator, String errorMessage) {
        if (regulationMode != null && regulationMode.getValue() == null) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "Regulation mode can not be unset");
        }
        StandbyAutomaton standbyAutomaton = staticVarCompensator.getExtension(StandbyAutomaton.class);
        if (regulationMode != null && standbyAutomaton != null && standbyAutomaton.isStandby()
                && getFinalRegulationMode(staticVarCompensator) != StaticVarCompensator.RegulationMode.VOLTAGE) {
            throw new NetworkModificationException(MODIFY_STATIC_VAR_COMPENSATOR_ERROR, errorMessage + "Standby is only supported in Voltage Regulation mode");
        }
    }

    private void checkLimitsAndSetpoints(Network network, StaticVarCompensator staticVarCompensator) {
        double nominalV = staticVarCompensator.getTerminal().getVoltageLevel().getNominalV();
        if (hasValue(voltageLevelId)) {
            nominalV = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId.getValue()).getNominalV();
        }
        Double finalMinSusceptance = null;
        Double finalMaxSusceptance = null;
        Double finalMinQAtNominalV = null;
        Double finalMaxQAtNominalV = null;
        if (isQAtNominalVModification()) {
            // limits are checked in Q so that the error message matches the values entered by the user
            finalMinQAtNominalV = hasValue(minQAtNominalV) ? minQAtNominalV.getValue() : toQAtNominalV(staticVarCompensator.getBmin(), nominalV);
            finalMaxQAtNominalV = hasValue(maxQAtNominalV) ? maxQAtNominalV.getValue() : toQAtNominalV(staticVarCompensator.getBmax(), nominalV);
        } else {
            finalMinSusceptance = getFinalValue(toSusceptanceModification(minSusceptance, minQAtNominalV, nominalV), staticVarCompensator.getBmin());
            finalMaxSusceptance = getFinalValue(toSusceptanceModification(maxSusceptance, maxQAtNominalV, nominalV), staticVarCompensator.getBmax());
        }
        StaticVarCompensator.RegulationMode regulationModeToCheck = null;
        if (getFinalRegulating(staticVarCompensator)) {
            regulationModeToCheck = getFinalRegulationMode(staticVarCompensator);
        }
        // the network values can be NaN : they are considered as not set
        ModificationUtils.getInstance().checkStaticVarCompensatorLimitsAndSetPoints(equipmentId, MODIFY_STATIC_VAR_COMPENSATOR_ERROR,
                finalMinSusceptance, finalMaxSusceptance, finalMinQAtNominalV, finalMaxQAtNominalV,
                regulationModeToCheck,
                nanToNull(getFinalValue(voltageSetpoint, staticVarCompensator.getVoltageSetpoint())),
                nanToNull(getFinalValue(reactivePowerSetpoint, staticVarCompensator.getReactivePowerSetpoint())));
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(equipmentId);
        ModificationUtils.getInstance().applyElementaryModifications(staticVarCompensator::setName,
                () -> staticVarCompensator.getOptionalName().orElse("No value"), equipmentName, subReportNode, "Name");
        // the feeder bay is moved first : the limits in Q are converted with the nominal voltage of the new voltage level
        modifyConnectivity(staticVarCompensator, subReportNode);
        modifyLimitsAndSetpoints(staticVarCompensator, subReportNode);
        updateMeasurements(staticVarCompensator, subReportNode);
        PropertiesUtils.applyProperties(staticVarCompensator, subReportNode, properties, "network.modification.StaticVarCompensatorProperties");
    }

    @Override
    public String getName() {
        return ModificationType.STATIC_VAR_COMPENSATOR_MODIFICATION.name();
    }

    private void modifyConnectivity(StaticVarCompensator staticVarCompensator, ReportNode subReportNode) {
        List<ReportNode> connectivityReports = ModificationUtils.getInstance().moveFeederBayAndReturnReports(staticVarCompensator,
                staticVarCompensator.getTerminal(), voltageLevelId, busOrBusbarSectionId);
        ConnectablePosition<StaticVarCompensator> connectablePosition = staticVarCompensator.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<StaticVarCompensator> connectablePositionAdder = staticVarCompensator.newExtension(ConnectablePositionAdder.class);
        ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder,
                staticVarCompensator, this, subReportNode, connectivityReports);
    }

    private void modifyLimitsAndSetpoints(StaticVarCompensator staticVarCompensator, ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        modifyLimits(staticVarCompensator, reports);
        modifyRegulation(staticVarCompensator, reports);
        modifyRegulatingTerminal(staticVarCompensator, reports);
        ModificationUtils.getInstance().reportModifications(subReportNode, reports, "network.modification.LimitsAndSetpoints");
    }

    private void modifyLimits(StaticVarCompensator staticVarCompensator, List<ReportNode> reports) {
        double nominalV = staticVarCompensator.getTerminal().getVoltageLevel().getNominalV();
        if (isQAtNominalVModification()) {
            // reported in Q, as entered by the user
            reports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                    q -> staticVarCompensator.setBmin(toSusceptance(q, nominalV)), () -> toQAtNominalV(staticVarCompensator.getBmin(), nominalV),
                    hasValue(minQAtNominalV) ? minQAtNominalV : null, "Q min at nominal voltage"));
            reports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                    q -> staticVarCompensator.setBmax(toSusceptance(q, nominalV)), () -> toQAtNominalV(staticVarCompensator.getBmax(), nominalV),
                    hasValue(maxQAtNominalV) ? maxQAtNominalV : null, "Q max at nominal voltage"));
            return;
        }
        reports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(staticVarCompensator::setBmin, staticVarCompensator::getBmin,
                toSusceptanceModification(minSusceptance, minQAtNominalV, nominalV), "Susceptance min"));
        reports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(staticVarCompensator::setBmax, staticVarCompensator::getBmax,
                toSusceptanceModification(maxSusceptance, maxQAtNominalV, nominalV), "Susceptance max"));
    }

    private void modifyRegulation(StaticVarCompensator staticVarCompensator, List<ReportNode> reports) {
        if (voltageSetpoint == null && reactivePowerSetpoint == null && regulationMode == null && regulating == null) {
            return;
        }
        boolean oldRegulating = staticVarCompensator.isRegulating();
        boolean finalRegulating = getFinalRegulating(staticVarCompensator);

        staticVarCompensator.setRegulating(false);
        reports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                value -> staticVarCompensator.setVoltageSetpoint(value != null ? value : Double.NaN),
                staticVarCompensator::getVoltageSetpoint, voltageSetpoint, "Voltage set point"));
        reports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                value -> staticVarCompensator.setReactivePowerSetpoint(value != null ? value : Double.NaN),
                staticVarCompensator::getReactivePowerSetpoint, reactivePowerSetpoint, "Reactive power set point"));
        reports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(staticVarCompensator::setRegulationMode,
                staticVarCompensator::getRegulationMode, regulationMode, "Regulation mode"));
        staticVarCompensator.setRegulating(finalRegulating);
        if (oldRegulating != finalRegulating) {
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

    private boolean getFinalRegulating(StaticVarCompensator staticVarCompensator) {
        if (hasValue(regulating)) {
            return regulating.getValue();
        }
        return staticVarCompensator.isRegulating();
    }

    private StaticVarCompensator.RegulationMode getFinalRegulationMode(StaticVarCompensator staticVarCompensator) {
        if (regulationMode != null) {
            return regulationMode.getValue();
        }
        return staticVarCompensator.getRegulationMode();
    }

    /**
     * The limits are entered in Q at nominal voltage (and not in susceptance) when no susceptance is modified.
     */
    private boolean isQAtNominalVModification() {
        return minSusceptance == null && maxSusceptance == null && (hasValue(minQAtNominalV) || hasValue(maxQAtNominalV));
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
        if (hasValue(qAtNominalV)) {
            return new AttributeModification<>(toSusceptance(qAtNominalV.getValue(), nominalV), OperationType.SET);
        }
        return null;
    }

    private static double toSusceptance(double qAtNominalV, double nominalV) {
        return qAtNominalV / Math.pow(nominalV, 2);
    }

    private static double toQAtNominalV(double susceptance, double nominalV) {
        return susceptance * Math.pow(nominalV, 2);
    }

    private static Double nanToNull(Double value) {
        return value == null || Double.isNaN(value) ? null : value;
    }
}
