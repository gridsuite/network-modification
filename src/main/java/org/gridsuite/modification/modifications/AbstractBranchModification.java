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
import com.powsybl.iidm.modification.topology.RemoveFeederBay;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.*;
import java.util.stream.Collectors;

import static com.powsybl.iidm.network.TwoSides.*;
import static org.gridsuite.modification.NetworkModificationException.Type.BRANCH_MODIFICATION_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.createBranchInNodeBreaker;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public abstract class AbstractBranchModification extends AbstractModification {

    private static final String DURATION = "duration";
    private static final String NAME = "name";
    private static final String VALUE = "value";
    private static final String VALIDITY = "validity";
    protected final BranchModificationInfos modificationInfos;

    protected AbstractBranchModification(BranchModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    protected void modifyBranch(Branch<?> branch, BranchModificationInfos branchModificationInfos, ReportNode subReportNode, String reporterKey, String reporterDefaultMessage) {
        subReportNode.newReportNode()
                .withMessageTemplate(reporterKey, reporterDefaultMessage)
                .withUntypedValue("id", branchModificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        if (branchModificationInfos.getEquipmentName() != null) {
            insertReportNode(subReportNode, ModificationUtils.getInstance().buildModificationReport(Optional.of(branch.getOptionalName()).orElse(null), branchModificationInfos.getEquipmentName().getValue(), "Name", 0));
            branch.setName(branchModificationInfos.getEquipmentName().getValue());
        }
        modifyBranchVoltageLevelBusOrBusBarSectionAttributes(modificationInfos, branch, subReportNode);
        modifyBranchConnectivityAttributes(branchModificationInfos, branch, subReportNode);

        if (characteristicsModified(branchModificationInfos)) {
            modifyCharacteristics(branch, branchModificationInfos, subReportNode);
        }

        CurrentLimitsModificationInfos currentLimitsInfos1 = modificationInfos.getCurrentLimits1();
        CurrentLimitsModificationInfos currentLimitsInfos2 = modificationInfos.getCurrentLimits2();
        List<ReportNode> side1LimitsReports = new ArrayList<>();
        CurrentLimits currentLimits1 = branch.getCurrentLimits1().orElse(null);
        if (currentLimitsInfos1 != null) {
            modifyCurrentLimits(currentLimitsInfos1, branch.newCurrentLimits1(), currentLimits1, side1LimitsReports);
        }
        List<ReportNode> side2LimitsReports = new ArrayList<>();
        CurrentLimits currentLimits2 = branch.getCurrentLimits2().orElse(null);
        if (currentLimitsInfos2 != null) {
            modifyCurrentLimits(currentLimitsInfos2, branch.newCurrentLimits2(), currentLimits2, side2LimitsReports);
        }
        if (!side1LimitsReports.isEmpty() || !side2LimitsReports.isEmpty()) {
            ReportNode limitsReportNode = subReportNode.newReportNode().withMessageTemplate("limits", "Limits").add();
            ModificationUtils.getInstance().reportModifications(limitsReportNode, side1LimitsReports, "side1LimitsModification",
                    "    Side 1");
            ModificationUtils.getInstance().reportModifications(limitsReportNode, side2LimitsReports, "side2LimitsModification",
                    "    Side 2");
        }

        updateConnections(branch, branchModificationInfos);

        updateMeasurements(branch, branchModificationInfos, subReportNode);
    }

    private void updateMeasurements(Branch<?> branch, BranchModificationInfos branchModificationInfos, ReportNode subReportNode) {
        Double p1Value = branchModificationInfos.getP1MeasurementValue() != null ? branchModificationInfos.getP1MeasurementValue().getValue() : null;
        Double q1Value = branchModificationInfos.getQ1MeasurementValue() != null ? branchModificationInfos.getQ1MeasurementValue().getValue() : null;
        Double p2Value = branchModificationInfos.getP2MeasurementValue() != null ? branchModificationInfos.getP2MeasurementValue().getValue() : null;
        Double q2Value = branchModificationInfos.getQ2MeasurementValue() != null ? branchModificationInfos.getQ2MeasurementValue().getValue() : null;
        Boolean p1Validity = branchModificationInfos.getP1MeasurementValidity() != null ? branchModificationInfos.getP1MeasurementValidity().getValue() : null;
        Boolean q1Validity = branchModificationInfos.getQ1MeasurementValidity() != null ? branchModificationInfos.getQ1MeasurementValidity().getValue() : null;
        Boolean p2Validity = branchModificationInfos.getP2MeasurementValidity() != null ? branchModificationInfos.getP2MeasurementValidity().getValue() : null;
        Boolean q2Validity = branchModificationInfos.getQ2MeasurementValidity() != null ? branchModificationInfos.getQ2MeasurementValidity().getValue() : null;
        if (p1Value == null && p1Validity == null && q1Value == null && q1Validity == null && p2Value == null && p2Validity == null && q2Value == null && q2Validity == null) {
            // no measurement modification requested
            return;
        }
        Measurements<?> measurements = (Measurements<?>) branch.getExtension(Measurements.class);
        if (measurements == null) {
            MeasurementsAdder<?> measurementsAdder = branch.newExtension(MeasurementsAdder.class);
            measurements = measurementsAdder.add();
        }
        // Side 1 measurements update
        List<ReportNode> side1Reports = new ArrayList<>();
        upsertMeasurement(measurements, Measurement.Type.ACTIVE_POWER, ThreeSides.ONE, p1Value, p1Validity, side1Reports);
        upsertMeasurement(measurements, Measurement.Type.REACTIVE_POWER, ThreeSides.ONE, q1Value, q1Validity, side1Reports);
        // Side 2 measurements update
        List<ReportNode> side2Reports = new ArrayList<>();
        upsertMeasurement(measurements, Measurement.Type.ACTIVE_POWER, ThreeSides.TWO, p2Value, p2Validity, side2Reports);
        upsertMeasurement(measurements, Measurement.Type.REACTIVE_POWER, ThreeSides.TWO, q2Value, q2Validity, side2Reports);
        // report changes
        ReportNode estimSubReportNode = null;
        if (!side1Reports.isEmpty() || !side2Reports.isEmpty()) {
            estimSubReportNode = subReportNode.newReportNode().withMessageTemplate("measurements", "State estimation").add();
        }
        if (!side1Reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(estimSubReportNode, side1Reports, "measurementsSide1", "Side 1");
        }
        if (!side2Reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(estimSubReportNode, side2Reports, "measurementsSide2", "Side 2");
        }
    }

    private void upsertMeasurement(Measurements<?> measurements, Measurement.Type type, ThreeSides side, Double value, Boolean validity, List<ReportNode> reports) {
        if (value == null && validity == null) {
            return;
        }
        String measurementType = (type == Measurement.Type.ACTIVE_POWER ? "Active power" : "Reactive power") + " measurement ";
        Measurement m = getExistingMeasurement(measurements, type, side);
        if (m != null) { // update measurement
            if (value != null) {
                double oldValue = m.getValue();
                m.setValue(value);
                reports.add(ModificationUtils.buildModificationReport(oldValue, value, measurementType + VALUE, 1, TypedValue.INFO_SEVERITY));
            }
            if (validity != null) {
                boolean oldValidity = m.isValid();
                m.setValid(validity);
                reports.add(ModificationUtils.buildModificationReport(oldValidity, validity, measurementType + VALIDITY, 1, TypedValue.INFO_SEVERITY));
            }
        } else { // add new measurement
            var mAdder = measurements.newMeasurement().setId(UUID.randomUUID().toString()).setType(type).setSide(side);
            if (value != null) {
                mAdder.setValue(value);
                reports.add(ModificationUtils.buildModificationReport(null, value, measurementType + VALUE, 1, TypedValue.INFO_SEVERITY));
            }
            if (validity != null) {
                mAdder.setValid(validity);
                reports.add(ModificationUtils.buildModificationReport(null, validity, measurementType + VALIDITY, 1, TypedValue.INFO_SEVERITY));
            }
            mAdder.add();
        }
    }

    private Measurement getExistingMeasurement(Measurements<?> measurements, Measurement.Type type, ThreeSides side) {
        return measurements.getMeasurements(type).stream().filter(m -> m.getSide() == side).findFirst().orElse(null);
    }

    private void updateConnections(Branch<?> branch, BranchModificationInfos branchModificationInfos) {
        List<TwoSides> errorSides = new ArrayList<>();
        List<String> errorTypes = new ArrayList<>();
        if (branchModificationInfos.getTerminal1Connected() != null && !updateConnection(branch, TwoSides.ONE, modificationInfos.getTerminal1Connected().getValue())) {
            errorSides.add(TwoSides.ONE);
            errorTypes.add(Boolean.TRUE.equals(modificationInfos.getTerminal1Connected().getValue()) ? "connect" : "disconnect");
        }
        if (branchModificationInfos.getTerminal2Connected() != null && !updateConnection(branch, TwoSides.TWO, modificationInfos.getTerminal2Connected().getValue())) {
            errorSides.add(TwoSides.TWO);
            errorTypes.add(Boolean.TRUE.equals(modificationInfos.getTerminal2Connected().getValue()) ? "connect" : "disconnect");
        }
        if (!errorSides.isEmpty()) {
            throw new NetworkModificationException(BRANCH_MODIFICATION_ERROR,
                String.format("Could not %s equipment '%s' on side %s",
                    errorTypes.stream().distinct().collect(Collectors.joining("/")),
                    branch.getId(),
                    errorSides.stream().map(Enum::toString).collect(Collectors.joining("/"))));
        }
    }

    private boolean updateConnection(Branch<?> branch, TwoSides side, Boolean connectionChange) {
        boolean done = true;
        if (branch.getTerminal(side).isConnected() && Boolean.FALSE.equals(connectionChange)) {
            branch.getTerminal(side).disconnect();
            if (branch.getTerminal(side).isConnected()) {
                done = false;
            }
        } else if (!branch.getTerminal(side).isConnected() && Boolean.TRUE.equals(connectionChange)) {
            branch.getTerminal(side).connect();
            if (!branch.getTerminal(side).isConnected()) {
                done = false;
            }
        }
        return done;
    }

    protected void modifyCurrentLimits(CurrentLimitsModificationInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder, CurrentLimits currentLimits, List<ReportNode> limitsReports) {
        boolean hasPermanent = currentLimitsInfos.getPermanentLimit() != null;
        if (hasPermanent) {
            limitsReports.add(ModificationUtils.getInstance().buildModificationReport(currentLimits != null ? currentLimits.getPermanentLimit() : Double.NaN,
                    currentLimitsInfos.getPermanentLimit(), "IST", 2));
            limitsAdder.setPermanentLimit(currentLimitsInfos.getPermanentLimit());
        } else {
            if (currentLimits != null) {
                limitsAdder.setPermanentLimit(currentLimits.getPermanentLimit());
            }
        }
        modifyTemporaryLimits(currentLimitsInfos, limitsAdder, currentLimits, limitsReports);
        limitsAdder.add();
    }

    protected void modifyTemporaryLimits(CurrentLimitsModificationInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder,
                                         CurrentLimits currentLimits, List<ReportNode> limitsReports) {
        // we create a mutable list of temporary limits to be able to remove the limits that are modified in current modification
        List<LoadingLimits.TemporaryLimit> branchTemporaryLimits = new ArrayList<>();
        if (currentLimits != null) {
            branchTemporaryLimits.addAll(currentLimits.getTemporaryLimits());
        }
        List<ReportNode> temporaryLimitsReports = new ArrayList<>();
        for (CurrentTemporaryLimitModificationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
            int limitAcceptableDuration = limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration();
            double limitValue = limit.getValue() == null ? Double.MAX_VALUE : limit.getValue();
            String limitDurationToReport = limitAcceptableDuration == Integer.MAX_VALUE ? " " : String.valueOf(limitAcceptableDuration);
            String limitValueToReport = limitValue == Double.MAX_VALUE ? "no value" : String.valueOf(limitValue);
            LoadingLimits.TemporaryLimit limitToModify = null;
            if (currentLimits != null) {
                limitToModify = currentLimits.getTemporaryLimit(limitAcceptableDuration);
                if (limitToModify != null && !limitToModify.getName().equals(limit.getName())) {
                    throw new PowsyblException("2temporary limits have the same duration " + limitAcceptableDuration);
                }
                // we remove the limit to modify from the list of temporary limits so we can get the list of temporary limits coming from previous modifications
                branchTemporaryLimits.removeIf(temporaryLimit -> temporaryLimit.getAcceptableDuration() == limitAcceptableDuration);
            }
            if (limitToModify == null && limit.getModificationType() == TemporaryLimitModificationType.ADDED) {
                temporaryLimitsReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("temporaryLimitAdded" + limit.getName(), "            ${name} (${duration}) added with ${value}")
                        .withUntypedValue(NAME, limit.getName())
                        .withUntypedValue(DURATION, limitDurationToReport)
                        .withUntypedValue(VALUE, limitValueToReport)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());

            } else if (limitToModify != null) {
                if (limit.getModificationType() == TemporaryLimitModificationType.DELETED) {
                    temporaryLimitsReports.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("temporaryLimitDeleted" + limit.getName(), "            ${name} (${duration}) deleted")
                            .withUntypedValue(NAME, limit.getName())
                            .withUntypedValue(DURATION, limitDurationToReport)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                    continue;
                } else if (Double.compare(limitToModify.getValue(), limitValue) != 0 && limit.getModificationType() != null) {
                    temporaryLimitsReports.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("temporaryLimitModified" + limit.getName(), "            ${name} (${duration}) : ${oldValue} -> ${value}")
                            .withUntypedValue(NAME, limit.getName())
                            .withUntypedValue(DURATION, limitDurationToReport)
                            .withUntypedValue(VALUE, limitValueToReport)
                            .withUntypedValue("oldValue",
                                    limitToModify.getValue() == Double.MAX_VALUE ? "no value"
                                            : String.valueOf(limitToModify.getValue()))
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                } else {
                    limitValue = limitToModify.getValue();
                }
            } else {
                continue;
            }
            limitsAdder
                    .beginTemporaryLimit()
                    .setName(limit.getName())
                    .setValue(limitValue)
                    .setAcceptableDuration(limitAcceptableDuration)
                    .endTemporaryLimit();
        }
        // we add the temporary limits comming from previous modifications
        if (!branchTemporaryLimits.isEmpty()) {
            for (LoadingLimits.TemporaryLimit limit : branchTemporaryLimits) {
                limitsAdder
                        .beginTemporaryLimit()
                        .setName(limit.getName())
                        .setValue(limit.getValue())
                        .setAcceptableDuration(limit.getAcceptableDuration())
                        .endTemporaryLimit();
            }
        }
        if (!temporaryLimitsReports.isEmpty()) {
            temporaryLimitsReports.add(0, ReportNode.newRootReportNode()
                    .withMessageTemplate("temporaryLimitsModification", "            Temporary current limits :")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            limitsReports.addAll(temporaryLimitsReports);
        }
    }

    protected boolean characteristicsModified(BranchModificationInfos branchModificationInfos) {
        return branchModificationInfos.getX() != null
                && branchModificationInfos.getX().getValue() != null
                || branchModificationInfos.getR() != null
                && branchModificationInfos.getR().getValue() != null;
    }

    protected abstract void modifyCharacteristics(Branch<?> branch, BranchModificationInfos branchModificationInfos,
            ReportNode subReportNode);

    private void modifyBranchVoltageLevelBusOrBusBarSectionAttributes(BranchModificationInfos modificationInfos,
                                                                      Branch<?> branch, ReportNode subReportNode) {
        if (ModificationUtils.getInstance().isNotModificationVoltageLevel1BusOrBusBar1Infos(modificationInfos) &&
                ModificationUtils.getInstance().isNotModificationVoltageLevel2BusOrBusBar2Infos(modificationInfos)) {
            return;
        }
        Network network = branch.getNetwork();
        BranchCreationInfos branchCreationInfos = createBranchCreationInfos(modificationInfos, branch, subReportNode);
        Map<String, String> properties = !branch.hasProperty()
                ? null
                : branch.getPropertyNames().stream().collect(Collectors.toMap(name -> name, branch::getProperty));
        new RemoveFeederBay(branch.getId()).apply(network, true, subReportNode);
        if (branch instanceof Line && branchCreationInfos != null) {
            createLine((LineCreationInfos) branchCreationInfos, subReportNode, network);
            var newLine = ModificationUtils.getInstance().getLine(network, modificationInfos.getEquipmentId());
            if (properties != null) {
                properties.forEach(newLine::setProperty);
            }
        } else if (branch instanceof TwoWindingsTransformer && branchCreationInfos != null) {
            createTransformer((TwoWindingsTransformerCreationInfos) branchCreationInfos, subReportNode, network);
            var newTransformer = ModificationUtils.getInstance().getTwoWindingsTransformer(network, modificationInfos.getEquipmentId());
            if (properties != null) {
                properties.forEach(newTransformer::setProperty);
            }
        }

    }

    private void createLine(LineCreationInfos modificationInfos, ReportNode subReportNode, Network network) {
        VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId1());
        VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId2());

        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER &&
                voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LineAdder lineAdder = ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, modificationInfos, false, false);
            createBranchInNodeBreaker(voltageLevel1, voltageLevel2, modificationInfos.getBusOrBusbarSectionId1(), modificationInfos.getBusOrBusbarSectionId2(),
                    modificationInfos.getConnectionPosition1(), modificationInfos.getConnectionPosition2(), modificationInfos.getConnectionDirection1(),
                    modificationInfos.getConnectionDirection2(), modificationInfos.getConnectionName1() != null ? modificationInfos.getConnectionName1() : modificationInfos.getEquipmentId(),
                    modificationInfos.getConnectionName2() != null ? modificationInfos.getConnectionName2() : modificationInfos.getEquipmentId(),
                    network, lineAdder, subReportNode);
        } else {
            ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, modificationInfos, false, false).add();
        }
        Line line = network.getLine(modificationInfos.getEquipmentId());
        List<OperationalLimitsGroupInfos> opLimitsGroupSide1 = modificationInfos.getOperationalLimitsGroups1();
        List<OperationalLimitsGroupInfos> opLimitsGroupSide2 = modificationInfos.getOperationalLimitsGroups2();
        if (!CollectionUtils.isEmpty(opLimitsGroupSide1)) {
            ModificationUtils.getInstance().setCurrentLimitsOnASide(opLimitsGroupSide1, line, ONE, subReportNode);
        }
        if (!CollectionUtils.isEmpty(opLimitsGroupSide2)) {
            ModificationUtils.getInstance().setCurrentLimitsOnASide(opLimitsGroupSide2, line, TWO, subReportNode);
        }
        // properties
        if (modificationInfos.getSelectedOperationalLimitsGroup1() != null) {
            line.setSelectedOperationalLimitsGroup1(modificationInfos.getSelectedOperationalLimitsGroup1());
        }
        if (modificationInfos.getSelectedOperationalLimitsGroup2() != null) {
            line.setSelectedOperationalLimitsGroup2(modificationInfos.getSelectedOperationalLimitsGroup2());
        }
    }

    private void createTransformer(TwoWindingsTransformerCreationInfos modificationInfos, ReportNode subReportNode, Network network) {
        VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId1());
        VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId2());

        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER && voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            var twoWindingsTransformerAdder = ModificationUtils.getInstance().createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, modificationInfos, false, false);
            createBranchInNodeBreaker(voltageLevel1, voltageLevel2, modificationInfos.getBusOrBusbarSectionId1(), modificationInfos.getBusOrBusbarSectionId2(),
                    modificationInfos.getConnectionPosition1(), modificationInfos.getConnectionPosition2(), modificationInfos.getConnectionDirection1(),
                    modificationInfos.getConnectionDirection2(), modificationInfos.getConnectionName1() != null ? modificationInfos.getConnectionName1() : modificationInfos.getEquipmentId(),
                    modificationInfos.getConnectionName2() != null ? modificationInfos.getConnectionName2() : modificationInfos.getEquipmentId(),
                    network, twoWindingsTransformerAdder, subReportNode);

            var twt = network.getTwoWindingsTransformer(modificationInfos.getEquipmentId());
            addTapChangersToTwoWindingsTransformer(modificationInfos, twt);
        } else {
            var twt = ModificationUtils.getInstance().createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, modificationInfos, false, false).add();
            addTapChangersToTwoWindingsTransformer(modificationInfos, twt);
        }
        Line line = network.getLine(modificationInfos.getEquipmentId());
        List<OperationalLimitsGroupInfos> opLimitsGroupSide1 = modificationInfos.getOperationalLimitsGroups1();
        List<OperationalLimitsGroupInfos> opLimitsGroupSide2 = modificationInfos.getOperationalLimitsGroups2();
        if (!CollectionUtils.isEmpty(opLimitsGroupSide1)) {
            ModificationUtils.getInstance().setCurrentLimitsOnASide(opLimitsGroupSide1, line, ONE, subReportNode);
        }
        if (!CollectionUtils.isEmpty(opLimitsGroupSide2)) {
            ModificationUtils.getInstance().setCurrentLimitsOnASide(opLimitsGroupSide2, line, TWO, subReportNode);
        }
        // properties
        if (modificationInfos.getSelectedOperationalLimitsGroup1() != null) {
            line.setSelectedOperationalLimitsGroup1(modificationInfos.getSelectedOperationalLimitsGroup1());
        }
        if (modificationInfos.getSelectedOperationalLimitsGroup2() != null) {
            line.setSelectedOperationalLimitsGroup2(modificationInfos.getSelectedOperationalLimitsGroup2());
        }
    }

    private void addTapChangersToTwoWindingsTransformer(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, TwoWindingsTransformer twt) {
        if (twoWindingsTransformerCreationInfos.getRatioTapChanger() != null) {
            ModificationUtils.getInstance().addRatioTapChangersToTwoWindingsTransformer(twoWindingsTransformerCreationInfos, twt);
        }

        if (twoWindingsTransformerCreationInfos.getPhaseTapChanger() != null) {
            ModificationUtils.getInstance().addPhaseTapChangersToTwoWindingsTransformer(twoWindingsTransformerCreationInfos, twt);
        }
    }

    private BranchCreationInfos createBranchCreationInfos(BranchModificationInfos modificationInfos, Branch branch, ReportNode subReportNode) {
        VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevelInfos(modificationInfos.getVoltageLevelId1(), branch.getTerminal1(),
                branch.getNetwork(), ModificationUtils.FeederSide.BRANCH_SIDE_ONE, subReportNode);
        String busOrBusbarSectionId1 = ModificationUtils.getInstance().getBusOrBusBarSectionInfos(modificationInfos.getBusOrBusbarSectionId1(),
                branch.getTerminal1(), ModificationUtils.FeederSide.BRANCH_SIDE_ONE, subReportNode);

        VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevelInfos(modificationInfos.getVoltageLevelId2(), branch.getTerminal2(),
                branch.getNetwork(), ModificationUtils.FeederSide.BRANCH_SIDE_TWO, subReportNode);
        String busOrBusbarSectionId2 = ModificationUtils.getInstance().getBusOrBusBarSectionInfos(modificationInfos.getBusOrBusbarSectionId2(),
                branch.getTerminal2(), ModificationUtils.FeederSide.BRANCH_SIDE_TWO, subReportNode);
        if (branch instanceof Line line) {
            ConnectablePosition position = line.getExtension(ConnectablePosition.class);
            ConnectablePosition.Feeder feeder1 = (position != null) ? position.getFeeder1() : null;
            ConnectablePosition.Feeder feeder2 = (position != null) ? position.getFeeder2() : null;

            return LineCreationInfos.builder()
                    .equipmentId(modificationInfos.getEquipmentId())
                    .equipmentName(modificationInfos.getEquipmentName() != null
                            ? modificationInfos.getEquipmentName().getValue()
                            : branch.getNameOrId())
                    .voltageLevelId1(voltageLevel1.getId())
                    .busOrBusbarSectionId1(busOrBusbarSectionId1)
                    .voltageLevelId2(voltageLevel2.getId())
                    .busOrBusbarSectionId2(busOrBusbarSectionId2)
                    .connectionName1((feeder1 != null)
                            ? feeder1.getName().orElse(modificationInfos.getEquipmentId())
                            : modificationInfos.getEquipmentId())
                    .connectionDirection1((feeder1 != null)
                            ? feeder1.getDirection()
                            : ConnectablePosition.Direction.UNDEFINED)
                    .connectionPosition1(null)
                    .connectionName2((feeder2 != null)
                            ? feeder2.getName().orElse(modificationInfos.getEquipmentId())
                            : modificationInfos.getEquipmentId())
                    .connectionDirection2((feeder2 != null)
                            ? feeder2.getDirection()
                            : ConnectablePosition.Direction.UNDEFINED)
                    .connectionPosition2(null)
                    .connected1(line.getTerminal1().isConnected())
                    .connected2(line.getTerminal2().isConnected())
                    .r(line.getR())
                    .x(line.getX())
                    .g1(line.getG1())
                    .b1(line.getB1())
                    .g2(line.getG2())
                    .b2(line.getB2())
                    .operationalLimitsGroups1(mapOperationalLimits(line.getOperationalLimitsGroups1().stream().toList()))
                    .operationalLimitsGroups2(mapOperationalLimits(line.getOperationalLimitsGroups2().stream().toList()))
                    .build();
        }
        if (branch instanceof TwoWindingsTransformer transformer) {
            ConnectablePosition position = transformer.getExtension(ConnectablePosition.class);
            ConnectablePosition.Feeder feeder1 = (position != null) ? position.getFeeder1() : null;
            ConnectablePosition.Feeder feeder2 = (position != null) ? position.getFeeder2() : null;
            return TwoWindingsTransformerCreationInfos.builder()
                    .equipmentId(modificationInfos.getEquipmentId())
                    .equipmentName(modificationInfos.getEquipmentName() != null
                            ? modificationInfos.getEquipmentName().getValue()
                            : branch.getNameOrId())
                    .voltageLevelId1(voltageLevel1.getId())
                    .busOrBusbarSectionId1(busOrBusbarSectionId1)
                    .voltageLevelId2(voltageLevel2.getId())
                    .busOrBusbarSectionId2(busOrBusbarSectionId2)
                    .connectionName1((feeder1 != null)
                            ? feeder1.getName().orElse(modificationInfos.getEquipmentId())
                            : modificationInfos.getEquipmentId())
                    .connectionDirection1((feeder1 != null)
                            ? feeder1.getDirection()
                            : ConnectablePosition.Direction.UNDEFINED)
                    .connectionPosition1(null)
                    .connectionName2((feeder2 != null)
                            ? feeder2.getName().orElse(modificationInfos.getEquipmentId())
                            : modificationInfos.getEquipmentId())
                    .connectionDirection2((feeder2 != null)
                            ? feeder2.getDirection()
                            : ConnectablePosition.Direction.UNDEFINED)
                    .connectionPosition2(null)
                    .connected1(transformer.getTerminal1().isConnected())
                    .connected2(transformer.getTerminal2().isConnected())
                    .r(transformer.getR())
                    .x(transformer.getX())
                    .g(transformer.getG())
                    .b(transformer.getB())
                    .ratedU1(transformer.getRatedU1())
                    .ratedU2(transformer.getRatedU2())
                    .ratedS(transformer.getRatedS())
                    .operationalLimitsGroups1(mapOperationalLimits(transformer.getOperationalLimitsGroups1().stream().toList()))
                    .operationalLimitsGroups2(mapOperationalLimits(transformer.getOperationalLimitsGroups2().stream().toList()))
                    .build();
        }
        return null;
    }

    private List<OperationalLimitsGroupInfos> mapOperationalLimits(List<OperationalLimitsGroup> limitsGroups) {
        return (List<OperationalLimitsGroupInfos>) limitsGroups.stream()
                .map(limitsGroup -> OperationalLimitsGroupInfos.builder()
                        .id(limitsGroup.getId())
                        .currentLimits(limitsGroup.getCurrentLimits()
                                .map(currentGroup -> CurrentLimitsInfos.builder()
                                        .permanentLimit(currentGroup.getPermanentLimit())
                                        .temporaryLimits(currentGroup.getTemporaryLimits().stream()
                                                .map(limit -> CurrentTemporaryLimitCreationInfos.builder()
                                                        .name(limit.getName())
                                                        .value(limit.getValue())
                                                        .acceptableDuration(limit.getAcceptableDuration())
                                                        .build())
                                                .toList())
                                        .build())
                                .orElse(null))
                        .build())
                .toList();
    }

    private ReportNode modifyBranchConnectivityAttributes(BranchModificationInfos branchModificationInfos,
                                                          Branch<?> branch, ReportNode subReportNode) {
        ConnectablePosition<?> connectablePosition = (ConnectablePosition<?>) branch.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<?> connectablePositionAdder = branch.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyBranchConnectivityAttributes(connectablePosition, connectablePositionAdder, branch, branchModificationInfos, subReportNode);
    }
}
