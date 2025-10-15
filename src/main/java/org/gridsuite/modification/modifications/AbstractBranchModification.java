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
import com.powsybl.iidm.network.extensions.*;
import jakarta.validation.constraints.NotNull;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.gridsuite.modification.NetworkModificationException.Type.BRANCH_MODIFICATION_ERROR;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.*;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public abstract class AbstractBranchModification extends AbstractModification {

    private static final String DURATION = "duration";
    private static final String NAME = "name";
    private static final String VALUE = "value";
    private static final String VALIDITY = "validity";
    private static final String LIMIT_ACCEPTABLE_DURATION = "limitAcceptableDuration";
    private static final String OPERATIONAL_LIMITS_GROUP_NAME = "operationalLimitsGroupName";
    private static final String SIDE = "side";

    protected final BranchModificationInfos modificationInfos;

    protected AbstractBranchModification(BranchModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    protected void modifyBranch(Branch<?> branch, BranchModificationInfos branchModificationInfos, ReportNode subReportNode, String reporterKey) {
        subReportNode.newReportNode()
                .withMessageTemplate(reporterKey)
                .withUntypedValue("id", branchModificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        if (branchModificationInfos.getEquipmentName() != null) {
            insertReportNode(subReportNode, ModificationUtils.getInstance().buildModificationReport(Optional.of(branch.getOptionalName()).orElse(null), branchModificationInfos.getEquipmentName().getValue(), "Name"));
            branch.setName(branchModificationInfos.getEquipmentName().getValue());
        }

        modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide1(modificationInfos, branch, subReportNode);
        modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide2(modificationInfos, branch, subReportNode);
        modifyBranchConnectivityAttributes(branchModificationInfos, branch, subReportNode);

        if (characteristicsModified(branchModificationInfos)) {
            modifyCharacteristics(branch, branchModificationInfos, subReportNode);
        }

        List<ReportNode> activeOLGReports = new ArrayList<>();
        List<ReportNode> olgReports = new ArrayList<>();

        boolean modifyOLG = branchModificationInfos.getEnableOLGModification() == null
                || branchModificationInfos.getEnableOLGModification();
        if (modifyOLG && branchModificationInfos.getOperationalLimitsGroups() != null) {
            modifyOperationalLimitsGroups(branch, branchModificationInfos.getOperationalLimitsGroups(), olgReports);
        }

        applySelectedOLGs(branch, activeOLGReports);

        if (!activeOLGReports.isEmpty() || !olgReports.isEmpty()) {
            ReportNode limitsReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
            ModificationUtils.getInstance().reportModifications(limitsReportNode, activeOLGReports, "network.modification.activeLimitsSets");
            ModificationUtils.getInstance().reportModifications(limitsReportNode, olgReports, "network.modification.limitsSets");
        }
        updateConnections(branch, branchModificationInfos);
    }

    private void copyOperationalLimitsOnSide(OperationalLimitsGroup opLimitsGroup, OperationalLimitsGroup opLimitGroupToCopy) {
        // Copy all limits of the other side
        opLimitGroupToCopy.getCurrentLimits().ifPresent(currentLimits -> {
            CurrentLimitsAdder adder = opLimitsGroup.newCurrentLimits()
                .setPermanentLimit(currentLimits.getPermanentLimit());

            for (LoadingLimits.TemporaryLimit tempLimit : currentLimits.getTemporaryLimits()) {
                addTemporaryLimit(adder, tempLimit.getName(), tempLimit.getValue(), tempLimit.getAcceptableDuration());
            }
        });
    }

    private void copyAndDeleteLimitSet(Branch<?> branch, List<OperationalLimitsGroupModificationInfos> modificationInfos, String modifiedLimitSet,
                                        OperationalLimitsGroup limitsGroupToCopy, boolean isSide1) {
        // if we have only one limit set with the same name but applicability is not good
        // we should copy existing limit set on the right side and removed it from the other side
        if (modificationInfos.stream().filter(limitSet -> limitSet.getId().equals(modifiedLimitSet)).toList().size() == 1) {
            // Copy limits
            OperationalLimitsGroup limitsGroup = isSide1 ? branch.newOperationalLimitsGroup1(limitsGroupToCopy.getId())
                : branch.newOperationalLimitsGroup2(limitsGroupToCopy.getId());
            copyOperationalLimitsOnSide(limitsGroup, limitsGroupToCopy);

            // delete other limit set
            if (isSide1) {
                branch.removeOperationalLimitsGroup2(modifiedLimitSet);
            } else {
                branch.removeOperationalLimitsGroup1(modifiedLimitSet);
            }
        }
    }

    // If we are changing applicability we may not find operational limits group where we should so check both sides
    private void detectApplicabilityChange(Branch<?> branch, List<OperationalLimitsGroupModificationInfos> modificationInfos,
                                           OperationalLimitsGroupModificationInfos modifiedLimitSet) {

        OperationalLimitsGroup limitsGroup1 = branch.getOperationalLimitsGroup1(modifiedLimitSet.getId()).orElse(null);
        OperationalLimitsGroup limitsGroup2 = branch.getOperationalLimitsGroup2(modifiedLimitSet.getId()).orElse(null);
        if (limitsGroup1 == null && limitsGroup2 == null || limitsGroup1 != null && limitsGroup2 != null || modifiedLimitSet.getApplicability().equals(SIDE1)
            && limitsGroup1 != null || modifiedLimitSet.getApplicability().equals(SIDE2) && limitsGroup2 != null) {
            return;
        }

        switch (modifiedLimitSet.getApplicability()) {
            case SIDE1 -> copyAndDeleteLimitSet(branch, modificationInfos, modifiedLimitSet.getId(), limitsGroup2, true);
            case SIDE2 -> copyAndDeleteLimitSet(branch, modificationInfos, modifiedLimitSet.getId(), limitsGroup1, false);
            case EQUIPMENT -> {
                if (limitsGroup1 == null) {
                    limitsGroup1 = branch.newOperationalLimitsGroup1(limitsGroup2.getId());
                    copyOperationalLimitsOnSide(limitsGroup1, limitsGroup2);
                }
                if (limitsGroup2 == null) {
                    limitsGroup2 = branch.newOperationalLimitsGroup2(limitsGroup1.getId());
                    copyOperationalLimitsOnSide(limitsGroup2, limitsGroup1);
                }
            }
        }
    }

    private void modifyOperationalLimitsGroups(Branch<?> branch, List<OperationalLimitsGroupModificationInfos> operationalLimitsInfos, List<ReportNode> olgReports) {
        for (OperationalLimitsGroupModificationInfos opLGModifInfos : operationalLimitsInfos) {
            if (opLGModifInfos.getModificationType() == null) {
                continue;
            }
            OperationalLimitsGroupInfos.Applicability applicability = opLGModifInfos.getApplicability();
            // here the modifications on an applicability EQUIPMENT are separated into two separate applications of both sides
            // because iidm has two separated sets of opLGs on the network object (and for better logs)

            detectApplicabilityChange(branch, operationalLimitsInfos, opLGModifInfos);

            if (applicability == SIDE1
                    || applicability == OperationalLimitsGroupInfos.Applicability.EQUIPMENT) {
                OperationalLimitsGroup operationalLimitsGroup1 = branch.getOperationalLimitsGroup1(opLGModifInfos.getId()).orElse(null);
                applyModificationToOperationalLimitsGroup(branch::newOperationalLimitsGroup1, opLGModifInfos, operationalLimitsGroup1, olgReports, SIDE1);
            }
            if (applicability == SIDE2
                    || applicability == OperationalLimitsGroupInfos.Applicability.EQUIPMENT) {
                OperationalLimitsGroup operationalLimitsGroup2 = branch.getOperationalLimitsGroup2(opLGModifInfos.getId()).orElse(null);
                applyModificationToOperationalLimitsGroup(branch::newOperationalLimitsGroup2, opLGModifInfos, operationalLimitsGroup2, olgReports, SIDE2);
            }
        }
    }

    private void applySelectedOLGs(Branch<?> branch, List<ReportNode> activeOLGReports) {
        if (modificationInfos.getSelectedOperationalLimitsGroup1() != null) {
            modifySelectedOperationalLimitsGroup(
                    branch,
                    modificationInfos.getSelectedOperationalLimitsGroup1(),
                    TwoSides.ONE,
                    activeOLGReports
            );
        }
        if (modificationInfos.getSelectedOperationalLimitsGroup2() != null) {
            modifySelectedOperationalLimitsGroup(
                    branch,
                    modificationInfos.getSelectedOperationalLimitsGroup2(),
                    TwoSides.TWO,
                    activeOLGReports);
        }
    }

    private static void applySelectedOLGOnSide1(Branch<?> branch, AttributeModification<String> modifOperationalLimitsGroup, List<ReportNode> reportNode, String newSelectedOLG) {
        if (!StringUtils.hasText(newSelectedOLG) || modifOperationalLimitsGroup.getOp() == OperationType.UNSET) {
            branch.cancelSelectedOperationalLimitsGroup1();
            if (reportNode != null) {
                reportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.noLimitSetSelectedOnSide1")
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        } else {
            if (StringUtils.hasText(newSelectedOLG) && branch.getOperationalLimitsGroup1(newSelectedOLG).isEmpty() && reportNode != null) {
                reportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetAbsentOnSide1")
                        .withUntypedValue("selectedOperationalLimitsGroup", newSelectedOLG)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());

            } else {
                branch.setSelectedOperationalLimitsGroup1(newSelectedOLG);
                if (reportNode != null) {
                    reportNode.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("network.modification.limitSetSelectedOnSide1")
                            .withUntypedValue("selectedOperationalLimitsGroup1", newSelectedOLG)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                }
            }
        }
    }

    private static void applySelectedOLGOnSide2(Branch<?> branch, AttributeModification<String> modifOperationalLimitsGroup, List<ReportNode> reportNode, String newSelectedOLG) {
        if (!StringUtils.hasText(newSelectedOLG) || modifOperationalLimitsGroup.getOp() == OperationType.UNSET) {
            branch.cancelSelectedOperationalLimitsGroup2();
            if (reportNode != null) {
                reportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.noLimitSetSelectedOnSide2")
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        } else {
            if (StringUtils.hasText(newSelectedOLG) && branch.getOperationalLimitsGroup2(newSelectedOLG).isEmpty() && reportNode != null) {
                reportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetAbsentOnSide2")
                        .withUntypedValue("selectedOperationalLimitsGroup", newSelectedOLG)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());

            } else {
                branch.setSelectedOperationalLimitsGroup2(newSelectedOLG);
                if (reportNode != null) {
                    reportNode.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("network.modification.limitSetSelectedOnSide2")
                            .withUntypedValue("selectedOperationalLimitsGroup2", newSelectedOLG)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                }
            }
        }
    }

    public ReportNode updateMeasurements(Branch<?> branch, BranchModificationInfos branchModificationInfos, ReportNode subReportNode) {
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
            return null;
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
            estimSubReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.stateEstimationData").add();
        }
        if (!side1Reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(estimSubReportNode, side1Reports, "network.modification.measurementsSide1");
        }
        if (!side2Reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(estimSubReportNode, side2Reports, "network.modification.measurementsSide2");
        }
        return estimSubReportNode;
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
                reports.add(ModificationUtils.buildModificationReport(oldValue, value, measurementType + VALUE, TypedValue.INFO_SEVERITY));
            }
            if (validity != null) {
                boolean oldValidity = m.isValid();
                m.setValid(validity);
                reports.add(ModificationUtils.buildModificationReport(oldValidity, validity, measurementType + VALIDITY, TypedValue.INFO_SEVERITY));
            }
        } else { // add new measurement
            var mAdder = measurements.newMeasurement().setId(UUID.randomUUID().toString()).setType(type).setSide(side);
            if (value != null) {
                mAdder.setValue(value);
                reports.add(ModificationUtils.buildModificationReport(null, value, measurementType + VALUE, TypedValue.INFO_SEVERITY));
            }
            if (validity != null) {
                mAdder.setValid(validity);
                reports.add(ModificationUtils.buildModificationReport(null, validity, measurementType + VALIDITY, TypedValue.INFO_SEVERITY));
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

    /**
     * only apply the modification to a singular operational limits group on a given side (like in iidm)
     * therefore applicability is SIDE1 or SIDE2, not EQUIPMENT
     */
    protected void applyModificationToOperationalLimitsGroup(
            Function<String, OperationalLimitsGroup> groupFactory,
            OperationalLimitsGroupModificationInfos opLGModificationInfos,
            OperationalLimitsGroup modifiedOperationalLimitsGroup,
            List<ReportNode> operationalLimitsGroupReports,
            OperationalLimitsGroupInfos.Applicability applicability
    ) {
        switch (opLGModificationInfos.getModificationType()) {
            case OperationalLimitsGroupModificationType.MODIFY_OR_ADD: {
                if (modifiedOperationalLimitsGroup == null) {
                    addOpLG(groupFactory, opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
                } else {
                    modifyOLG(opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
                }
            } break;
            case OperationalLimitsGroupModificationType.MODIFY: {
                if (modifiedOperationalLimitsGroup == null) {
                    throw new PowsyblException("Cannot modify operational limit group " + opLGModificationInfos.getId() + " which has not been found in equipment given side");
                }
                modifyOLG(opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
            } break;
            case OperationalLimitsGroupModificationType.ADD: {
                addOpLG(groupFactory, opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
            } break;
            case OperationalLimitsGroupModificationType.REPLACE: {
                replaceOpLG(groupFactory, opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
            } break;
        }
    }

    private void replaceOpLG(Function<String, OperationalLimitsGroup> groupFactory, OperationalLimitsGroupModificationInfos opLGModificationInfos, OperationalLimitsGroup modifiedOperationalLimitsGroup, List<ReportNode> operationalLimitsGroupReports, OperationalLimitsGroupInfos.Applicability applicability) {
        if (modifiedOperationalLimitsGroup != null) {
            modifiedOperationalLimitsGroup.removeCurrentLimits();
        }
        OperationalLimitsGroup newOperationalLimitsGroup = groupFactory.apply(opLGModificationInfos.getId());
        operationalLimitsGroupReports.add(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("network.modification.operationalLimitsGroupReplaced")
                .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, opLGModificationInfos.getId())
                .withUntypedValue(SIDE, applicability.toString())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        modifyCurrentLimits(opLGModificationInfos, newOperationalLimitsGroup.newCurrentLimits(), null, operationalLimitsGroupReports);
    }

    private void modifyOLG(
            OperationalLimitsGroupModificationInfos operationalLimitsGroupInfos,
            OperationalLimitsGroup modifiedOperationalLimitsGroup,
            List<ReportNode> olgsReports,
            OperationalLimitsGroupInfos.Applicability applicability) {
        modifiedOperationalLimitsGroup.getCurrentLimits().ifPresent(currentLimits -> {
            List<ReportNode> limitsReports = new ArrayList<>();
            modifyCurrentLimits(operationalLimitsGroupInfos, modifiedOperationalLimitsGroup.newCurrentLimits(), currentLimits, limitsReports);
            if (!limitsReports.isEmpty()) {
                // operational limits group is logged only if it contains at least a change of limit
                limitsReports.addFirst(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.operationalLimitsGroupModified")
                        .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, operationalLimitsGroupInfos.getId())
                        .withUntypedValue(SIDE, applicability.toString())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
                olgsReports.addAll(limitsReports);
            }
        });
    }

    private void addOpLG(Function<String, OperationalLimitsGroup> groupFactory, OperationalLimitsGroupModificationInfos operationalLimitsGroupInfos, OperationalLimitsGroup modifiedOperationalLimitsGroup, List<ReportNode> operationalLimitsGroupReports, OperationalLimitsGroupInfos.Applicability applicability) {
        if (modifiedOperationalLimitsGroup != null) {
            throw new PowsyblException("Cannot add " + modifiedOperationalLimitsGroup.getId() + " operational limit group, one with the given name already exists");
        }
        OperationalLimitsGroup newOperationalLimitsGroup = groupFactory.apply(operationalLimitsGroupInfos.getId());
        operationalLimitsGroupReports.add(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("network.modification.operationalLimitsGroupAdded")
                .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, operationalLimitsGroupInfos.getId())
                .withUntypedValue(SIDE, applicability.toString())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        modifyCurrentLimits(operationalLimitsGroupInfos, newOperationalLimitsGroup.newCurrentLimits(), newOperationalLimitsGroup.getCurrentLimits().orElse(null), operationalLimitsGroupReports);
    }

    protected void modifyCurrentLimits(
            OperationalLimitsGroupModificationInfos operationalLimitsGroupModificationInfos,
            CurrentLimitsAdder limitsAdder,
            CurrentLimits currentLimits,
            List<ReportNode> limitsReports) {
        CurrentLimitsModificationInfos currentLimitsInfos = operationalLimitsGroupModificationInfos.getCurrentLimits();
        boolean hasPermanent = currentLimitsInfos.getPermanentLimit() != null;
        if (hasPermanent) {
            limitsReports.add(ModificationUtils.getInstance().buildModificationReport(currentLimits != null ? currentLimits.getPermanentLimit() : Double.NaN,
                    currentLimitsInfos.getPermanentLimit(), "IST"));
            limitsAdder.setPermanentLimit(currentLimitsInfos.getPermanentLimit());
        } else {
            if (currentLimits != null) {
                limitsAdder.setPermanentLimit(currentLimits.getPermanentLimit());
            }
        }
        modifyTemporaryLimits(operationalLimitsGroupModificationInfos, limitsAdder, currentLimits, limitsReports);
        limitsAdder.add();
    }

    /**
     * is the limit identified by acceptableDuration deleted in temporaryLimitsModification ?
     */
    public boolean isThisLimitDeleted(List<CurrentTemporaryLimitModificationInfos> temporaryLimitsModification, int acceptableDuration) {
        return temporaryLimitsModification.stream()
                .filter(temporaryLimit -> temporaryLimit.getAcceptableDuration() != null)
                .anyMatch(temporaryLimit -> temporaryLimit.getAcceptableDuration() == acceptableDuration && temporaryLimit.getModificationType() == TemporaryLimitModificationType.DELETE);
    }

    /**
     * This function removes all the temporary limits of the 'currentLimits' concerned and recreates them (except in case of deletion)
     */
    protected void modifyTemporaryLimits(@NotNull OperationalLimitsGroupModificationInfos operationalLimitsGroupModificationInfos,
                                         CurrentLimitsAdder limitsAdder,
                                         CurrentLimits currentLimits,
                                         List<ReportNode> limitsReports) {
        CurrentLimitsModificationInfos currentLimitsInfos = operationalLimitsGroupModificationInfos.getCurrentLimits();

        // we create a mutable list of temporary limits to be able to remove the limits that are modified in this current modification
        // those left at the end of the network modification are those that have not been modified (or deleted)
        List<LoadingLimits.TemporaryLimit> unmodifiedTemporaryLimits = new ArrayList<>();
        boolean areLimitsReplaced = TemporaryLimitModificationType.REPLACE.equals(operationalLimitsGroupModificationInfos.getTemporaryLimitsModificationType());
        if (currentLimits != null) {
            unmodifiedTemporaryLimits.addAll(currentLimits.getTemporaryLimits());
        }
        List<ReportNode> temporaryLimitsReports = new ArrayList<>();

        if (currentLimitsInfos != null && currentLimitsInfos.getTemporaryLimits() != null) {
            for (CurrentTemporaryLimitModificationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
                applyTemporaryLimitModification(
                        operationalLimitsGroupModificationInfos,
                        limitsAdder,
                        currentLimits,
                        limit,
                        unmodifiedTemporaryLimits,
                        temporaryLimitsReports
                );
            }
        }

        if (!unmodifiedTemporaryLimits.isEmpty()) {
            if (areLimitsReplaced) {
                // this needs to be logged only if there are unmodifiedTemporaryLimits left.
                // which means that they are going to be removed by the REPLACE mode
                temporaryLimitsReports.addFirst(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.temporaryLimitsReplaced")
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            } else {
                // we add (back) the temporary limits that have not been modified
                for (LoadingLimits.TemporaryLimit limit : unmodifiedTemporaryLimits) {
                    addTemporaryLimit(limitsAdder, limit.getName(), limit.getValue(), limit.getAcceptableDuration());
                }
            }
        }
        if (!temporaryLimitsReports.isEmpty()) {
            temporaryLimitsReports.addFirst(ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.temporaryLimitsModification")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            limitsReports.addAll(temporaryLimitsReports);
        }
    }

    private static boolean mayCreateALimit(TemporaryLimitModificationType modificationType) {
        return modificationType == TemporaryLimitModificationType.ADD
                || modificationType == TemporaryLimitModificationType.REPLACE
                || modificationType == TemporaryLimitModificationType.MODIFY_OR_ADD;
    }

    /**
     * modify a specific limit
     * @param operationalLimitsGroupModificationInfos part of the network modification containing the operational limits groups data
     * @param limitsAdder adder which receives all the "validated" limits to be added at the end
     * @param networkCurrentLimits limits of the branch which is currently modified by the network modification
     * @param limit modification to be applied to the limit
     * @param unmodifiedTemporaryLimits list of all the unmodified limits that will be added at the end of the network modification
     * @param temporaryLimitsReports log report
     */
    private void applyTemporaryLimitModification(
            OperationalLimitsGroupModificationInfos operationalLimitsGroupModificationInfos,
            CurrentLimitsAdder limitsAdder,
            CurrentLimits networkCurrentLimits,
            CurrentTemporaryLimitModificationInfos limit,
            List<LoadingLimits.TemporaryLimit> unmodifiedTemporaryLimits,
            List<ReportNode> temporaryLimitsReports) {
        CurrentLimitsModificationInfos currentLimitsInfos = operationalLimitsGroupModificationInfos.getCurrentLimits();
        int limitAcceptableDuration = limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration();
        double limitValue = limit.getValue() == null ? Double.MAX_VALUE : limit.getValue();
        String limitDurationToReport = limitAcceptableDuration == Integer.MAX_VALUE ? " " : String.valueOf(limitAcceptableDuration);
        String limitValueToReport = limitValue == Double.MAX_VALUE ? "no value" : String.valueOf(limitValue);
        LoadingLimits.TemporaryLimit limitToModify = null;
        if (networkCurrentLimits != null) {
            limitToModify = getTemporaryLimitToModify(networkCurrentLimits, limit, currentLimitsInfos, operationalLimitsGroupModificationInfos.getTemporaryLimitsModificationType());
            // this limit is modified by the network modification so we remove it from the list of unmodified temporary limits
            unmodifiedTemporaryLimits.removeIf(temporaryLimit -> temporaryLimit.getAcceptableDuration() == limitAcceptableDuration);
        }
        if (limitToModify == null && mayCreateALimit(limit.getModificationType())) {
            createTemporaryLimit(limitsAdder, limit, temporaryLimitsReports, limitDurationToReport, limitValueToReport, limitValue, limitAcceptableDuration);
        } else if (limitToModify != null) {
            // the limit already exists
            if (limit.getModificationType() == TemporaryLimitModificationType.DELETE) {
                // the limit has been removed previously
                temporaryLimitsReports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.temporaryLimitDeleted.name")
                        .withUntypedValue(NAME, limit.getName())
                        .withUntypedValue(DURATION, limitDurationToReport)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            } else {
                modifyTemporaryLimit(limitsAdder, limit, temporaryLimitsReports, limitToModify, limitValue, limitDurationToReport, limitValueToReport, limitAcceptableDuration);
            }
        } else if (limit.getModificationType() == TemporaryLimitModificationType.MODIFY || limit.getModificationType() == TemporaryLimitModificationType.MODIFY_OR_ADD) {
            // invalid modification
            temporaryLimitsReports.add(ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.temporaryLimitsNoMatch")
                    .withUntypedValue(LIMIT_ACCEPTABLE_DURATION, limitAcceptableDuration)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        }
    }

    private static void modifyTemporaryLimit(
            CurrentLimitsAdder limitsAdder,
            CurrentTemporaryLimitModificationInfos limitModificationInfos,
            List<ReportNode> temporaryLimitsReports,
            LoadingLimits.TemporaryLimit limitToModify,
            double limitValue,
            String limitDurationToReport,
            String limitValueToReport,
            int limitAcceptableDuration) {
        if (Double.compare(limitToModify.getValue(), limitValue) != 0 && limitModificationInfos.getModificationType() != null) {
            temporaryLimitsReports.add(ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.temporaryLimitModified.name")
                    .withUntypedValue(NAME, limitModificationInfos.getName())
                    .withUntypedValue(DURATION, limitDurationToReport)
                    .withUntypedValue(VALUE, limitValueToReport)
                    .withUntypedValue("oldValue",
                            limitToModify.getValue() == Double.MAX_VALUE ? "no value"
                                    : String.valueOf(limitToModify.getValue()))
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            addTemporaryLimit(limitsAdder, limitModificationInfos.getName(), limitValue, limitAcceptableDuration);
        } else {
            // no real modification
            addTemporaryLimit(limitsAdder, limitModificationInfos.getName(), limitToModify.getValue(), limitAcceptableDuration);
        }
    }

    private static void createTemporaryLimit(
            CurrentLimitsAdder limitsAdder,
            CurrentTemporaryLimitModificationInfos limit,
            List<ReportNode> temporaryLimitsReports,
            String limitDurationToReport,
            String limitValueToReport,
            double limitValue,
            int limitAcceptableDuration) {
        temporaryLimitsReports.add(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("network.modification.temporaryLimitAdded.name")
                .withUntypedValue(NAME, limit.getName())
                .withUntypedValue(DURATION, limitDurationToReport)
                .withUntypedValue(VALUE, limitValueToReport)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        addTemporaryLimit(limitsAdder, limit.getName(), limitValue, limitAcceptableDuration);
    }

    private static void addTemporaryLimit(CurrentLimitsAdder limitsAdder, String limit, double limitValue, int limitAcceptableDuration) {
        limitsAdder
                .beginTemporaryLimit()
                .setName(limit)
                .setValue(limitValue)
                .setAcceptableDuration(limitAcceptableDuration)
                .endTemporaryLimit();
    }

    private LoadingLimits.TemporaryLimit getTemporaryLimitToModify(
            CurrentLimits networkCurrentLimits,
            CurrentTemporaryLimitModificationInfos limit,
            CurrentLimitsModificationInfos currentLimitsInfos,
            TemporaryLimitModificationType temporaryLimitsModificationType) {
        int limitAcceptableDuration = limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration();
        LoadingLimits.TemporaryLimit limitToModify;
        limitToModify = networkCurrentLimits.getTemporaryLimit(limitAcceptableDuration);
        if (limitToModify != null && !limitToModify.getName().equals(limit.getName())) {
            boolean isThisLimitDeleted = isThisLimitDeleted(currentLimitsInfos.getTemporaryLimits(), limitAcceptableDuration);
            if (isThisLimitDeleted) {
                limitToModify = null;
            } else if (TemporaryLimitModificationType.ADD.equals(limit.getModificationType())) {
                throw new PowsyblException("2 temporary limits have the same duration " + limitAcceptableDuration);
            }
        }

        //Additional check for limit sets tabular modifications
        if (TemporaryLimitModificationType.ADD.equals(temporaryLimitsModificationType)) {
            networkCurrentLimits.getTemporaryLimits().stream().filter(temporaryLimit -> temporaryLimit.getName().equals(limit.getName())).findFirst().ifPresent(temporaryLimit -> {
                throw new PowsyblException("2 temporary limits have the same name " + limit.getName());
            });
        }
        return limitToModify;
    }

    protected boolean characteristicsModified(BranchModificationInfos branchModificationInfos) {
        return branchModificationInfos.getX() != null
                && branchModificationInfos.getX().getValue() != null
                || branchModificationInfos.getR() != null
                && branchModificationInfos.getR().getValue() != null;
    }

    protected abstract void modifyCharacteristics(Branch<?> branch, BranchModificationInfos branchModificationInfos,
                                                  ReportNode subReportNode);

    private ReportNode modifyBranchConnectivityAttributes(BranchModificationInfos branchModificationInfos,
                                                          Branch<?> branch, ReportNode subReportNode) {
        ConnectablePosition<?> connectablePosition = (ConnectablePosition<?>) branch.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<?> connectablePositionAdder = branch.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyBranchConnectivityAttributes(connectablePosition, connectablePositionAdder, branch, branchModificationInfos, subReportNode);
    }

    private void modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide1(BranchModificationInfos modificationInfos,
                                                                           Branch<?> branch, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                (Connectable<?>) branch, branch.getTerminal1(),
                modificationInfos.getVoltageLevelId1(),
                modificationInfos.getBusOrBusbarSectionId1(),
                subReportNode
        );
    }

    private void modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide2(BranchModificationInfos modificationInfos,
                                                                           Branch<?> branch, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                (Connectable<?>) branch, branch.getTerminal2(),
                modificationInfos.getVoltageLevelId2(),
                modificationInfos.getBusOrBusbarSectionId2(),
                subReportNode
        );
    }

    public static void modifySelectedOperationalLimitsGroup(
            Branch<?> branch,
            AttributeModification<String> modifOperationalLimitsGroup,
            TwoSides side,
            List<ReportNode> reportNode) {
        Objects.requireNonNull(side);
        if (modifOperationalLimitsGroup != null) {
            String newSelectedOLG = modifOperationalLimitsGroup.getValue();
            if (side == TwoSides.ONE) {
                applySelectedOLGOnSide1(branch, modifOperationalLimitsGroup, reportNode, newSelectedOLG);
            } else if (side == TwoSides.TWO) {
                applySelectedOLGOnSide2(branch, modifOperationalLimitsGroup, reportNode, newSelectedOLG);
            }
        }
    }
}
