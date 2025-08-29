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

        List<ReportNode> side1LimitsReports = new ArrayList<>();
        List<ReportNode> side2LimitsReports = new ArrayList<>();

        if (branchModificationInfos.getOperationalLimitsGroups() != null) {
            modifyOperationalLimitsGroups(branch, branchModificationInfos.getOperationalLimitsGroups(), side1LimitsReports, side2LimitsReports);
        }

        applySelectedOpLGs(branch, side1LimitsReports, side2LimitsReports);

        if (!side1LimitsReports.isEmpty() || !side2LimitsReports.isEmpty()) {
            ReportNode limitsReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
            ModificationUtils.getInstance().reportModifications(limitsReportNode, side1LimitsReports, "network.modification.side1LimitsModification");
            ModificationUtils.getInstance().reportModifications(limitsReportNode, side2LimitsReports, "network.modification.side2LimitsModification");
        }
        updateConnections(branch, branchModificationInfos);
    }

    private void modifyOperationalLimitsGroups(Branch<?> branch, List<OperationalLimitsGroupModificationInfos> operationalLimitsInfos, List<ReportNode> side1LimitsReports, List<ReportNode> side2LimitsReports) {
        for (OperationalLimitsGroupModificationInfos opLGModifInfos : operationalLimitsInfos) {
            if (opLGModifInfos.getModificationType() == null) {
                continue;
            }
            OperationalLimitsGroupInfos.Applicability applicability = opLGModifInfos.getApplicability();
            // here the modification on an applicability EQUIPMENT are separated into two separate applications of both sides
            // because iidm has two separated sets of opLGs on the network object (and for better logs)
            if (applicability == SIDE1
                    || applicability == OperationalLimitsGroupInfos.Applicability.EQUIPMENT) {
                OperationalLimitsGroup operationalLimitsGroup1 = branch.getOperationalLimitsGroup1(opLGModifInfos.getId()).orElse(null);
                applyModificationToOperationalLimitsGroup(branch::newOperationalLimitsGroup1, opLGModifInfos, operationalLimitsGroup1, side1LimitsReports, branch, SIDE1);
            }
            if (applicability == SIDE2
                    || applicability == OperationalLimitsGroupInfos.Applicability.EQUIPMENT) {
                OperationalLimitsGroup operationalLimitsGroup2 = branch.getOperationalLimitsGroup2(opLGModifInfos.getId()).orElse(null);
                applyModificationToOperationalLimitsGroup(branch::newOperationalLimitsGroup2, opLGModifInfos, operationalLimitsGroup2, side2LimitsReports, branch, SIDE2);
            }
        }
    }

    private void applySelectedOpLGs(Branch<?> branch, List<ReportNode> side1LimitsReports, List<ReportNode> side2LimitsReports) {
        if (modificationInfos.getSelectedOperationalLimitsGroup1() != null) {
            applySelectedOpLGOnSide1(branch, side1LimitsReports);
        }
        if (modificationInfos.getSelectedOperationalLimitsGroup2() != null) {
            applySelectedOpLGOnSide2(branch, side2LimitsReports);
        }
    }

    private void applySelectedOpLGOnSide1(Branch<?> branch, List<ReportNode> reportNode) {
        switch (modificationInfos.getSelectedOperationalLimitsGroup1().getOp()) {
            case UNSET : {
                branch.cancelSelectedOperationalLimitsGroup1();
                reportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.noLimitSetSelectedOnSide1")
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            } break;
            case SET: {
                String newSelectedOpLG1 = modificationInfos.getSelectedOperationalLimitsGroup1().getValue();
                if (StringUtils.hasText(newSelectedOpLG1) && branch.getOperationalLimitsGroup1(newSelectedOpLG1).isEmpty()) {
                    reportNode.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("network.modification.limitSetAbsentOnSide1")
                            .withUntypedValue("selectedOperationalLimitsGroup", newSelectedOpLG1)
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .build());

                } else {
                    branch.setSelectedOperationalLimitsGroup1(newSelectedOpLG1);
                    reportNode.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("network.modification.limitSetSelectedOnSide1")
                            .withUntypedValue("selectedOperationalLimitsGroup1", newSelectedOpLG1)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                }
            } break;
        }
    }

    private void applySelectedOpLGOnSide2(Branch<?> branch, List<ReportNode> reportNode) {
        switch (modificationInfos.getSelectedOperationalLimitsGroup2().getOp()) {
            case UNSET : {
                branch.setSelectedOperationalLimitsGroup2("");
                reportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.noLimitSetSelectedOnSide2")
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            } break;
            case SET: {
                String newSelectedOpLG = modificationInfos.getSelectedOperationalLimitsGroup2().getValue();
                if (StringUtils.hasText(newSelectedOpLG) && branch.getOperationalLimitsGroup2(newSelectedOpLG).isEmpty()) {
                    reportNode.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("network.modification.limitSetAbsentOnSide2")
                            .withUntypedValue("selectedOperationalLimitsGroup2", newSelectedOpLG)
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .build());

                } else {
                    branch.setSelectedOperationalLimitsGroup2(newSelectedOpLG);
                    reportNode.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("network.modification.limitSetSelectedOnSide2")
                            .withUntypedValue("selectedOperationalLimitsGroup2", newSelectedOpLG)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                }
            } break;
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
            Branch<?> branch,
            OperationalLimitsGroupInfos.Applicability applicability
    ) {
        switch (opLGModificationInfos.getModificationType()) {
            case OperationalLimitsGroupModificationType.MODIFY_OR_ADD: {
                if (modifiedOperationalLimitsGroup == null) {
                    addOpLG(groupFactory, opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
                } else {
                    modifyOpLG(opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
                }
            } break;
            case OperationalLimitsGroupModificationType.MODIFY: {
                if (modifiedOperationalLimitsGroup == null) {
                    throw new PowsyblException("Cannot modify operational limit group " + opLGModificationInfos.getId() + " which has not been found in equipment given side");
                }
                modifyOpLG(opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
            } break;
            case OperationalLimitsGroupModificationType.ADD: {
                addOpLG(groupFactory, opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
            } break;
            case OperationalLimitsGroupModificationType.REPLACE: {
                replaceOpLG(groupFactory, opLGModificationInfos, modifiedOperationalLimitsGroup, operationalLimitsGroupReports, applicability);
            } break;
            case OperationalLimitsGroupModificationType.DELETE: {
                if (applicability == SIDE1 && branch.getOperationalLimitsGroup1(opLGModificationInfos.getId()).isEmpty() ||
                        applicability == SIDE2 && branch.getOperationalLimitsGroup2(opLGModificationInfos.getId()).isEmpty()) {
                    throw new PowsyblException("Cannot delete operational limit group " + opLGModificationInfos.getId() + " which has not been found in equipment on side " + applicability);
                }
                if (applicability == SIDE1) {
                    branch.removeOperationalLimitsGroup1(opLGModificationInfos.getId());
                } else if (applicability == SIDE2) {
                    branch.removeOperationalLimitsGroup2(opLGModificationInfos.getId());
                }
                operationalLimitsGroupReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.operationalLimitsGroupDeleted")
                        .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, opLGModificationInfos.getId())
                        .withUntypedValue(SIDE, applicability.toString())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
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

    private void modifyOpLG(OperationalLimitsGroupModificationInfos operationalLimitsGroupInfos, OperationalLimitsGroup modifiedOperationalLimitsGroup, List<ReportNode> operationalLimitsGroupReports, OperationalLimitsGroupInfos.Applicability applicability) {
        modifiedOperationalLimitsGroup.getCurrentLimits().ifPresent(currentLimits -> {
            operationalLimitsGroupReports.add(ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.operationalLimitsGroupModified")
                    .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, operationalLimitsGroupInfos.getId())
                    .withUntypedValue(SIDE, applicability.toString())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            modifyCurrentLimits(operationalLimitsGroupInfos, modifiedOperationalLimitsGroup.newCurrentLimits(), currentLimits, operationalLimitsGroupReports);
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

    protected void modifyTemporaryLimits(OperationalLimitsGroupModificationInfos operationalLimitsGroupModificationInfos,
                                         CurrentLimitsAdder limitsAdder,
                                         CurrentLimits currentLimits,
                                         List<ReportNode> limitsReports) {
        CurrentLimitsModificationInfos currentLimitsInfos = operationalLimitsGroupModificationInfos.getCurrentLimits();

        // we create a mutable list of temporary limits to be able to remove the limits that are modified in current modification
        List<LoadingLimits.TemporaryLimit> branchTemporaryLimits = new ArrayList<>();
        boolean areLimitsReplaced = operationalLimitsGroupModificationInfos != null && TemporaryLimitModificationType.REPLACE.equals(operationalLimitsGroupModificationInfos.getTemporaryLimitsModificationType());
        if (currentLimits != null && !areLimitsReplaced) {
            branchTemporaryLimits.addAll(currentLimits.getTemporaryLimits());
        }
        List<ReportNode> temporaryLimitsReports = new ArrayList<>();
        if (operationalLimitsGroupModificationInfos != null && TemporaryLimitModificationType.REPLACE.equals(operationalLimitsGroupModificationInfos.getTemporaryLimitsModificationType())) {
            temporaryLimitsReports.add(ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.temporaryLimitsReplaced")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }

        if (currentLimitsInfos != null && currentLimitsInfos.getTemporaryLimits() != null) {
            for (CurrentTemporaryLimitModificationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
                int limitAcceptableDuration = limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration();
                double limitValue = limit.getValue() == null ? Double.MAX_VALUE : limit.getValue();
                String limitDurationToReport = limitAcceptableDuration == Integer.MAX_VALUE ? " " : String.valueOf(limitAcceptableDuration);
                String limitValueToReport = limitValue == Double.MAX_VALUE ? "no value" : String.valueOf(limitValue);
                LoadingLimits.TemporaryLimit limitToModify = null;
                if (currentLimits != null) {
                    limitToModify = currentLimits.getTemporaryLimit(limitAcceptableDuration);
                    if (limitToModify != null && !limitToModify.getName().equals(limit.getName())) {
                        boolean isThisLimitDeleted = currentLimitsInfos.isThisLimitDeleted(limitAcceptableDuration);
                        if (isThisLimitDeleted) {
                            limitToModify = null;
                        } else {
                            throw new PowsyblException("2 temporary limits have the same duration " + limitAcceptableDuration);
                        }
                    }

                    //Additional check for limit sets tabular modifications
                    if (operationalLimitsGroupModificationInfos != null && TemporaryLimitModificationType.ADD.equals(operationalLimitsGroupModificationInfos.getTemporaryLimitsModificationType())) {
                        currentLimits.getTemporaryLimits().stream().filter(temporaryLimit -> temporaryLimit.getName().equals(limit.getName())).findFirst().ifPresent(temporaryLimit -> {
                            throw new PowsyblException("2 temporary limits have the same name " + limit.getName());
                        });
                    }
                    // we remove the limit to modify from the list of temporary limits so we can get the list of temporary limits coming from previous modifications
                    branchTemporaryLimits.removeIf(temporaryLimit -> temporaryLimit.getAcceptableDuration() == limitAcceptableDuration);
                }
                if (limitToModify == null && limit.getModificationType() == TemporaryLimitModificationType.ADD || limit.getModificationType() == TemporaryLimitModificationType.REPLACE) {
                    temporaryLimitsReports.add(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.temporaryLimitAdded.name")
                            .withUntypedValue(NAME, limit.getName())
                            .withUntypedValue(DURATION, limitDurationToReport)
                            .withUntypedValue(VALUE, limitValueToReport)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());

                } else if (limitToModify != null) {
                    if (limit.getModificationType() == TemporaryLimitModificationType.DELETE) {
                        temporaryLimitsReports.add(ReportNode.newRootReportNode()
                                .withAllResourceBundlesFromClasspath()
                                .withMessageTemplate("network.modification.temporaryLimitDeleted.name")
                                .withUntypedValue(NAME, limit.getName())
                                .withUntypedValue(DURATION, limitDurationToReport)
                                .withSeverity(TypedValue.INFO_SEVERITY)
                                .build());
                        continue;
                    } else if (Double.compare(limitToModify.getValue(), limitValue) != 0 && limit.getModificationType() != null) {
                        temporaryLimitsReports.add(ReportNode.newRootReportNode()
                                .withAllResourceBundlesFromClasspath()
                                .withMessageTemplate("network.modification.temporaryLimitModified.name")
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
                } else if (limit.getModificationType() == TemporaryLimitModificationType.MODIFY) {
                    temporaryLimitsReports.add(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.temporaryLimitsNoMatch")
                            .withUntypedValue(LIMIT_ACCEPTABLE_DURATION, limitAcceptableDuration)
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .build());
                    continue;
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
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.temporaryLimitsModification")
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

    private ReportNode modifyBranchConnectivityAttributes(BranchModificationInfos branchModificationInfos,
                                                          Branch<?> branch, ReportNode subReportNode) {
        ConnectablePosition<?> connectablePosition = (ConnectablePosition<?>) branch.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<?> connectablePositionAdder = branch.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyBranchConnectivityAttributes(connectablePosition, connectablePositionAdder, branch, branchModificationInfos, subReportNode);
    }

    private void modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide1(BranchModificationInfos modificationInfos,
                                                                           Branch<?> branch, ReportNode subReportNode) {
        ModificationUtils.getInstance().modifyVoltageLevelBusOrBusBarSectionAttributes(
                (Connectable<?>) branch, branch.getTerminal1(),
                modificationInfos.getVoltageLevelId1(),
                modificationInfos.getBusOrBusbarSectionId1(),
                subReportNode
        );
    }

    private void modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide2(BranchModificationInfos modificationInfos,
                                                                           Branch<?> branch, ReportNode subReportNode) {
        ModificationUtils.getInstance().modifyVoltageLevelBusOrBusBarSectionAttributes(
                (Connectable<?>) branch, branch.getTerminal2(),
                modificationInfos.getVoltageLevelId2(),
                modificationInfos.getBusOrBusbarSectionId2(),
                subReportNode
        );
    }

    public static void modifySelectedOperationalLimitsGroup(Branch<?> branch, AttributeModification<String> modifOperationalLimitsGroup,
                                                            TwoSides side, ReportNode reportNode) {
        Objects.requireNonNull(side);
        if (modifOperationalLimitsGroup != null) {
            String value = modifOperationalLimitsGroup.getValue();
            String previousSelectedLimitsGroup = null;
            if (side == TwoSides.ONE) {
                previousSelectedLimitsGroup = branch.getSelectedOperationalLimitsGroupId1().orElse(null);
                if (!StringUtils.hasText(value)) {
                    branch.cancelSelectedOperationalLimitsGroup1();
                } else {
                    branch.setSelectedOperationalLimitsGroup1(value);
                }
            } else if (side == TwoSides.TWO) {
                previousSelectedLimitsGroup = branch.getSelectedOperationalLimitsGroupId2().orElse(null);
                if (!StringUtils.hasText(value)) {
                    branch.cancelSelectedOperationalLimitsGroup2();
                } else {
                    branch.setSelectedOperationalLimitsGroup2(value);
                }
            }
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(previousSelectedLimitsGroup,
                        modifOperationalLimitsGroup.getValue(), "selected operational limits group side " + side.getNum()));
            }
        }
    }
}
