/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.olg;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.model.constants.OperationType;
import org.gridsuite.modification.model.constants.OperationalLimitsGroupModificationType;
import org.gridsuite.modification.model.constants.TemporaryLimitModificationType;
import org.gridsuite.modification.modifications.AbstractBranchModification;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability.*;
import static org.gridsuite.modification.modifications.AbstractBranchModification.*;

/**
 * handles the modification of a single operational limits group from AbstractBranchModification
 * it may affect both sides of an operational limits group
 *
 * @author Mathieu DEHARBE <mathieu.deharbe at rte-france.com>
 */
public class OperationalLimitsGroupModification {
    private final Branch<?> modifiedBranch; // branch modified by the network modification
    private final OperationalLimitsGroupModificationModel olgModifModel;
    private final ReportNode olgsReportNode;
    List<ReportNode> limitsReportsSide1;
    List<ReportNode> limitsReportsSide2;

    private static final String NO_VALUE = "no value";

    public OperationalLimitsGroupModification(
            Branch<?> modifiedBranch,
            OperationalLimitsGroupModificationModel olgModifModel,
            ReportNode limitSetsReportNode) {
        this.modifiedBranch = modifiedBranch;
        this.olgModifModel = olgModifModel;
        olgsReportNode = limitSetsReportNode;
        limitsReportsSide1 = new ArrayList<>();
        limitsReportsSide2 = new ArrayList<>();
    }

    private void addToLogsOnSide(ReportNode reportNode, OperationalLimitsGroupModel.Applicability applicability) {
        if (applicability == EQUIPMENT || applicability == SIDE1) {
            limitsReportsSide1.add(reportNode);
        }
        if (applicability == EQUIPMENT || applicability == SIDE2) {
            limitsReportsSide2.add(reportNode);
        }
    }

    private OperationalLimitsGroup modifiedOperationalLimitsGroup1() {
        return modifiedBranch.getOperationalLimitsGroup1(olgModifModel.getId()).orElse(null);
    }

    private OperationalLimitsGroup modifiedOperationalLimitsGroup2() {
        return modifiedBranch.getOperationalLimitsGroup2(olgModifModel.getId()).orElse(null);
    }

    protected void applyModificationToOperationalLimitsGroup() {
        switch (olgModifModel.getModificationType()) {
            case OperationalLimitsGroupModificationType.MODIFY_OR_ADD:
                switch (olgModifModel.getApplicability()) {
                    case EQUIPMENT :
                        if (modifiedOperationalLimitsGroup1() == null && modifiedOperationalLimitsGroup2() == null) {
                            addOlg(EQUIPMENT);
                        } else if (modifiedOperationalLimitsGroup1() != null && modifiedOperationalLimitsGroup2() != null) {
                            modifyOLG(EQUIPMENT);
                        } else {
                            // one side already existed (modification), the other was empty so this is a creation
                            if (modifiedOperationalLimitsGroup1() == null) {
                                addOlg(SIDE1);
                            } else {
                                modifyOLG(SIDE1);
                            }
                            if (modifiedOperationalLimitsGroup2() == null) {
                                addOlg(SIDE2);
                            } else {
                                modifyOLG(SIDE2);
                            }
                        }
                        break;
                    case SIDE1 :
                        if (modifiedOperationalLimitsGroup1() == null) {
                            addOlg(olgModifModel.getApplicability());
                        } else {
                            modifyOLG(olgModifModel.getApplicability());
                        }
                        break;
                    case SIDE2 :
                        if (modifiedOperationalLimitsGroup2() == null) {
                            addOlg(olgModifModel.getApplicability());
                        } else {
                            modifyOLG(olgModifModel.getApplicability());
                        }
                        break;
                }
                break;
            case OperationalLimitsGroupModificationType.MODIFY:
                if (applicableOnSide1() && modifiedOperationalLimitsGroup1() == null) {
                    throw new PowsyblException("Cannot modify operational limit group " + olgModifModel.getId() + " which has not been found in equipment side 1");
                }
                if (applicableOnSide2() && modifiedOperationalLimitsGroup2() == null) {
                    throw new PowsyblException("Cannot modify operational limit group " + olgModifModel.getId() + " which has not been found in equipment side 2");
                }
                modifyOLG(olgModifModel.getApplicability());
                break;
            case OperationalLimitsGroupModificationType.ADD:
                addOlg(olgModifModel.getApplicability());
                break;
            case OperationalLimitsGroupModificationType.REPLACE:
                replaceOlg();
                break;
            case DELETE:
                removeOlg();
        }
    }

    private boolean applicableOnSide1() {
        return olgModifModel.getApplicability() == SIDE1 || olgModifModel.getApplicability() == EQUIPMENT;
    }

    private boolean applicableOnSide2() {
        return olgModifModel.getApplicability() == SIDE2 || olgModifModel.getApplicability() == EQUIPMENT;
    }

    private void modifyOLG(OperationalLimitsGroupModel.Applicability applicability) {
        if (applicability == SIDE1 || applicability == EQUIPMENT) {
            OperationalLimitsGroup modifiedOlg = modifiedOperationalLimitsGroup1();
            if (modifiedOlg != null) {
                modifiedOlg.getCurrentLimits().ifPresent(currentLimits -> {
                    modifyCurrentLimits(modifiedOlg.newCurrentLimits(), currentLimits, SIDE1);
                    modifyProperties(modifiedOlg, SIDE1);
                });
            }
        }
        if (applicability == SIDE2 || applicability == EQUIPMENT) {
            OperationalLimitsGroup modifiedOlg = modifiedOperationalLimitsGroup2();
            if (modifiedOlg != null) {
                modifiedOlg.getCurrentLimits().ifPresent(currentLimits -> {
                    modifyCurrentLimits(modifiedOlg.newCurrentLimits(), currentLimits, SIDE2);
                    modifyProperties(modifiedOlg, SIDE2);
                });
            }
        }

        if (!limitsReportsSide1.isEmpty() || !limitsReportsSide2.isEmpty()) {
            ReportNode limitSetReport = olgsReportNode.newReportNode()
                    .withMessageTemplate("network.modification.operationalLimitsGroupModified")
                    .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, olgModifModel.getId())
                    .withUntypedValue(SIDE, ModificationUtils.applicabilityToString(applicability))
                    .withSeverity(TypedValue.INFO_SEVERITY).add();
            createLogNodeForSide(limitSetReport, "network.modification.operationalLimitsGroupModified.detail", SIDE1);
            createLogNodeForSide(limitSetReport, "network.modification.operationalLimitsGroupModified.detail", SIDE2);
        }
    }

    private void createLogNodeForSide(ReportNode limitSetReport, String messageTemplate, OperationalLimitsGroupModel.Applicability applicability) {
        if (applicability == EQUIPMENT) {
            throw new IllegalArgumentException("createLogNodeForSide cannot be called on an EQUIPMENT applicability");
        }
        List<ReportNode> limitsReports = applicability == SIDE1 ? limitsReportsSide1 : limitsReportsSide2;
        if (!limitsReports.isEmpty()) {
            ReportNode limitSetReportDetail = limitSetReport.newReportNode()
                    .withMessageTemplate(messageTemplate)
                    .withUntypedValue(SIDE, ModificationUtils.applicabilityToString(applicability))
                    .withSeverity(TypedValue.DETAIL_SEVERITY).add();
            ModificationUtils.getInstance().reportModifications(limitSetReportDetail, limitsReports);
        }
    }

    private void modifyProperties(OperationalLimitsGroup limitsGroup, OperationalLimitsGroupModel.Applicability applicability) {
        if (limitsGroup == null || olgModifModel == null) {
            return;
        }

        Set<String> currentProperties = limitsGroup.getPropertyNames();

        List<LimitsPropertyModel> propertiesToModify = new ArrayList<>();
        List<LimitsPropertyModel> propertiesToAdd = new ArrayList<>();
        List<String> propertiesToRemove;

        if (!CollectionUtils.isEmpty(olgModifModel.getLimitsProperties())) {
            for (LimitsPropertyModel propertyModel : olgModifModel.getLimitsProperties()) {
                if (currentProperties.contains(propertyModel.name())) {
                    propertiesToModify.add(propertyModel);
                } else {
                    propertiesToAdd.add(propertyModel);
                }
            }

            propertiesToRemove = currentProperties.stream().filter(
                    (String propertyName) -> propertiesToModify.stream().filter(propertyModel ->
                            propertyModel.name().equals(propertyName)).toList().isEmpty()).toList();
        } else {
            propertiesToRemove = new ArrayList<>(currentProperties);
        }

        propertiesToRemove.forEach((String propertyName) -> {
            limitsGroup.removeProperty(propertyName);
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyDeleted")
                    .withUntypedValue(NAME, propertyName)
                    .withSeverity(TypedValue.DETAIL_SEVERITY).build(),
                    applicability);
        });

        propertiesToModify.forEach((LimitsPropertyModel property) -> {
            // Skip changes when value does not change
            if (limitsGroup.getProperty(property.name()).equals(property.value())) {
                return;
            }
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyChanged")
                    .withUntypedValue(NAME, property.name())
                    .withUntypedValue("to", property.value())
                    .withUntypedValue("from", limitsGroup.getProperty(property.name()))
                    .withSeverity(TypedValue.DETAIL_SEVERITY).build(),
                    applicability);
            limitsGroup.setProperty(property.name(), property.value());
        });

        propertiesToAdd.forEach((LimitsPropertyModel property) -> {
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyAdded")
                    .withUntypedValue(NAME, property.name())
                    .withUntypedValue(VALUE, property.value())
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build(),
                    applicability);
            limitsGroup.setProperty(property.name(), property.value());
        });
    }

    protected void modifyCurrentLimits(
            CurrentLimitsAdder limitsAdder,
            CurrentLimits currentLimits,
            OperationalLimitsGroupModel.Applicability applicability) {
        CurrentLimitsModificationModel currentLimitsModel = olgModifModel.getCurrentLimits();
        boolean hasPermanent = currentLimitsModel.getPermanentLimit() != null;
        if (hasPermanent) {
            if (!(currentLimits != null && currentLimits.getPermanentLimit() == currentLimitsModel.getPermanentLimit())) {
                addToLogsOnSide(ModificationUtils.buildModificationReport(
                        currentLimits != null ? currentLimits.getPermanentLimit() : Double.NaN, currentLimitsModel.getPermanentLimit(),
                                "Permanent limit",
                                TypedValue.DETAIL_SEVERITY
                        ),
                        applicability);
            }
            limitsAdder.setPermanentLimit(currentLimitsModel.getPermanentLimit());
        } else {
            if (currentLimits != null) {
                limitsAdder.setPermanentLimit(currentLimits.getPermanentLimit());
            }
        }
        modifyTemporaryLimits(limitsAdder, currentLimits, applicability);
        limitsAdder.add();
    }

    private void addOlg(OperationalLimitsGroupModel.Applicability applicability) {
        if (applicability == EQUIPMENT || applicability == SIDE1) {
            if (modifiedOperationalLimitsGroup1() != null) {
                throw new PowsyblException("Cannot add " + modifiedOperationalLimitsGroup1().getId() + " operational limit group, one with the given name already exists");
            }
            addOlgOnSide(modifiedBranch.newOperationalLimitsGroup1(olgModifModel.getId()), SIDE1);
        }
        if (applicability == EQUIPMENT || applicability == SIDE2) {
            if (modifiedOperationalLimitsGroup2() != null) {
                throw new PowsyblException("Cannot add " + modifiedOperationalLimitsGroup2().getId() + " operational limit group, one with the given name already exists");
            }
            addOlgOnSide(modifiedBranch.newOperationalLimitsGroup2(olgModifModel.getId()), SIDE2);
        }

        if (!limitsReportsSide1.isEmpty() || !limitsReportsSide2.isEmpty()) {
            ReportNode limitSetReport = olgsReportNode.newReportNode()
                    .withMessageTemplate("network.modification.operationalLimitsGroupAdded")
                    .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, olgModifModel.getId())
                    .withUntypedValue(SIDE, ModificationUtils.applicabilityToString(applicability))
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            createLogNodeForSide(limitSetReport, ModificationUtils.OPERATIONAL_LIMITS_GROUP_ADDED_LOG_DETAIL, SIDE1);
            createLogNodeForSide(limitSetReport, ModificationUtils.OPERATIONAL_LIMITS_GROUP_ADDED_LOG_DETAIL, SIDE2);
        }
    }

    private void addOlgOnSide(OperationalLimitsGroup newOperationalLimitsGroup, OperationalLimitsGroupModel.Applicability applicability) {
        modifyCurrentLimits(newOperationalLimitsGroup.newCurrentLimits(), newOperationalLimitsGroup.getCurrentLimits().orElse(null), applicability);
        addProperties(newOperationalLimitsGroup, applicability);
    }

    private void replaceOlg() {
        if (applicableOnSide1()) {
            OperationalLimitsGroup modifiedOlg = modifiedOperationalLimitsGroup1();
            if (modifiedOlg != null) {
                modifiedOlg.removeCurrentLimits();
                removeAllProperties(modifiedOlg);
            }
            OperationalLimitsGroup newOperationalLimitsGroup = modifiedBranch.newOperationalLimitsGroup1(olgModifModel.getId());
            modifyCurrentLimits(newOperationalLimitsGroup.newCurrentLimits(), null, SIDE1);
            addProperties(newOperationalLimitsGroup, SIDE1);
        }
        if (applicableOnSide2()) {
            OperationalLimitsGroup modifiedOlg = modifiedOperationalLimitsGroup2();
            if (modifiedOlg != null) {
                modifiedOlg.removeCurrentLimits();
                removeAllProperties(modifiedOlg);
            }
            OperationalLimitsGroup newOperationalLimitsGroup = modifiedBranch.newOperationalLimitsGroup2(olgModifModel.getId());
            modifyCurrentLimits(newOperationalLimitsGroup.newCurrentLimits(), null, SIDE2);
            addProperties(newOperationalLimitsGroup, SIDE2);
        }

        if (!limitsReportsSide1.isEmpty() || !limitsReportsSide2.isEmpty()) {
            ReportNode limitSetReport = olgsReportNode.newReportNode()
                    .withMessageTemplate("network.modification.operationalLimitsGroupReplaced")
                    .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, olgModifModel.getId())
                    .withUntypedValue(SIDE, ModificationUtils.applicabilityToString(olgModifModel.getApplicability()))
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            createLogNodeForSide(limitSetReport, ModificationUtils.OPERATIONAL_LIMITS_GROUP_ADDED_LOG_DETAIL, SIDE1);
            createLogNodeForSide(limitSetReport, ModificationUtils.OPERATIONAL_LIMITS_GROUP_ADDED_LOG_DETAIL, SIDE2);
        }
    }

    private void addProperties(OperationalLimitsGroup limitsGroup, OperationalLimitsGroupModel.Applicability applicability) {
        if (limitsGroup == null || CollectionUtils.isEmpty(olgModifModel.getLimitsProperties())) {
            return;
        }

        olgModifModel.getLimitsProperties().forEach((LimitsPropertyModel property) -> {
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyAdded")
                    .withUntypedValue(NAME, property.name())
                    .withUntypedValue(VALUE, property.value())
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build(),
                    applicability);
            limitsGroup.setProperty(property.name(), property.value());
        });
    }

    public void removeOlg() {
        String olgId = olgModifModel.getId();
        if (applicableOnSide1() && modifiedBranch.getOperationalLimitsGroup1(olgId).isEmpty() ||
            applicableOnSide2() && modifiedBranch.getOperationalLimitsGroup2(olgId).isEmpty()) {
            throw new PowsyblException(
                    "Cannot delete operational limit group " + olgId + " which has not been found in equipment on " + ModificationUtils.applicabilityToString(olgModifModel.getApplicability()));
        }
        if (applicableOnSide1()) {
            modifiedBranch.removeOperationalLimitsGroup1(olgId);
        }
        if (applicableOnSide2()) {
            modifiedBranch.removeOperationalLimitsGroup2(olgId);
        }
        olgsReportNode.newReportNode()
                .withMessageTemplate("network.modification.operationalLimitsGroupDeleted")
                .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, olgId)
                .withUntypedValue(SIDE, ModificationUtils.applicabilityToString(olgModifModel.getApplicability()))
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void removeAllProperties(OperationalLimitsGroup limitsGroup) {

        if (limitsGroup == null) {
            return;
        }

        Iterator<String> propertiesIt = limitsGroup.getPropertyNames().iterator();
        while (propertiesIt.hasNext()) {
            String propertyName = propertiesIt.next();
            limitsGroup.removeProperty(propertyName);
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyDeleted")
                    .withUntypedValue(NAME, propertyName)
                    .withSeverity(TypedValue.DETAIL_SEVERITY).build(),
                    olgModifModel.getApplicability());
        }
    }

    /**
     * This function removes all the temporary limits of the 'currentLimits' concerned and recreates them (except in case of deletion)
     */
    protected void modifyTemporaryLimits(CurrentLimitsAdder limitsAdder, CurrentLimits currentLimits, OperationalLimitsGroupModel.Applicability applicability) {
        CurrentLimitsModificationModel currentLimitsModel = olgModifModel.getCurrentLimits();

        // we create a mutable list of temporary limits to be able to remove the limits that are modified in this current modification
        // those left at the end of the network modification are those that have not been modified (or deleted)
        List<LoadingLimits.TemporaryLimit> unmodifiedTemporaryLimits = new ArrayList<>();
        boolean areLimitsReplaced = TemporaryLimitModificationType.REPLACE.equals(olgModifModel.getTemporaryLimitsModificationType());
        if (currentLimits != null) {
            unmodifiedTemporaryLimits.addAll(currentLimits.getTemporaryLimits());
        }

        // Working set tracking the projected limits after the inputs accepted so far (duration -> name).
        // Seeded from the network snapshot so that subsequent inputs in the same batch see the effects
        // of earlier accepted inputs (additions, renames, deletions).
        Map<Integer, String> workingTemporaryLimits = new HashMap<>();
        if (currentLimits != null) {
            for (LoadingLimits.TemporaryLimit tl : currentLimits.getTemporaryLimits()) {
                workingTemporaryLimits.put(tl.getAcceptableDuration(), tl.getName());
            }
        }

        // tracks whether at least one input actually applied a change (created / modified /
        // deleted a temporary limit); limits ignored by validation (missing fields, duplicates,
        // no match...) do not count
        boolean atLeastOneLimitApplied = false;

        // APPLY MODIFICATIONS
        if (currentLimitsModel != null && currentLimitsModel.getTemporaryLimits() != null) {
            for (CurrentTemporaryLimitModificationModel limit : currentLimitsModel.getTemporaryLimits()) {
                if (limit == null) {
                    continue;
                }
                atLeastOneLimitApplied |= applyTemporaryLimitModification(
                        limitsAdder,
                        currentLimits,
                        limit,
                        unmodifiedTemporaryLimits,
                        workingTemporaryLimits,
                        applicability
                );
            }
        }

        // ADD BACK LIMITS (if necessary)
        handleUnmodifiedTemporaryLimits(limitsAdder, unmodifiedTemporaryLimits, areLimitsReplaced, atLeastOneLimitApplied, applicability);
    }

    /**
     * In REPLACE mode with at least one applied input, the untouched limits are intentionally dropped and a log is emitted.
     * Otherwise the untouched limits are re-added so they survive the modification.
     */
    private void handleUnmodifiedTemporaryLimits(
            CurrentLimitsAdder limitsAdder,
            List<LoadingLimits.TemporaryLimit> unmodifiedTemporaryLimits,
            boolean areLimitsReplaced,
            boolean atLeastOneLimitApplied,
            OperationalLimitsGroupModel.Applicability applicability) {
        if (unmodifiedTemporaryLimits.isEmpty()) {
            return;
        }
        if (areLimitsReplaced && atLeastOneLimitApplied) {
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.temporaryLimitsReplaced")
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build(),
                    applicability);
        } else {
            for (LoadingLimits.TemporaryLimit limit : unmodifiedTemporaryLimits) {
                addTemporaryLimit(limitsAdder, limit.getName(), limit.getValue(), limit.getAcceptableDuration());
            }
        }
    }

    /**
     * Ensure that the mandatory fields are correct before applying the temporary limit modification.
     * @return true if the modification can proceed, false if it must be skipped.
     */
    private boolean preModificationCheck(CurrentTemporaryLimitModificationModel limit, OperationalLimitsGroupModel.Applicability applicability) {
        boolean validModificationType = checkTemporaryLimitModificationType(limit, applicability);
        boolean validMandatoryFields = checkTemporaryLimitMandatoryFields(limit, applicability);
        return validModificationType && validMandatoryFields;
    }

    /**
     * Ensure that the per-limit {@code modificationType} is consistent with the enclosing limit group modification type.
     * When the limit group is ADD or REPLACE, a non-ADD limit is auto-fixed to ADD (a detail log is emitted).
     * When the limit group is MODIFY or MODIFY_OR_ADD, a missing per-limit {@code modificationType} rejects the limit.
     * @return true if the limit may proceed, false if it must be skipped.
     */
    private boolean checkTemporaryLimitModificationType(CurrentTemporaryLimitModificationModel limit, OperationalLimitsGroupModel.Applicability applicability) {
        if (olgModifModel.getModificationType() == OperationalLimitsGroupModificationType.ADD
                || olgModifModel.getModificationType() == OperationalLimitsGroupModificationType.REPLACE) {
            // If we aren't modifying or deleting an existing limit set, temporary limit modification is necessarily of ADD type
            if (limit.getModificationType() != TemporaryLimitModificationType.ADD) {
                addToLogsOnSide(ReportNode.newRootReportNode()
                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                        .withMessageTemplate("network.modification.temporaryLimitsWrongModification")
                        .withUntypedValue("modificationType", limit.getModificationType() != null ? limit.getModificationType().name() : "null")
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build(),
                    applicability);
                limit.setModificationType(TemporaryLimitModificationType.ADD);
            }
        } else if (olgModifModel.getModificationType() == OperationalLimitsGroupModificationType.MODIFY
                || olgModifModel.getModificationType() == OperationalLimitsGroupModificationType.MODIFY_OR_ADD) {
            // For MODIFY / MODIFY_OR_ADD, the modification type is required (DELETE bypasses individual limit processing).
            if (limit.getModificationType() == null) {
                addToLogsOnSide(ReportNode.newRootReportNode()
                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                        .withMessageTemplate("network.modification.temporaryLimitsMissingModificationType")
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build(),
                    applicability);
                return false;
            }
        }
        return true;
    }

    /**
     * Ensure that name and duration are present on the limit (duration is the identifier).
     * Each missing field emits its own log so a limit missing both is reported twice.
     * @return true if both fields are present, false otherwise.
     */
    private boolean checkTemporaryLimitMandatoryFields(CurrentTemporaryLimitModificationModel limit, OperationalLimitsGroupModel.Applicability applicability) {
        boolean valid = true;
        if (!hasValue(limit.getName())) {
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.temporaryLimitsMissingName")
                    .withUntypedValue("modificationType", limit.getModificationType() != null ? limit.getModificationType().name() : "null")
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build(),
                applicability);
            valid = false;
        }
        if (!hasValue(limit.getAcceptableDuration())) {
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.temporaryLimitsMissingDuration")
                    .withUntypedValue("modificationType", limit.getModificationType() != null ? limit.getModificationType().name() : "null")
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build(),
                applicability);
            valid = false;
        }
        return valid;
    }

    /**
     * A MODIFY input can only update an existing temporary limit, it cannot create one. Reject the input
     * when no existing temporary limit matches its acceptable duration.
     * @return true if the input must be skipped because it targets no existing temporary limit; false otherwise.
     */
    private boolean isModifyWithoutMatch(
            CurrentTemporaryLimitModificationModel limit,
            LoadingLimits.TemporaryLimit limitToModify,
            OperationalLimitsGroupModel.Applicability applicability) {
        if (limit.getModificationType() != TemporaryLimitModificationType.MODIFY || limitToModify != null) {
            return false;
        }
        addToLogsOnSide(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("network.modification.temporaryLimitsNoMatch")
                .withUntypedValue(LIMIT_ACCEPTABLE_DURATION, limit.getAcceptableDuration().getValue())
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build(),
                applicability);
        return true;
    }

    /**
     * For ADD / MODIFY / MODIFY_OR_ADD, refuse to introduce a duplicate name or duration in the limit set.
     * Conflicting limits being deleted in the same batch are not considered duplicates.
     * @return true if a duplicate has been detected (line must be skipped); false otherwise.
     */
    private boolean wouldCreateDuplicate(
            Map<Integer, String> workingTemporaryLimits,
            CurrentTemporaryLimitModificationModel limit,
            OperationalLimitsGroupModel.Applicability applicability) {
        TemporaryLimitModificationType type = limit.getModificationType();
        if (type != TemporaryLimitModificationType.ADD
                && type != TemporaryLimitModificationType.MODIFY
                && type != TemporaryLimitModificationType.MODIFY_OR_ADD) {
            return false;
        }

        int duration = limit.getAcceptableDuration().getValue();
        String name = limit.getName().getValue();
        List<CurrentTemporaryLimitModificationModel> batch = olgModifModel.getCurrentLimits().getTemporaryLimits();

        boolean existingByDuration = workingTemporaryLimits.containsKey(duration);
        Optional<Map.Entry<Integer, String>> existingByName = workingTemporaryLimits.entrySet().stream()
                .filter(e -> name.equals(e.getValue()))
                .findFirst();

        boolean durationConflict;
        boolean nameConflict;

        if (type == TemporaryLimitModificationType.ADD) {
            durationConflict = existingByDuration
                    && !isThisLimitDeleted(batch, duration);
            nameConflict = existingByName.isPresent()
                    && !isThisLimitDeleted(batch, existingByName.get().getKey());
        } else {
            // MODIFY / MODIFY_OR_ADD: a limit with the same duration is the one being modified, not a duplicate.
            // Only flag a name collision with a different limit.
            durationConflict = false;
            nameConflict = existingByName.isPresent()
                && existingByName.get().getKey() != duration // It needs to be a different limit, duration is the identifier.
                && !isThisLimitDeleted(batch, existingByName.get().getKey());
        }

        if (durationConflict) {
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.temporaryLimitsDuplicateDuration")
                    .withUntypedValue("modificationType", type.name())
                    .withUntypedValue(NAME, name)
                    .withUntypedValue(DURATION, String.valueOf(duration))
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build(),
                applicability);
        }
        if (nameConflict) {
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.temporaryLimitsDuplicateName")
                    .withUntypedValue("modificationType", type.name())
                    .withUntypedValue(NAME, name)
                    .withUntypedValue(DURATION, String.valueOf(duration))
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build(),
                applicability);
        }
        return durationConflict || nameConflict;
    }

    /**
     * modify a specific limit
     * @param limitsAdder adder which receives all the "validated" limits to be added at the end
     * @param networkCurrentLimits limits of the branch which is currently modified by the network modification
     * @param limit modification to be applied to the limit
     * @param unmodifiedTemporaryLimits list of all the unmodified limits that will be added at the end of the network modification
     */
    private boolean applyTemporaryLimitModification(
        CurrentLimitsAdder limitsAdder,
        CurrentLimits networkCurrentLimits,
        CurrentTemporaryLimitModificationModel limit,
        List<LoadingLimits.TemporaryLimit> unmodifiedTemporaryLimits,
        Map<Integer, String> workingTemporaryLimits,
        OperationalLimitsGroupModel.Applicability applicability) {
        CurrentLimitsModificationModel currentLimitsModel = olgModifModel.getCurrentLimits();
        if (!preModificationCheck(limit, applicability)) {
            return false;
        }
        LoadingLimits.TemporaryLimit limitToModify = networkCurrentLimits != null
                ? getTemporaryLimitToModify(networkCurrentLimits, limit, currentLimitsModel)
                : null;
        if (isModifyWithoutMatch(limit, limitToModify, applicability)) {
            return false;
        }
        if (wouldCreateDuplicate(workingTemporaryLimits, limit, applicability)) {
            return false;
        }
        int limitAcceptableDuration = limit.getAcceptableDuration().getValue();
        double limitValue = hasValue(limit.getValue()) ? limit.getValue().getValue() : Double.MAX_VALUE;
        String limitDurationToReport = String.valueOf(limitAcceptableDuration);
        String limitValueToReport = limitValue == Double.MAX_VALUE ? NO_VALUE : String.valueOf(limitValue);
        if (networkCurrentLimits != null) {
            // this limit is modified by the network modification so we remove it from the list of unmodified temporary limits
            unmodifiedTemporaryLimits.removeIf(temporaryLimit -> temporaryLimit.getAcceptableDuration() == limitAcceptableDuration);
        }
        if (limitToModify == null && mayCreateLimit(limit.getModificationType())) {
            createTemporaryLimit(limitsAdder, limit, limitDurationToReport, limitValueToReport, limitValue, limitAcceptableDuration, applicability);
            workingTemporaryLimits.put(limitAcceptableDuration, limit.getName().getValue());
            return true;
        } else if (limitToModify != null) {
            // the limit already exists
            if (limit.getModificationType() == TemporaryLimitModificationType.DELETE) {
                // the limit has been removed previously
                addToLogsOnSide(ReportNode.newRootReportNode()
                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                        .withMessageTemplate("network.modification.temporaryLimitDeleted.name")
                        .withUntypedValue(NAME, limit.getName().getValue())
                        .withUntypedValue(DURATION, limitDurationToReport)
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build(),
                        applicability);
                workingTemporaryLimits.remove(limitAcceptableDuration);
            } else {
                modifyTemporaryLimit(limitsAdder, limit, limitToModify, limitValue, limitDurationToReport, limitAcceptableDuration, applicability);
                workingTemporaryLimits.put(limitAcceptableDuration, limit.getName().getValue());
            }
            return true;
        }
        return false;
    }

    /**
     * is the limit identified by acceptableDuration deleted in temporaryLimitsModification ?
     */
    public boolean isThisLimitDeleted(List<CurrentTemporaryLimitModificationModel> temporaryLimitsModification, int acceptableDuration) {
        return temporaryLimitsModification.stream()
                .anyMatch(temporaryLimit -> temporaryLimit.getModificationType() == TemporaryLimitModificationType.DELETE
                        && hasValue(temporaryLimit.getAcceptableDuration())
                        && hasValue(temporaryLimit.getName())
                        && temporaryLimit.getAcceptableDuration().getValue() == acceptableDuration);
    }

    private LoadingLimits.TemporaryLimit getTemporaryLimitToModify(
            CurrentLimits networkCurrentLimits,
            CurrentTemporaryLimitModificationModel limit,
            CurrentLimitsModificationModel currentLimitsModel) {
        int limitAcceptableDuration = limit.getAcceptableDuration().getValue();
        LoadingLimits.TemporaryLimit limitToModify = networkCurrentLimits.getTemporaryLimit(limitAcceptableDuration);
        // Treat the matched limit as missing if it is being deleted in the same batch
        if (limitToModify != null
                && !limitToModify.getName().equals(limit.getName().getValue())
                && isThisLimitDeleted(currentLimitsModel.getTemporaryLimits(), limitAcceptableDuration)) {
            limitToModify = null;
        }
        return limitToModify;
    }

    private boolean hasValue(AttributeModification<?> attribute) {
        return attribute != null && attribute.getValue() != null;
    }

    private boolean hasModification(AttributeModification<?> attribute) {
        return hasValue(attribute) && attribute.getOp().equals(OperationType.SET);
    }

    public void modifyTemporaryLimit(
            CurrentLimitsAdder limitsAdder,
            CurrentTemporaryLimitModificationModel limitModificationModel,
            LoadingLimits.TemporaryLimit limitToModify,
            double limitValue,
            String limitDurationToReport,
            int limitAcceptableDuration,
            OperationalLimitsGroupModel.Applicability applicability) {
        boolean isReplace = limitModificationModel.getModificationType() == TemporaryLimitModificationType.REPLACE;

        // The name and acceptable duration are mandatory at this point.
        // For REPLACE: take the provided value, a missing value has already been converted to Double.MAX_VALUE.
        // For others: keep the existing value when it is not explicitly modified.
        String finalName = limitModificationModel.getName().getValue();

        double finalValue = (isReplace || hasModification(limitModificationModel.getValue()))
                ? limitValue
                : limitToModify.getValue();

        // Check if there are any actual changes
        boolean nameChanged = !limitToModify.getName().equals(finalName);
        boolean valueChanged = Double.compare(limitToModify.getValue(), finalValue) != 0;
        String finalValueToReport = finalValue == Double.MAX_VALUE ? NO_VALUE : String.valueOf(finalValue);

        if (valueChanged && !nameChanged) {
            // only the value changes
            addToLogsOnSide(ReportNode.newRootReportNode()
                            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                            .withMessageTemplate("network.modification.temporaryLimitValueModified.name")
                            .withUntypedValue(AbstractBranchModification.NAME, finalName)
                            .withUntypedValue(DURATION, limitDurationToReport)
                            .withUntypedValue(AbstractBranchModification.VALUE, finalValueToReport)
                            .withUntypedValue("oldValue",
                                    limitToModify.getValue() == Double.MAX_VALUE ? NO_VALUE
                                            : String.valueOf(limitToModify.getValue()))
                            .withSeverity(TypedValue.DETAIL_SEVERITY)
                            .build(),
                    applicability);
        } else if (nameChanged) { // || valueChanged is not necessary, because we would enter above
            // log only if there is at least one actual modification
            addToLogsOnSide(ReportNode.newRootReportNode()
                            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                            .withMessageTemplate("network.modification.temporaryLimitModified.name")
                            .withUntypedValue(NAME, finalName)
                            .withUntypedValue(VALUE, finalValueToReport)
                            .withUntypedValue(DURATION, limitAcceptableDuration)
                            .withSeverity(TypedValue.DETAIL_SEVERITY)
                            .build(),
                    applicability);
        }
        addTemporaryLimit(limitsAdder, finalName, finalValue, limitAcceptableDuration);
    }

    public void createTemporaryLimit(
            CurrentLimitsAdder limitsAdder,
            CurrentTemporaryLimitModificationModel limit,
            String limitDurationToReport,
            String limitValueToReport,
            double limitValue,
            int limitAcceptableDuration,
            OperationalLimitsGroupModel.Applicability applicability) {
        addToLogsOnSide(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("network.modification.temporaryLimitModified.name")
                .withUntypedValue(AbstractBranchModification.NAME, limit.getName().getValue())
                .withUntypedValue(DURATION, limitDurationToReport)
                .withUntypedValue(AbstractBranchModification.VALUE, limitValueToReport)
                .withSeverity(TypedValue.DETAIL_SEVERITY)
                .build(),
                applicability);
        addTemporaryLimit(limitsAdder, limit.getName().getValue(), limitValue, limitAcceptableDuration);
    }
}
