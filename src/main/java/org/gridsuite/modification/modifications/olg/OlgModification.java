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
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.AbstractBranchModification;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.*;
import static org.gridsuite.modification.modifications.AbstractBranchModification.*;

/**
 * handles the modification of a single operational limits group from AbstractBranchModification
 * it may affect both sides of an operational limits group
 *
 * @author Mathieu DEHARBE <mathieu.deharbe at rte-france.com>
 */
public class OlgModification {
    private final Branch<?> modifiedBranch; // branch modified by the network modification
    private final OperationalLimitsGroupModificationInfos olgModifInfos;
    private final ReportNode olgsReportNode;
    List<ReportNode> limitsReportsSide1;
    List<ReportNode> limitsReportsSide2;

    public OlgModification(
            Branch<?> modifiedBranch,
            OperationalLimitsGroupModificationInfos olgModifInfos,
            ReportNode limitSetsReportNode) {
        this.modifiedBranch = modifiedBranch;
        this.olgModifInfos = olgModifInfos;
        olgsReportNode = limitSetsReportNode;
        limitsReportsSide1 = new ArrayList<>();
        limitsReportsSide2 = new ArrayList<>();
    }

    private void addToLogsOnSide(ReportNode reportNode, OperationalLimitsGroupInfos.Applicability applicability) {
        if (applicability == EQUIPMENT || applicability == SIDE1) {
            limitsReportsSide1.add(reportNode);
        }
        if (applicability == EQUIPMENT || applicability == SIDE2) {
            limitsReportsSide2.add(reportNode);
        }
    }

    private OperationalLimitsGroup modifiedOperationalLimitsGroup1() {
        return modifiedBranch.getOperationalLimitsGroup1(olgModifInfos.getId()).orElse(null);
    }

    private OperationalLimitsGroup modifiedOperationalLimitsGroup2() {
        return modifiedBranch.getOperationalLimitsGroup2(olgModifInfos.getId()).orElse(null);
    }

    protected void applyModificationToOperationalLimitsGroup() {
        switch (olgModifInfos.getModificationType()) {
            case OperationalLimitsGroupModificationType.MODIFY_OR_ADD: {
                switch (olgModifInfos.getApplicability()) {
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
                            addOlg(olgModifInfos.getApplicability());
                        } else {
                            modifyOLG(olgModifInfos.getApplicability());
                        }
                        break;
                    case SIDE2 :
                        if (modifiedOperationalLimitsGroup2() == null) {
                            addOlg(olgModifInfos.getApplicability());
                        } else {
                            modifyOLG(olgModifInfos.getApplicability());
                        }
                        break;
                }
            } break;
            case OperationalLimitsGroupModificationType.MODIFY: {
                if (applicableOnSide1() && modifiedOperationalLimitsGroup1() == null) {
                    throw new PowsyblException("Cannot modify operational limit group " + olgModifInfos.getId() + " which has not been found in equipment side 1");
                }
                if (applicableOnSide2() && modifiedOperationalLimitsGroup2() == null) {
                    throw new PowsyblException("Cannot modify operational limit group " + olgModifInfos.getId() + " which has not been found in equipment side 2");
                }
                modifyOLG(olgModifInfos.getApplicability());
            } break;
            case OperationalLimitsGroupModificationType.ADD: {
                addOlg(olgModifInfos.getApplicability());
            } break;
            case OperationalLimitsGroupModificationType.REPLACE: {
                replaceOpLG();
            } break;
            case DELETE: {
                removeOlg();
            }
        }
    }

    private boolean applicableOnSide1() {
        return olgModifInfos.getApplicability() == SIDE1 || olgModifInfos.getApplicability() == EQUIPMENT;
    }

    private boolean applicableOnSide2() {
        return olgModifInfos.getApplicability() == SIDE2 || olgModifInfos.getApplicability() == EQUIPMENT;
    }

    private void modifyOLG(OperationalLimitsGroupInfos.Applicability applicability) {
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
                    .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, olgModifInfos.getId())
                    .withUntypedValue(SIDE, applicabilityToString(applicability))
                    .withSeverity(TypedValue.INFO_SEVERITY).add();
            logSideNode(limitSetReport, "network.modification.operationalLimitsGroupModified.detail", SIDE1);
            logSideNode(limitSetReport, "network.modification.operationalLimitsGroupModified.detail", SIDE2);
        }
    }

    /**
     * creates a log node specific for each side
     */
    private void logSideNode(ReportNode limitSetReport, String messageTemplate, OperationalLimitsGroupInfos.Applicability applicability) {
        List<ReportNode> limitsReports;
        if (applicability == SIDE1) {
            limitsReports = limitsReportsSide1;
        } else {
            limitsReports = limitsReportsSide2;
        }
        if (!limitsReports.isEmpty()) {
            ReportNode limitSetReportDetail = limitSetReport.newReportNode()
                    .withMessageTemplate(messageTemplate)
                    .withUntypedValue(SIDE, applicabilityToString(applicability))
                    .withSeverity(TypedValue.DETAIL_SEVERITY).add();
            ModificationUtils.getInstance().reportModifications(limitSetReportDetail, limitsReports);
        }
    }

    private void modifyProperties(OperationalLimitsGroup limitsGroup, OperationalLimitsGroupInfos.Applicability applicability) {
        if (limitsGroup == null || olgModifInfos == null) {
            return;
        }

        Set<String> currentProperties = limitsGroup.getPropertyNames();

        List<LimitsPropertyInfos> propertiesToModify = new ArrayList<>();
        List<LimitsPropertyInfos> propertiesToAdd = new ArrayList<>();
        List<String> propertiesToRemove;

        if (!CollectionUtils.isEmpty(olgModifInfos.getLimitsProperties())) {
            for (LimitsPropertyInfos propertyInfos : olgModifInfos.getLimitsProperties()) {
                if (currentProperties.contains(propertyInfos.name())) {
                    propertiesToModify.add(propertyInfos);
                } else {
                    propertiesToAdd.add(propertyInfos);
                }
            }

            propertiesToRemove = currentProperties.stream().filter(
                    (String propertyName) -> propertiesToModify.stream().filter(propertyInfos ->
                            propertyInfos.name().equals(propertyName)).toList().isEmpty()).toList();
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

        propertiesToModify.forEach((LimitsPropertyInfos property) -> {
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

        propertiesToAdd.forEach((LimitsPropertyInfos property) -> {
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
            OperationalLimitsGroupInfos.Applicability applicability) {
        CurrentLimitsModificationInfos currentLimitsInfos = olgModifInfos.getCurrentLimits();
        boolean hasPermanent = currentLimitsInfos.getPermanentLimit() != null;
        if (hasPermanent) {
            if (!(currentLimits != null && currentLimits.getPermanentLimit() == currentLimitsInfos.getPermanentLimit())) {
                addToLogsOnSide(ModificationUtils.buildModificationReport(
                        currentLimits != null ? currentLimits.getPermanentLimit() : Double.NaN, currentLimitsInfos.getPermanentLimit(),
                                "Permanent limit",
                                TypedValue.DETAIL_SEVERITY
                        ),
                        applicability);
            }
            limitsAdder.setPermanentLimit(currentLimitsInfos.getPermanentLimit());
        } else {
            if (currentLimits != null) {
                limitsAdder.setPermanentLimit(currentLimits.getPermanentLimit());
            }
        }
        modifyTemporaryLimits(limitsAdder, currentLimits, applicability);
        limitsAdder.add();
    }

    private void addOlg(OperationalLimitsGroupInfos.Applicability applicability) {
        if (applicability == EQUIPMENT || applicability == SIDE1) {
            if (modifiedOperationalLimitsGroup1() != null) {
                throw new PowsyblException("Cannot add " + modifiedOperationalLimitsGroup1().getId() + " operational limit group, one with the given name already exists");
            }
            addOlgOnASide(modifiedBranch.newOperationalLimitsGroup1(olgModifInfos.getId()), SIDE1);
        }
        if (applicability == EQUIPMENT || applicability == SIDE2) {
            if (modifiedOperationalLimitsGroup2() != null) {
                throw new PowsyblException("Cannot add " + modifiedOperationalLimitsGroup2().getId() + " operational limit group, one with the given name already exists");
            }
            addOlgOnASide(modifiedBranch.newOperationalLimitsGroup2(olgModifInfos.getId()), SIDE2);
        }

        if (!CollectionUtils.isEmpty(limitsReportsSide1) || !CollectionUtils.isEmpty(limitsReportsSide2)) {
            ReportNode limitSetReport = olgsReportNode.newReportNode()
                    .withMessageTemplate("network.modification.operationalLimitsGroupAdded")
                    .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, olgModifInfos.getId())
                    .withUntypedValue(SIDE, applicabilityToString(applicability))
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            logSideNode(limitSetReport, "network.modification.operationalLimitsGroupAdded.detail", SIDE1);
            logSideNode(limitSetReport, "network.modification.operationalLimitsGroupAdded.detail", SIDE2);
        }
    }

    private String applicabilityToString(OperationalLimitsGroupInfos.Applicability applicability) {
        return switch (applicability) {
            case EQUIPMENT -> "sides 1 & 2";
            case SIDE1 -> "side 1";
            case SIDE2 -> "side 2";
        };
    }

    private void addOlgOnASide(OperationalLimitsGroup newOperationalLimitsGroup, OperationalLimitsGroupInfos.Applicability applicability) {
        modifyCurrentLimits(newOperationalLimitsGroup.newCurrentLimits(), newOperationalLimitsGroup.getCurrentLimits().orElse(null), applicability);
        addProperties(newOperationalLimitsGroup, applicability);
    }

    private void replaceOpLG() {
        if (applicableOnSide1()) {
            OperationalLimitsGroup modifiedOlg = modifiedOperationalLimitsGroup1();
            if (modifiedOlg != null) {
                modifiedOlg.removeCurrentLimits();
                removeAllProperties(modifiedOlg);
            }
            OperationalLimitsGroup newOperationalLimitsGroup = modifiedBranch.newOperationalLimitsGroup1(olgModifInfos.getId());
            modifyCurrentLimits(newOperationalLimitsGroup.newCurrentLimits(), null, SIDE1);
            addProperties(newOperationalLimitsGroup, SIDE1);
        }
        if (applicableOnSide2()) {
            OperationalLimitsGroup modifiedOlg = modifiedOperationalLimitsGroup2();
            if (modifiedOlg != null) {
                modifiedOlg.removeCurrentLimits();
                removeAllProperties(modifiedOlg);
            }
            OperationalLimitsGroup newOperationalLimitsGroup = modifiedBranch.newOperationalLimitsGroup2(olgModifInfos.getId());
            modifyCurrentLimits(newOperationalLimitsGroup.newCurrentLimits(), null, SIDE2);
            addProperties(newOperationalLimitsGroup, SIDE2);
        }

        if (!CollectionUtils.isEmpty(limitsReportsSide1) || !CollectionUtils.isEmpty(limitsReportsSide2)) {
            ReportNode limitSetReport = olgsReportNode.newReportNode()
                    .withMessageTemplate("network.modification.operationalLimitsGroupReplaced")
                    .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, olgModifInfos.getId())
                    .withUntypedValue(SIDE, applicabilityToString(olgModifInfos.getApplicability()))
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            logSideNode(limitSetReport, "network.modification.operationalLimitsGroupAdded.detail", SIDE1);
            logSideNode(limitSetReport, "network.modification.operationalLimitsGroupAdded.detail", SIDE2);
        }
    }

    private void addProperties(OperationalLimitsGroup limitsGroup, OperationalLimitsGroupInfos.Applicability applicability) {
        if (limitsGroup == null || CollectionUtils.isEmpty(olgModifInfos.getLimitsProperties())) {
            return;
        }

        olgModifInfos.getLimitsProperties().forEach((LimitsPropertyInfos property) -> {
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
        String olgId = olgModifInfos.getId();
        if (applicableOnSide1() && modifiedBranch.getOperationalLimitsGroup1(olgId).isEmpty() ||
            applicableOnSide2() && modifiedBranch.getOperationalLimitsGroup2(olgId).isEmpty()) {
            throw new PowsyblException(
                    "Cannot delete operational limit group " + olgId + " which has not been found in equipment on " + applicabilityToString(olgModifInfos.getApplicability()));
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
                .withUntypedValue(SIDE, applicabilityToString(olgModifInfos.getApplicability()))
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
                    olgModifInfos.getApplicability());
        }
    }

    /**
     * This function removes all the temporary limits of the 'currentLimits' concerned and recreates them (except in case of deletion)
     */
    protected void modifyTemporaryLimits(CurrentLimitsAdder limitsAdder, CurrentLimits currentLimits, OperationalLimitsGroupInfos.Applicability applicability) {
        CurrentLimitsModificationInfos currentLimitsInfos = olgModifInfos.getCurrentLimits();

        // we create a mutable list of temporary limits to be able to remove the limits that are modified in this current modification
        // those left at the end of the network modification are those that have not been modified (or deleted)
        List<LoadingLimits.TemporaryLimit> unmodifiedTemporaryLimits = new ArrayList<>();
        boolean areLimitsReplaced = TemporaryLimitModificationType.REPLACE.equals(olgModifInfos.getTemporaryLimitsModificationType());
        if (currentLimits != null) {
            unmodifiedTemporaryLimits.addAll(currentLimits.getTemporaryLimits());
        }

        if (currentLimitsInfos != null && currentLimitsInfos.getTemporaryLimits() != null) {
            for (CurrentTemporaryLimitModificationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
                applyTemporaryLimitModification(
                        limitsAdder,
                        currentLimits,
                        limit,
                        unmodifiedTemporaryLimits,
                        applicability
                );
            }
        }

        if (!unmodifiedTemporaryLimits.isEmpty()) {
            if (areLimitsReplaced) {
                // this needs to be logged only if there are unmodifiedTemporaryLimits left.
                // which means that they are going to be removed by the REPLACE mode
                addToLogsOnSide(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.temporaryLimitsReplaced")
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build(),
                        applicability);
            } else {
                // we add (back) the temporary limits that have not been modified
                for (LoadingLimits.TemporaryLimit limit : unmodifiedTemporaryLimits) {
                    addTemporaryLimit(limitsAdder, limit.getName(), limit.getValue(), limit.getAcceptableDuration(), applicability);
                }
            }
        }
    }

    /**
     * modify a specific limit
     * @param limitsAdder adder which receives all the "validated" limits to be added at the end
     * @param networkCurrentLimits limits of the branch which is currently modified by the network modification
     * @param limit modification to be applied to the limit
     * @param unmodifiedTemporaryLimits list of all the unmodified limits that will be added at the end of the network modification
     */
    private void applyTemporaryLimitModification(
            CurrentLimitsAdder limitsAdder,
            CurrentLimits networkCurrentLimits,
            CurrentTemporaryLimitModificationInfos limit,
            List<LoadingLimits.TemporaryLimit> unmodifiedTemporaryLimits,
            OperationalLimitsGroupInfos.Applicability applicability) {
        CurrentLimitsModificationInfos currentLimitsInfos = olgModifInfos.getCurrentLimits();
        int limitAcceptableDuration = limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration();
        double limitValue = limit.getValue() == null ? Double.MAX_VALUE : limit.getValue();
        String limitDurationToReport = limitAcceptableDuration == Integer.MAX_VALUE ? " " : String.valueOf(limitAcceptableDuration);
        String limitValueToReport = limitValue == Double.MAX_VALUE ? "no value" : String.valueOf(limitValue);
        LoadingLimits.TemporaryLimit limitToModify = null;
        if (networkCurrentLimits != null) {
            limitToModify = getTemporaryLimitToModify(networkCurrentLimits, limit, currentLimitsInfos, olgModifInfos.getTemporaryLimitsModificationType());
            // this limit is modified by the network modification so we remove it from the list of unmodified temporary limits
            unmodifiedTemporaryLimits.removeIf(temporaryLimit -> temporaryLimit.getAcceptableDuration() == limitAcceptableDuration);
        }
        if (limitToModify == null && mayCreateALimit(limit.getModificationType())) {
            createTemporaryLimit(limitsAdder, limit, limitDurationToReport, limitValueToReport, limitValue, limitAcceptableDuration, applicability);
        } else if (limitToModify != null) {
            // the limit already exists
            if (limit.getModificationType() == TemporaryLimitModificationType.DELETE) {
                // the limit has been removed previously
                addToLogsOnSide(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.temporaryLimitDeleted.name")
                        .withUntypedValue(NAME, limit.getName())
                        .withUntypedValue(DURATION, limitDurationToReport)
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build(),
                        applicability);
            } else {
                modifyTemporaryLimit(limitsAdder, limit, limitToModify, limitValue, limitDurationToReport, limitValueToReport, limitAcceptableDuration, applicability);
            }
        } else if (limit.getModificationType() == TemporaryLimitModificationType.MODIFY || limit.getModificationType() == TemporaryLimitModificationType.MODIFY_OR_ADD) {
            // invalid modification
            addToLogsOnSide(ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.temporaryLimitsNoMatch")
                    .withUntypedValue(LIMIT_ACCEPTABLE_DURATION, limitAcceptableDuration)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build(),
                    applicability);
        }
    }

    /**
     * is the limit identified by acceptableDuration deleted in temporaryLimitsModification ?
     */
    public boolean isThisLimitDeleted(List<CurrentTemporaryLimitModificationInfos> temporaryLimitsModification, int acceptableDuration) {
        return temporaryLimitsModification.stream()
                .filter(temporaryLimit -> temporaryLimit.getAcceptableDuration() != null)
                .anyMatch(temporaryLimit ->
                        temporaryLimit.getAcceptableDuration() == acceptableDuration && temporaryLimit.getModificationType() == TemporaryLimitModificationType.DELETE
                );
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

    public void modifyTemporaryLimit(
            CurrentLimitsAdder limitsAdder,
            CurrentTemporaryLimitModificationInfos limitModificationInfos,
            LoadingLimits.TemporaryLimit limitToModify,
            double limitValue,
            String limitDurationToReport,
            String limitValueToReport,
            int limitAcceptableDuration,
            OperationalLimitsGroupInfos.Applicability applicability) {
        if (Double.compare(limitToModify.getValue(), limitValue) != 0 && limitModificationInfos.getModificationType() != null) {
            // value change
            addToLogsOnSide(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.temporaryLimitValueModified.name")
                            .withUntypedValue(AbstractBranchModification.NAME, limitModificationInfos.getName())
                            .withUntypedValue(DURATION, limitDurationToReport)
                            .withUntypedValue(AbstractBranchModification.VALUE, limitValueToReport)
                            .withUntypedValue("oldValue",
                                    limitToModify.getValue() == Double.MAX_VALUE ? "no value"
                                            : String.valueOf(limitToModify.getValue()))
                            .withSeverity(TypedValue.DETAIL_SEVERITY)
                            .build(),
                    applicability);
            addTemporaryLimit(limitsAdder, limitModificationInfos.getName(), limitValue, limitAcceptableDuration, applicability);
        } else {
            addTemporaryLimit(limitsAdder, limitModificationInfos.getName(), limitToModify.getValue(), limitAcceptableDuration, applicability);
        }
    }

    public void addTemporaryLimit(CurrentLimitsAdder limitsAdder,
                                  String limit,
                                  double limitValue,
                                  int limitAcceptableDuration,
                                  OperationalLimitsGroupInfos.Applicability applicability) {
        AbstractBranchModification.addTemporaryLimit(limitsAdder, limit, limitValue, limitAcceptableDuration);
        addToLogsOnSide(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.temporaryLimitModified.name")
                        .withUntypedValue(NAME, limit)
                        .withUntypedValue(VALUE, limitValue)
                        .withUntypedValue(DURATION, limitAcceptableDuration)
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build(),
                applicability);
    }

    public void createTemporaryLimit(
            CurrentLimitsAdder limitsAdder,
            CurrentTemporaryLimitModificationInfos limit,
            String limitDurationToReport,
            String limitValueToReport,
            double limitValue,
            int limitAcceptableDuration,
            OperationalLimitsGroupInfos.Applicability applicability) {
        addToLogsOnSide(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("network.modification.temporaryLimitAdded.name")
                .withUntypedValue(AbstractBranchModification.NAME, limit.getName())
                .withUntypedValue(DURATION, limitDurationToReport)
                .withUntypedValue(AbstractBranchModification.VALUE, limitValueToReport)
                .withSeverity(TypedValue.DETAIL_SEVERITY)
                .build(),
                olgModifInfos.getApplicability());
        addTemporaryLimit(limitsAdder, limit.getName(), limitValue, limitAcceptableDuration, applicability);
    }
}
