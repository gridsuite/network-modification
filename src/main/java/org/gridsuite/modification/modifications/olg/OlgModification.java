package org.gridsuite.modification.modifications.olg;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.OlgUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.*;
import static org.gridsuite.modification.modifications.AbstractBranchModification.NAME;
import static org.gridsuite.modification.modifications.AbstractBranchModification.VALUE;

/**
 * handles the modification of a single operational limits group from AbstractBranchModification
 * it may affect both sides of an operational limits group
 */
public class OlgModification {
    private final Branch<?> modifiedBranch; // branch modified by the network modification
    private final OperationalLimitsGroupModificationInfos olgModifInfos;
    private final ReportNode olgsReportNode;

    public OlgModification(
            Branch<?> modifiedBranch,
            OperationalLimitsGroupModificationInfos olgModifInfos,
            ReportNode limitSetsReportNode) {
        this.modifiedBranch = modifiedBranch;
        this.olgModifInfos = olgModifInfos;
        olgsReportNode = limitSetsReportNode;
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
                            // TODO : attention quand un est valide et l'autre non
                            addOlg();
                        } else {
                            modifyOLG();
                        }
                        break;
                    case SIDE1 :
                        if (modifiedOperationalLimitsGroup1() == null) {
                            addOlg();
                        } else {
                            modifyOLG();
                        }
                        break;
                    case SIDE2 :
                        if (modifiedOperationalLimitsGroup2() == null) {
                            addOlg();
                        } else {
                            modifyOLG();
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
                modifyOLG();
            } break;
            case OperationalLimitsGroupModificationType.ADD: {
                addOlg();
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

    private void modifyOLG() {

        List<ReportNode> limitSetsReports = new ArrayList<>();

        if (applicableOnSide1()) {
            modifiedOperationalLimitsGroup1().getCurrentLimits().ifPresent(currentLimits -> {
                modifyCurrentLimits(modifiedOperationalLimitsGroup1().newCurrentLimits(), currentLimits, limitSetsReports);
                modifyProperties(modifiedOperationalLimitsGroup1(), limitSetsReports);
            });
        }
        if (applicableOnSide2()) {
            modifiedOperationalLimitsGroup2().getCurrentLimits().ifPresent(currentLimits -> {
                modifyCurrentLimits(modifiedOperationalLimitsGroup2().newCurrentLimits(), currentLimits, limitSetsReports);
                modifyProperties(modifiedOperationalLimitsGroup2(), limitSetsReports);
            });
        }

        if (!limitSetsReports.isEmpty()) {
            ReportNode limitSetReport = olgsReportNode.newReportNode()
                    .withMessageTemplate("network.modification.operationalLimitsGroupModified")
                    .withUntypedValue(OlgUtils.OPERATIONAL_LIMITS_GROUP_NAME, olgModifInfos.getId())
                    .withUntypedValue(OlgUtils.SIDE, applicabilityToString(olgModifInfos.getApplicability()))
                    .withSeverity(TypedValue.INFO_SEVERITY).add();
            ModificationUtils.getInstance().reportModifications(limitSetReport, limitSetsReports);
        }
    }

    private void modifyProperties(OperationalLimitsGroup limitsGroup,
                                  List<ReportNode> limitSetsReports) {
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
            limitSetsReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyDeleted")
                    .withUntypedValue(NAME, propertyName)
                    .withSeverity(TypedValue.DETAIL_SEVERITY).build());
        });

        propertiesToModify.forEach((LimitsPropertyInfos property) -> {
            // Skip changes when value does not change
            if (limitsGroup.getProperty(property.name()).equals(property.value())) {
                return;
            }
            limitSetsReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyChanged")
                    .withUntypedValue(NAME, property.name())
                    .withUntypedValue("to", property.value())
                    .withUntypedValue("from", limitsGroup.getProperty(property.name()))
                    .withSeverity(TypedValue.DETAIL_SEVERITY).build());
            limitsGroup.setProperty(property.name(), property.value());
        });

        propertiesToAdd.forEach((LimitsPropertyInfos property) -> {
            limitSetsReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyAdded")
                    .withUntypedValue(NAME, property.name())
                    .withUntypedValue(VALUE, property.value())
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build());
            limitsGroup.setProperty(property.name(), property.value());
        });
    }

    protected void modifyCurrentLimits(
            CurrentLimitsAdder limitsAdder,
            CurrentLimits currentLimits,
            List<ReportNode> limitsReports) {
        CurrentLimitsModificationInfos currentLimitsInfos = olgModifInfos.getCurrentLimits();
        boolean hasPermanent = currentLimitsInfos.getPermanentLimit() != null;
        if (hasPermanent) {
            if (!(currentLimits != null && currentLimits.getPermanentLimit() == currentLimitsInfos.getPermanentLimit())) {
                limitsReports.add(
                        ModificationUtils.buildModificationReport(
                                currentLimits != null ? currentLimits.getPermanentLimit() : Double.NaN, currentLimitsInfos.getPermanentLimit(),
                                "Permanent limit",
                                TypedValue.DETAIL_SEVERITY
                        ));
            }
            limitsAdder.setPermanentLimit(currentLimitsInfos.getPermanentLimit());
        } else {
            if (currentLimits != null) {
                limitsAdder.setPermanentLimit(currentLimits.getPermanentLimit());
            }
        }
        modifyTemporaryLimits(limitsAdder, currentLimits, limitsReports);
        limitsAdder.add();
    }

    private void addOlg() {

        List<ReportNode> limitSetReports = new ArrayList<>();
        if (olgModifInfos.getApplicability() == EQUIPMENT || olgModifInfos.getApplicability() == SIDE1) {
            if (modifiedOperationalLimitsGroup1() != null) {
                throw new PowsyblException("Cannot add " + modifiedOperationalLimitsGroup1().getId() + " operational limit group, one with the given name already exists");
            }
            addOlgOnASide(modifiedBranch.newOperationalLimitsGroup1(olgModifInfos.getId()), limitSetReports);
        }
        if (olgModifInfos.getApplicability() == EQUIPMENT || olgModifInfos.getApplicability() == SIDE2) {
            if (modifiedOperationalLimitsGroup2() != null) {
                throw new PowsyblException("Cannot add " + modifiedOperationalLimitsGroup2().getId() + " operational limit group, one with the given name already exists");
            }
            addOlgOnASide(modifiedBranch.newOperationalLimitsGroup2(olgModifInfos.getId()), limitSetReports);
        }

        if (!CollectionUtils.isEmpty(limitSetReports)) {
            ReportNode limitSetNode = olgsReportNode.newReportNode()
                    .withMessageTemplate("network.modification.operationalLimitsGroupAdded")
                    .withUntypedValue(OlgUtils.OPERATIONAL_LIMITS_GROUP_NAME, olgModifInfos.getId())
                    .withUntypedValue(OlgUtils.SIDE, applicabilityToString(olgModifInfos.getApplicability()))
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            ModificationUtils.getInstance().reportModifications(limitSetNode, limitSetReports);
        }
    }

    private String applicabilityToString(OperationalLimitsGroupInfos.Applicability applicability) {
        return switch (applicability) {
            case EQUIPMENT -> "sides 1 & 2";
            case SIDE1 -> "side 1";
            case SIDE2 -> "side 2";
        };
    }

    private void addOlgOnASide(OperationalLimitsGroup newOperationalLimitsGroup, List<ReportNode> limitSetReports) {
        modifyCurrentLimits(newOperationalLimitsGroup.newCurrentLimits(),
                newOperationalLimitsGroup.getCurrentLimits().orElse(null), limitSetReports);
        addProperties(newOperationalLimitsGroup, limitSetReports);
    }

    private void replaceOpLG() {
        List<ReportNode> limitSetReports = new ArrayList<>();
        if (applicableOnSide1()) {
            OperationalLimitsGroup modifiedOlg = modifiedOperationalLimitsGroup1();
            if (modifiedOlg != null) {
                modifiedOlg.removeCurrentLimits();
                removeAllProperties(modifiedOlg, limitSetReports);
            }
            OperationalLimitsGroup newOperationalLimitsGroup = modifiedBranch.newOperationalLimitsGroup1(olgModifInfos.getId());
            modifyCurrentLimits(newOperationalLimitsGroup.newCurrentLimits(), null, limitSetReports);
            addProperties(newOperationalLimitsGroup, limitSetReports);
        }
        if (applicableOnSide2()) {
            OperationalLimitsGroup modifiedOlg = modifiedOperationalLimitsGroup2();
            if (modifiedOlg != null) {
                modifiedOlg.removeCurrentLimits();
                removeAllProperties(modifiedOlg, limitSetReports);
            }
            OperationalLimitsGroup newOperationalLimitsGroup = modifiedBranch.newOperationalLimitsGroup2(olgModifInfos.getId());
            modifyCurrentLimits(newOperationalLimitsGroup.newCurrentLimits(), null, limitSetReports);
            addProperties(newOperationalLimitsGroup, limitSetReports);
        }

        if (!CollectionUtils.isEmpty(limitSetReports)) {
            ReportNode reportNode = olgsReportNode.newReportNode()
                    .withMessageTemplate("network.modification.operationalLimitsGroupReplaced")
                    .withUntypedValue(OlgUtils.OPERATIONAL_LIMITS_GROUP_NAME, olgModifInfos.getId())
                    .withUntypedValue(OlgUtils.SIDE, applicabilityToString(olgModifInfos.getApplicability()))
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            ModificationUtils.getInstance().reportModifications(reportNode, limitSetReports);
        }
    }

    private void addProperties(OperationalLimitsGroup limitsGroup, List<ReportNode> limitSetsReports) {
        if (limitsGroup == null || CollectionUtils.isEmpty(olgModifInfos.getLimitsProperties())) {
            return;
        }

        olgModifInfos.getLimitsProperties().forEach((LimitsPropertyInfos property) -> {
            limitSetsReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyAdded")
                    .withUntypedValue(NAME, property.name())
                    .withUntypedValue(VALUE, property.value())
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build());
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
                .withUntypedValue(OlgUtils.OPERATIONAL_LIMITS_GROUP_NAME, olgId)
                .withUntypedValue(OlgUtils.SIDE, applicabilityToString(olgModifInfos.getApplicability()))
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void removeAllProperties(OperationalLimitsGroup limitsGroup, List<ReportNode> limitSetsReports) {

        if (limitsGroup == null) {
            return;
        }

        Iterator<String> propertiesIt = limitsGroup.getPropertyNames().iterator();
        while (propertiesIt.hasNext()) {
            String propertyName = propertiesIt.next();
            limitsGroup.removeProperty(propertyName);
            limitSetsReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.propertyDeleted")
                    .withUntypedValue(NAME, propertyName)
                    .withSeverity(TypedValue.DETAIL_SEVERITY).build());
        }
    }

    /**
     * This function removes all the temporary limits of the 'currentLimits' concerned and recreates them (except in case of deletion)
     */
    protected void modifyTemporaryLimits(CurrentLimitsAdder limitsAdder,
                                         CurrentLimits currentLimits,
                                         List<ReportNode> limitsReports) {
        CurrentLimitsModificationInfos currentLimitsInfos = olgModifInfos.getCurrentLimits();

        // we create a mutable list of temporary limits to be able to remove the limits that are modified in this current modification
        // those left at the end of the network modification are those that have not been modified (or deleted)
        List<LoadingLimits.TemporaryLimit> unmodifiedTemporaryLimits = new ArrayList<>();
        boolean areLimitsReplaced = TemporaryLimitModificationType.REPLACE.equals(olgModifInfos.getTemporaryLimitsModificationType());
        if (currentLimits != null) {
            unmodifiedTemporaryLimits.addAll(currentLimits.getTemporaryLimits());
        }
        List<ReportNode> temporaryLimitsReports = new ArrayList<>();

        if (currentLimitsInfos != null && currentLimitsInfos.getTemporaryLimits() != null) {
            for (CurrentTemporaryLimitModificationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
                applyTemporaryLimitModification(
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
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build());
            } else {
                // we add (back) the temporary limits that have not been modified
                for (LoadingLimits.TemporaryLimit limit : unmodifiedTemporaryLimits) {
                    OlgUtils.addTemporaryLimit(limitsAdder, limit.getName(), limit.getValue(), limit.getAcceptableDuration());
                }
            }
        }
        if (!temporaryLimitsReports.isEmpty()) {
            temporaryLimitsReports.addFirst(ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.temporaryLimitsModification")
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build());
            limitsReports.addAll(temporaryLimitsReports);
        }
    }

    /**
     * modify a specific limit
     * @param limitsAdder adder which receives all the "validated" limits to be added at the end
     * @param networkCurrentLimits limits of the branch which is currently modified by the network modification
     * @param limit modification to be applied to the limit
     * @param unmodifiedTemporaryLimits list of all the unmodified limits that will be added at the end of the network modification
     * @param temporaryLimitsReports log report
     */
    private void applyTemporaryLimitModification(
            CurrentLimitsAdder limitsAdder,
            CurrentLimits networkCurrentLimits,
            CurrentTemporaryLimitModificationInfos limit,
            List<LoadingLimits.TemporaryLimit> unmodifiedTemporaryLimits,
            List<ReportNode> temporaryLimitsReports) {
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
        if (limitToModify == null && OlgUtils.mayCreateALimit(limit.getModificationType())) {
            OlgUtils.createTemporaryLimit(limitsAdder, limit, temporaryLimitsReports, limitDurationToReport, limitValueToReport, limitValue, limitAcceptableDuration);
        } else if (limitToModify != null) {
            // the limit already exists
            if (limit.getModificationType() == TemporaryLimitModificationType.DELETE) {
                // the limit has been removed previously
                temporaryLimitsReports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.temporaryLimitDeleted.name")
                        .withUntypedValue(NAME, limit.getName())
                        .withUntypedValue(OlgUtils.DURATION, limitDurationToReport)
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build());
            } else {
                OlgUtils.modifyTemporaryLimit(limitsAdder, limit, temporaryLimitsReports, limitToModify, limitValue, limitDurationToReport, limitValueToReport, limitAcceptableDuration);
            }
        } else if (limit.getModificationType() == TemporaryLimitModificationType.MODIFY || limit.getModificationType() == TemporaryLimitModificationType.MODIFY_OR_ADD) {
            // invalid modification
            temporaryLimitsReports.add(ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.temporaryLimitsNoMatch")
                    .withUntypedValue(OlgUtils.LIMIT_ACCEPTABLE_DURATION, limitAcceptableDuration)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
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
}
