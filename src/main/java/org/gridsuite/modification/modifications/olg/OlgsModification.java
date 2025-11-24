/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.olg;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.OlgUtils;

import java.util.*;

import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.*;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.EQUIPMENT;
import static org.gridsuite.modification.dto.OperationalLimitsGroupModificationType.DELETE;

/**
 * handles the modification of a list of operational limits groups from AbstractBranchModification
 *
 * @author Mathieu DEHARBE <mathieu.deharbe at rte-france.com>
 */
public class OlgsModification {
    private final Branch<?> modifiedBranch; // branch modified by the network modification
    private final List<OperationalLimitsGroupModificationInfos> olgModificationInfos;
    private final ReportNode olgsReportNode;

    public OlgsModification(
            Branch<?> branch,
            List<OperationalLimitsGroupModificationInfos> operationalLimitsInfos,
            ReportNode limitSetsReportNode) {
        modifiedBranch = branch;
        olgModificationInfos = operationalLimitsInfos != null ? operationalLimitsInfos : new ArrayList<>();
        olgsReportNode = limitSetsReportNode;
    }

    public void modifyOperationalLimitsGroups(OperationalLimitsGroupsModificationType operationalLimitsGroupsModificationType) {

        if (operationalLimitsGroupsModificationType == OperationalLimitsGroupsModificationType.REPLACE) {
            // because we are replacing all the limit sets we remove all the limit sets that are not specified in the network modification
            // the others may be modified instead of recreated so it is better to not delete them in order to have more precise logs
            deleteOlgsUnspecifiedInTheModification(SIDE1);
            deleteOlgsUnspecifiedInTheModification(SIDE2);
        }

        for (OperationalLimitsGroupModificationInfos opLGModifInfos : olgModificationInfos) {
            if (opLGModifInfos.getModificationType() == null) {
                continue;
            }

            ArrayList<ReportNode> olgSetReports = new ArrayList<>();

            if (!opLGModifInfos.getModificationType().equals(DELETE)) {
                detectApplicabilityChange(opLGModifInfos, olgSetReports);
            }

            new OlgModification(
                    modifiedBranch,
                    opLGModifInfos,
                    olgsReportNode
            ).applyModificationToOperationalLimitsGroup();
        }
    }

    private void deleteOlgsUnspecifiedInTheModification(OperationalLimitsGroupInfos.Applicability applicability) {
        List<String> olgToBeDeleted = new ArrayList<>();
        Collection<OperationalLimitsGroup> operationalLimitsGroups = applicability == SIDE1 ?
                modifiedBranch.getOperationalLimitsGroups1() :
                modifiedBranch.getOperationalLimitsGroups2();
        operationalLimitsGroups.stream().filter(
                operationalLimitsGroup ->
                        olgModificationInfos.stream().noneMatch(
                                operationalLimitsGroupModificationInfos ->
                                        // we don't want to remove the limit sets specified in the network modification (operationalLimitsGroups) :
                                        Objects.equals(operationalLimitsGroupModificationInfos.getId(), operationalLimitsGroup.getId())
                                                && (operationalLimitsGroupModificationInfos.getApplicability() == applicability
                                                || operationalLimitsGroupModificationInfos.getApplicability() == EQUIPMENT)
                        )
        ).forEach(operationalLimitsGroup -> olgToBeDeleted.add(operationalLimitsGroup.getId()));

        Iterator<String> i = olgToBeDeleted.iterator();
        while (i.hasNext()) {
            String s = i.next();
            new OlgModification(
                    modifiedBranch,
                    OperationalLimitsGroupModificationInfos.builder()
                            .id(s)
                            .applicability(applicability)
                            .build(),
                    olgsReportNode
            ).removeOlg();
        }
    }

    private void logApplicabilityChange(List<ReportNode> olgReports, String groupId, OperationalLimitsGroupInfos.Applicability applicability) {
        olgReports.add(ReportNode.newRootReportNode().withMessageTemplate("network.modification.applicabilityChanged")
                .withUntypedValue(OlgUtils.OPERATIONAL_LIMITS_GROUP_NAME, groupId)
                .withUntypedValue(OlgUtils.APPLICABILITY, applicability.toString())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private boolean shouldDeletedOtherSide(Branch<?> branch, OperationalLimitsGroupModificationInfos limitsModifInfos) {
        boolean hasModificationOnSideOne = !olgModificationInfos.stream().filter(opLimitModifInfo ->
                        opLimitModifInfo.getId().equals(limitsModifInfos.getId()) && opLimitModifInfo.getApplicability().equals(SIDE1))
                .toList().isEmpty();

        boolean hasModificationOnSideTwo = !olgModificationInfos.stream().filter(opLimitModifInfo ->
                        opLimitModifInfo.getId().equals(limitsModifInfos.getId()) && opLimitModifInfo.getApplicability().equals(SIDE2))
                .toList().isEmpty();

        switch (limitsModifInfos.getApplicability()) {
            case SIDE1 -> {
                return !hasModificationOnSideTwo && branch.getOperationalLimitsGroup2(limitsModifInfos.getId()).isPresent();
            }
            case SIDE2 -> {
                return !hasModificationOnSideOne && branch.getOperationalLimitsGroup1(limitsModifInfos.getId()).isPresent();
            }
            default -> {
                return false;
            }
        }
    }

    // If we are changing applicability we may not find operational limits group where we should so check both sides
    private void detectApplicabilityChange(OperationalLimitsGroupModificationInfos modifiedLimitSetInfos, List<ReportNode> olgReports) {

        OperationalLimitsGroup limitsGroup1 = modifiedBranch.getOperationalLimitsGroup1(modifiedLimitSetInfos.getId()).orElse(null);
        OperationalLimitsGroup limitsGroup2 = modifiedBranch.getOperationalLimitsGroup2(modifiedLimitSetInfos.getId()).orElse(null);
        if (limitsGroup1 == null && modifiedLimitSetInfos.getApplicability().equals(SIDE2)
                || limitsGroup2 == null && modifiedLimitSetInfos.getApplicability().equals(SIDE1)) {
            return;
        } else if (limitsGroup1 != null && limitsGroup2 != null && !modifiedLimitSetInfos.getApplicability().equals(EQUIPMENT)) {
            // applicability change from EQUIPMENT to one side
            if (shouldDeletedOtherSide(modifiedBranch, modifiedLimitSetInfos)) {
                if (modifiedLimitSetInfos.getApplicability().equals(SIDE1)) {
                    modifiedBranch.removeOperationalLimitsGroup2(modifiedLimitSetInfos.getId());
                    logApplicabilityChange(olgReports, limitsGroup1.getId(), SIDE1);
                } else if (modifiedLimitSetInfos.getApplicability().equals(SIDE2)) {
                    modifiedBranch.removeOperationalLimitsGroup1(modifiedLimitSetInfos.getId());
                    logApplicabilityChange(olgReports, limitsGroup2.getId(), SIDE2);
                }
            }
            return;
        }

        switch (modifiedLimitSetInfos.getApplicability()) {
            case SIDE1 -> moveLimitSetToTheOtherSide(modifiedBranch, limitsGroup2, modifiedLimitSetInfos.getId(), true, olgReports);
            case SIDE2 -> moveLimitSetToTheOtherSide(modifiedBranch, limitsGroup1, modifiedLimitSetInfos.getId(), false, olgReports);
            case EQUIPMENT -> {
                boolean applicabilityChanged = false;
                if (limitsGroup1 == null && limitsGroup2 != null) {
                    limitsGroup1 = modifiedBranch.newOperationalLimitsGroup1(limitsGroup2.getId());
                    copyOperationalLimitsGroup(limitsGroup1.newCurrentLimits(), limitsGroup2);
                    applicabilityChanged = true;
                }
                if (limitsGroup2 == null && limitsGroup1 != null) {
                    limitsGroup2 = modifiedBranch.newOperationalLimitsGroup2(limitsGroup1.getId());
                    copyOperationalLimitsGroup(limitsGroup2.newCurrentLimits(), limitsGroup1);
                    applicabilityChanged = true;
                }
                if (applicabilityChanged) {
                    logApplicabilityChange(olgReports, limitsGroup1.getId(), EQUIPMENT);
                }
            }
        }
    }

    private void moveLimitSetToTheOtherSide(Branch<?> branch,
                                            OperationalLimitsGroup limitsGroupToCopy, String modifiedLimitSet,
                                            boolean isSide1,
                                            List<ReportNode> olgReports) {
        // if we have only one limit set with the same name but applicability is not good
        // we should copy existing limit set on the right side and removed it from the other side
        if (olgModificationInfos.stream().filter(limitSet -> limitSet.getId().equals(modifiedLimitSet)).toList().size() == 1) {
            // Copy operational limits group to the other side
            OperationalLimitsGroup limitsGroup = isSide1 ? branch.newOperationalLimitsGroup1(limitsGroupToCopy.getId())
                    : branch.newOperationalLimitsGroup2(limitsGroupToCopy.getId());
            copyOperationalLimitsGroup(limitsGroup.newCurrentLimits(), limitsGroupToCopy);

            olgReports.add(ReportNode.newRootReportNode().withMessageTemplate("network.modification.applicabilityChanged")
                    .withUntypedValue(OlgUtils.OPERATIONAL_LIMITS_GROUP_NAME, limitsGroupToCopy.getId())
                    .withUntypedValue(OlgUtils.APPLICABILITY, isSide1 ? SIDE1.toString() : SIDE2.toString())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            // Remove copied operational limits group
            if (isSide1) {
                branch.removeOperationalLimitsGroup2(modifiedLimitSet);
            } else {
                branch.removeOperationalLimitsGroup1(modifiedLimitSet);
            }
        }
    }

    private void copyOperationalLimitsGroup(CurrentLimitsAdder limitsAdder, OperationalLimitsGroup opLimitGroupToCopy) {
        // Copy all limits of the other side
        opLimitGroupToCopy.getCurrentLimits().ifPresent(currentLimits -> {
            limitsAdder.setPermanentLimit(currentLimits.getPermanentLimit());

            for (LoadingLimits.TemporaryLimit tempLimit : currentLimits.getTemporaryLimits()) {
                OlgUtils.addTemporaryLimit(limitsAdder, tempLimit.getName(), tempLimit.getValue(), tempLimit.getAcceptableDuration());
            }
            limitsAdder.add();
        });
    }
}
