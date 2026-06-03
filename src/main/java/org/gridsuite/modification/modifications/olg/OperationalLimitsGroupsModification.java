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
import jakarta.validation.constraints.NotNull;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.model.OperationalLimitsGroupModel;
import org.gridsuite.modification.model.OperationalLimitsGroupModificationModel;
import org.gridsuite.modification.model.constants.OperationalLimitsGroupsModificationType;

import java.util.*;
import java.util.stream.Stream;

import static org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability.*;
import static org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability.EQUIPMENT;
import static org.gridsuite.modification.model.constants.OperationalLimitsGroupModificationType.DELETE;
import static org.gridsuite.modification.modifications.AbstractBranchModification.*;

/**
 * handles the modification of a list of operational limits groups from AbstractBranchModification
 *
 * @author Mathieu DEHARBE <mathieu.deharbe at rte-france.com>
 */
public class OperationalLimitsGroupsModification {
    private final Branch<?> modifiedBranch; // branch modified by the network modification
    private final List<OperationalLimitsGroupModificationModel> olgModificationModel;
    private final ReportNode olgsReportNode;

    public OperationalLimitsGroupsModification(
            Branch<?> branch,
            List<OperationalLimitsGroupModificationModel> operationalLimitsModel,
            ReportNode limitSetsReportNode) {
        modifiedBranch = branch;
        olgModificationModel = operationalLimitsModel != null ? operationalLimitsModel : new ArrayList<>();
        olgsReportNode = limitSetsReportNode;
    }

    public void modifyOperationalLimitsGroups(OperationalLimitsGroupsModificationType operationalLimitsGroupsModificationType) {

        if (operationalLimitsGroupsModificationType == OperationalLimitsGroupsModificationType.REPLACE) {
            // because we are replacing all the limit sets we remove all the limit sets that are not specified in the network modification
            // the others may be modified instead of recreated so it is better to not delete them in order to have more precise logs
            deleteOlgsUnspecifiedInTheModification();
        }

        for (OperationalLimitsGroupModificationModel opLGModifModel : olgModificationModel) {
            if (opLGModifModel.getModificationType() == null) {
                continue;
            }

            ArrayList<ReportNode> olgSetReports = new ArrayList<>();

            if (!opLGModifModel.getModificationType().equals(DELETE)) {
                detectApplicabilityChange(opLGModifModel, olgSetReports);
            }

            new OperationalLimitsGroupModification(
                    modifiedBranch,
                    opLGModifModel,
                    olgsReportNode
            ).applyModificationToOperationalLimitsGroup();
        }
    }

    private void deleteOlgsUnspecifiedInTheModification() {
        Map<String, OperationalLimitsGroupModel.Applicability> olgsToBeDeleted = new HashMap<>();

        // get the deletions on side 1
        getDeletableOperationalLimitsGroupStream(modifiedBranch.getOperationalLimitsGroups1(), SIDE1)
                .forEach(operationalLimitsGroup -> olgsToBeDeleted.put(operationalLimitsGroup.getId(), SIDE1));

        // then get the deletions on side 2. And if there is already a deletion on side 1, change it to EQUIPMENT
        getDeletableOperationalLimitsGroupStream(modifiedBranch.getOperationalLimitsGroups2(), SIDE2)
                .forEach(operationalLimitsGroup -> {
                    if (olgsToBeDeleted.containsKey(operationalLimitsGroup.getId())) {
                        olgsToBeDeleted.put(operationalLimitsGroup.getId(), EQUIPMENT);
                    } else {
                        olgsToBeDeleted.put(operationalLimitsGroup.getId(), SIDE2);
                    }
                });

        for (Map.Entry<String, OperationalLimitsGroupModel.Applicability> deletedOlg : olgsToBeDeleted.entrySet()) {
            new OperationalLimitsGroupModification(
                    modifiedBranch,
                    OperationalLimitsGroupModificationModel.builder()
                            .id(deletedOlg.getKey())
                            .applicability(deletedOlg.getValue())
                            .build(),
                    olgsReportNode
            ).removeOlg();
        }
    }

    @NotNull
    private Stream<OperationalLimitsGroup> getDeletableOperationalLimitsGroupStream(Collection<OperationalLimitsGroup> modifiedOlgs, OperationalLimitsGroupModel.Applicability side) {
        return modifiedOlgs.stream().filter(
                operationalLimitsGroup ->
                        olgModificationModel.stream().noneMatch(
                                operationalLimitsGroupModificationModel ->
                                        // we don't want to remove the limit sets specified in the network modification (operationalLimitsGroups) :
                                        Objects.equals(operationalLimitsGroupModificationModel.getId(), operationalLimitsGroup.getId())
                                                && (operationalLimitsGroupModificationModel.getApplicability() == side
                                                || operationalLimitsGroupModificationModel.getApplicability() == EQUIPMENT)
                        )
        );
    }

    private void logApplicabilityChange(List<ReportNode> olgReports, String groupId, OperationalLimitsGroupModel.Applicability applicability) {
        olgReports.add(ReportNode.newRootReportNode().withMessageTemplate("network.modification.applicabilityChanged")
                .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, groupId)
                .withUntypedValue(APPLICABILITY, applicability.toString())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private boolean shouldDeletedOtherSide(Branch<?> branch, OperationalLimitsGroupModificationModel limitsModifModel) {
        boolean hasModificationOnSideOne = !olgModificationModel.stream().filter(opLimitModifInfo ->
                        opLimitModifInfo.getId().equals(limitsModifModel.getId()) && opLimitModifInfo.getApplicability().equals(SIDE1))
                .toList().isEmpty();

        boolean hasModificationOnSideTwo = !olgModificationModel.stream().filter(opLimitModifInfo ->
                        opLimitModifInfo.getId().equals(limitsModifModel.getId()) && opLimitModifInfo.getApplicability().equals(SIDE2))
                .toList().isEmpty();

        switch (limitsModifModel.getApplicability()) {
            case SIDE1 -> {
                return !hasModificationOnSideTwo && branch.getOperationalLimitsGroup2(limitsModifModel.getId()).isPresent();
            }
            case SIDE2 -> {
                return !hasModificationOnSideOne && branch.getOperationalLimitsGroup1(limitsModifModel.getId()).isPresent();
            }
            default -> {
                return false;
            }
        }
    }

    // If we are changing applicability we may not find operational limits group where we should so check both sides
    private void detectApplicabilityChange(OperationalLimitsGroupModificationModel modifiedLimitSetModel, List<ReportNode> olgReports) {

        OperationalLimitsGroup limitsGroup1 = modifiedBranch.getOperationalLimitsGroup1(modifiedLimitSetModel.getId()).orElse(null);
        OperationalLimitsGroup limitsGroup2 = modifiedBranch.getOperationalLimitsGroup2(modifiedLimitSetModel.getId()).orElse(null);
        if (limitsGroup1 == null && modifiedLimitSetModel.getApplicability().equals(SIDE2)
                || limitsGroup2 == null && modifiedLimitSetModel.getApplicability().equals(SIDE1)) {
            return;
        } else if (limitsGroup1 != null && limitsGroup2 != null && !modifiedLimitSetModel.getApplicability().equals(EQUIPMENT)) {
            // applicability change from EQUIPMENT to one side
            if (shouldDeletedOtherSide(modifiedBranch, modifiedLimitSetModel)) {
                if (modifiedLimitSetModel.getApplicability().equals(SIDE1)) {
                    modifiedBranch.removeOperationalLimitsGroup2(modifiedLimitSetModel.getId());
                    logApplicabilityChange(olgReports, limitsGroup1.getId(), SIDE1);
                } else if (modifiedLimitSetModel.getApplicability().equals(SIDE2)) {
                    modifiedBranch.removeOperationalLimitsGroup1(modifiedLimitSetModel.getId());
                    logApplicabilityChange(olgReports, limitsGroup2.getId(), SIDE2);
                }
            }
            return;
        }

        switch (modifiedLimitSetModel.getApplicability()) {
            case SIDE1 -> moveLimitSetToTheOtherSide(modifiedBranch, limitsGroup2, modifiedLimitSetModel.getId(), true, olgReports);
            case SIDE2 -> moveLimitSetToTheOtherSide(modifiedBranch, limitsGroup1, modifiedLimitSetModel.getId(), false, olgReports);
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
        if (olgModificationModel.stream().filter(limitSet -> limitSet.getId().equals(modifiedLimitSet)).toList().size() == 1) {
            // Copy operational limits group to the other side
            OperationalLimitsGroup limitsGroup = isSide1 ? branch.newOperationalLimitsGroup1(limitsGroupToCopy.getId())
                    : branch.newOperationalLimitsGroup2(limitsGroupToCopy.getId());
            copyOperationalLimitsGroup(limitsGroup.newCurrentLimits(), limitsGroupToCopy);

            olgReports.add(ReportNode.newRootReportNode().withMessageTemplate("network.modification.applicabilityChanged")
                    .withUntypedValue(OPERATIONAL_LIMITS_GROUP_NAME, limitsGroupToCopy.getId())
                    .withUntypedValue(APPLICABILITY, isSide1 ? SIDE1.toString() : SIDE2.toString())
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
                addTemporaryLimit(limitsAdder, tempLimit.getName(), tempLimit.getValue(), tempLimit.getAcceptableDuration());
            }
            limitsAdder.add();
        });
    }
}
