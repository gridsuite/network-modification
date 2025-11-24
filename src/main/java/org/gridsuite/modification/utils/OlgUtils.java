package org.gridsuite.modification.utils;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.CurrentLimitsAdder;
import com.powsybl.iidm.network.TwoSides;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.dto.TemporaryLimitModificationType;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Objects;

public final class OlgUtils {
    public static final String DURATION = "duration";
    public static final String LIMIT_ACCEPTABLE_DURATION = "limitAcceptableDuration";
    public static final String OPERATIONAL_LIMITS_GROUP_NAME = "operationalLimitsGroupName";
    public static final String SIDE = "side";
    public static final String APPLICABILITY = "applicability";

    private OlgUtils() {
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

    public static boolean mayCreateALimit(TemporaryLimitModificationType modificationType) {
        return modificationType == TemporaryLimitModificationType.ADD
                || modificationType == TemporaryLimitModificationType.REPLACE
                || modificationType == TemporaryLimitModificationType.MODIFY_OR_ADD;
    }

    public static void addTemporaryLimit(CurrentLimitsAdder limitsAdder, String limit, double limitValue, int limitAcceptableDuration) {
        limitsAdder
                .beginTemporaryLimit()
                .setName(limit)
                .setValue(limitValue)
                .setAcceptableDuration(limitAcceptableDuration)
                .endTemporaryLimit();
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
