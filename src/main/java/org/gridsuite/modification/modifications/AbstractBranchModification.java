/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.olg.OperationalLimitsGroupsModification;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.NetworkModificationException.Type.BRANCH_MODIFICATION_ERROR;
import static org.gridsuite.modification.utils.MeasurementUtils.upsertSideMeasurement;
import static org.gridsuite.modification.utils.ModificationUtils.NO_VALUE;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@NoArgsConstructor
@Getter
public abstract class AbstractBranchModification extends AbstractModification {

    public static final String DURATION = "duration";
    public static final String NAME = "name";
    public static final String VALUE = "value";
    public static final String LIMIT_ACCEPTABLE_DURATION = "limitAcceptableDuration";
    public static final String OPERATIONAL_LIMITS_GROUP_NAME = "operationalLimitsGroupName";
    public static final String SIDE = "side";
    public static final String APPLICABILITY = "applicability";

    protected String equipmentId;
    protected List<FreePropertyInfos> properties;
    protected AttributeModification<String> equipmentName;
    protected AttributeModification<Double> r;
    protected AttributeModification<Double> x;
    protected OperationalLimitsGroupsModificationType operationalLimitsGroupsModificationType;
    protected Boolean enableOLGModification;
    protected List<OperationalLimitsGroupModificationInfos> operationalLimitsGroups;
    protected AttributeModification<String> selectedOperationalLimitsGroupId1;
    protected AttributeModification<String> selectedOperationalLimitsGroupId2;
    protected AttributeModification<String> voltageLevelId1;
    protected AttributeModification<String> voltageLevelId2;
    protected AttributeModification<String> busOrBusbarSectionId1;
    protected AttributeModification<String> busOrBusbarSectionId2;
    protected AttributeModification<String> connectionName1;
    protected AttributeModification<String> connectionName2;
    protected AttributeModification<ConnectablePosition.Direction> connectionDirection1;
    protected AttributeModification<ConnectablePosition.Direction> connectionDirection2;
    protected AttributeModification<Integer> connectionPosition1;
    protected AttributeModification<Integer> connectionPosition2;
    protected AttributeModification<Boolean> terminal1Connected;
    protected AttributeModification<Boolean> terminal2Connected;
    protected AttributeModification<Double> p1MeasurementValue;
    protected AttributeModification<Boolean> p1MeasurementValidity;
    protected AttributeModification<Double> p2MeasurementValue;
    protected AttributeModification<Boolean> p2MeasurementValidity;
    protected AttributeModification<Double> q1MeasurementValue;
    protected AttributeModification<Boolean> q1MeasurementValidity;
    protected AttributeModification<Double> q2MeasurementValue;
    protected AttributeModification<Boolean> q2MeasurementValidity;

    protected void modifyBranch(Branch<?> branch, ReportNode subReportNode, String reporterKey) {
        subReportNode.newReportNode()
                .withMessageTemplate(reporterKey)
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        if (equipmentName != null && equipmentName.getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(
                    branch::setName,
                    () -> branch.getOptionalName().orElse(NO_VALUE),
                    equipmentName,
                    subReportNode,
                    "Name"
            );
        }

        modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide1(branch, subReportNode);
        modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide2(branch, subReportNode);
        modifyBranchConnectivityAttributes(branch, subReportNode);

        if (characteristicsModified()) {
            modifyCharacteristics(branch, subReportNode);
        }

        List<ReportNode> activeOLGReports = new ArrayList<>();

        ReportNode limitsReportNode = null;
        boolean modifyOLG = enableOLGModification == null || enableOLGModification;

        if (modifyOLG) {
            limitsReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
            ReportNode limitSetsReportNode = limitsReportNode.newReportNode().withMessageTemplate("network.modification.limitsSets").add();
            new OperationalLimitsGroupsModification(
                    branch,
                    operationalLimitsGroups,
                    limitSetsReportNode
            ).modifyOperationalLimitsGroups(operationalLimitsGroupsModificationType);
        }

        applySelectedOLGs(branch, activeOLGReports);

        if (!activeOLGReports.isEmpty()) {
            if (limitsReportNode == null) {
                limitsReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
            }
            ModificationUtils.getInstance().reportModifications(limitsReportNode, activeOLGReports, "network.modification.activeLimitsSets");
        }
        updateConnections(branch);
    }

    private void applySelectedOLGs(Branch<?> branch, List<ReportNode> activeOLGReports) {
        if (selectedOperationalLimitsGroupId1 != null) {
            modifySelectedOperationalLimitsGroup(
                    branch,
                    selectedOperationalLimitsGroupId1,
                    TwoSides.ONE,
                    activeOLGReports
            );
        }
        if (selectedOperationalLimitsGroupId2 != null) {
            modifySelectedOperationalLimitsGroup(
                    branch,
                    selectedOperationalLimitsGroupId2,
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

    public ReportNode updateMeasurements(Branch<?> branch, ReportNode subReportNode) {
        Double p1Value = p1MeasurementValue != null ? p1MeasurementValue.getValue() : null;
        Double q1Value = q1MeasurementValue != null ? q1MeasurementValue.getValue() : null;
        Double p2Value = p2MeasurementValue != null ? p2MeasurementValue.getValue() : null;
        Double q2Value = q2MeasurementValue != null ? q2MeasurementValue.getValue() : null;
        Boolean p1Validity = p1MeasurementValidity != null ? p1MeasurementValidity.getValue() : null;
        Boolean q1Validity = q1MeasurementValidity != null ? q1MeasurementValidity.getValue() : null;
        Boolean p2Validity = p2MeasurementValidity != null ? p2MeasurementValidity.getValue() : null;
        Boolean q2Validity = q2MeasurementValidity != null ? q2MeasurementValidity.getValue() : null;
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
        upsertSideMeasurement(measurements, Measurement.Type.ACTIVE_POWER, ThreeSides.ONE, p1Value, p1Validity, side1Reports);
        upsertSideMeasurement(measurements, Measurement.Type.REACTIVE_POWER, ThreeSides.ONE, q1Value, q1Validity, side1Reports);
        // Side 2 measurements update
        List<ReportNode> side2Reports = new ArrayList<>();
        upsertSideMeasurement(measurements, Measurement.Type.ACTIVE_POWER, ThreeSides.TWO, p2Value, p2Validity, side2Reports);
        upsertSideMeasurement(measurements, Measurement.Type.REACTIVE_POWER, ThreeSides.TWO, q2Value, q2Validity, side2Reports);
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

    private void updateConnections(Branch<?> branch) {
        List<TwoSides> errorSides = new ArrayList<>();
        List<String> errorTypes = new ArrayList<>();
        if (terminal1Connected != null && !updateConnection(branch, TwoSides.ONE, terminal1Connected.getValue())) {
            errorSides.add(TwoSides.ONE);
            errorTypes.add(Boolean.TRUE.equals(terminal1Connected.getValue()) ? "connect" : "disconnect");
        }
        if (terminal2Connected != null && !updateConnection(branch, TwoSides.TWO, terminal2Connected.getValue())) {
            errorSides.add(TwoSides.TWO);
            errorTypes.add(Boolean.TRUE.equals(terminal2Connected.getValue()) ? "connect" : "disconnect");
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

    public static boolean mayCreateLimit(TemporaryLimitModificationType modificationType) {
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

    protected boolean characteristicsModified() {
        return x != null
                && x.getValue() != null
                || r != null
                && r.getValue() != null;
    }

    protected abstract void modifyCharacteristics(Branch<?> branch, ReportNode subReportNode);

    private ReportNode modifyBranchConnectivityAttributes(Branch<?> branch, ReportNode subReportNode) {
        ConnectablePosition<?> connectablePosition = (ConnectablePosition<?>) branch.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<?> connectablePositionAdder = branch.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyBranchConnectivityAttributes(connectablePosition, connectablePositionAdder, branch,
                equipmentId, voltageLevelId1, voltageLevelId2, busOrBusbarSectionId1, busOrBusbarSectionId2, connectionName1, connectionName2,
                connectionDirection1, connectionDirection2, connectionPosition1, connectionPosition2, terminal1Connected, terminal2Connected, subReportNode);
    }

    private void modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide1(Branch<?> branch, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                (Connectable<?>) branch, branch.getTerminal1(),
                voltageLevelId1,
                busOrBusbarSectionId1,
                subReportNode
        );
    }

    private void modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide2(Branch<?> branch, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                (Connectable<?>) branch, branch.getTerminal2(),
                voltageLevelId2,
                busOrBusbarSectionId2,
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
