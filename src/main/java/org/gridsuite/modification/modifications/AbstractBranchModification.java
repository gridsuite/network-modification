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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.AttributeModification;
import org.gridsuite.modification.model.BranchModificationModel;
import org.gridsuite.modification.model.OperationType;
import org.gridsuite.modification.model.TemporaryLimitModificationType;
import org.gridsuite.modification.modifications.olg.OperationalLimitsGroupsModification;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.NetworkModificationException.Type.BRANCH_MODIFICATION_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.NO_VALUE;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public abstract class AbstractBranchModification extends AbstractModification {

    public static final String DURATION = "duration";
    public static final String NAME = "name";
    public static final String VALUE = "value";
    private static final String VALIDITY = "validity";
    public static final String LIMIT_ACCEPTABLE_DURATION = "limitAcceptableDuration";
    public static final String OPERATIONAL_LIMITS_GROUP_NAME = "operationalLimitsGroupName";
    public static final String SIDE = "side";
    public static final String APPLICABILITY = "applicability";

    protected final BranchModificationModel modificationModel;

    protected AbstractBranchModification(BranchModificationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    protected void modifyBranch(Branch<?> branch, BranchModificationModel branchModificationModel, ReportNode subReportNode, String reporterKey) {
        subReportNode.newReportNode()
            .withMessageTemplate(reporterKey)
            .withUntypedValue("id", branchModificationModel.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
        if (branchModificationModel.getEquipmentName() != null && modificationModel.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(
                branch::setName,
                () -> branch.getOptionalName().orElse(NO_VALUE),
                modificationModel.getEquipmentName(),
                subReportNode,
                "Name"
            );
        }

        modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide1(modificationModel, branch, subReportNode);
        modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide2(modificationModel, branch, subReportNode);
        modifyBranchConnectivityAttributes(branchModificationModel, branch, subReportNode);

        if (characteristicsModified(branchModificationModel)) {
            modifyCharacteristics(branch, branchModificationModel, subReportNode);
        }

        List<ReportNode> activeOLGReports = new ArrayList<>();

        ReportNode limitsReportNode = null;
        boolean modifyOLG = branchModificationModel.getEnableOLGModification() == null
            || branchModificationModel.getEnableOLGModification();

        if (modifyOLG) {
            limitsReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
            ReportNode limitSetsReportNode = limitsReportNode.newReportNode().withMessageTemplate("network.modification.limitsSets").add();
            new OperationalLimitsGroupsModification(
                branch,
                branchModificationModel.getOperationalLimitsGroups(),
                limitSetsReportNode
            ).modifyOperationalLimitsGroups(branchModificationModel.getOperationalLimitsGroupsModificationType());
        }

        applySelectedOLGs(branch, activeOLGReports);

        if (!activeOLGReports.isEmpty()) {
            if (limitsReportNode == null) {
                limitsReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
            }
            ModificationUtils.getInstance().reportModifications(limitsReportNode, activeOLGReports, "network.modification.activeLimitsSets");
        }
        updateConnections(branch, branchModificationModel);
    }

    private void applySelectedOLGs(Branch<?> branch, List<ReportNode> activeOLGReports) {
        if (modificationModel.getSelectedOperationalLimitsGroupId1() != null) {
            modifySelectedOperationalLimitsGroup(
                branch,
                modificationModel.getSelectedOperationalLimitsGroupId1(),
                TwoSides.ONE,
                activeOLGReports
            );
        }
        if (modificationModel.getSelectedOperationalLimitsGroupId2() != null) {
            modifySelectedOperationalLimitsGroup(
                branch,
                modificationModel.getSelectedOperationalLimitsGroupId2(),
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

    public ReportNode updateMeasurements(Branch<?> branch, BranchModificationModel branchModificationModel, ReportNode subReportNode) {
        Double p1Value = branchModificationModel.getP1MeasurementValue() != null ? branchModificationModel.getP1MeasurementValue().getValue() : null;
        Double q1Value = branchModificationModel.getQ1MeasurementValue() != null ? branchModificationModel.getQ1MeasurementValue().getValue() : null;
        Double p2Value = branchModificationModel.getP2MeasurementValue() != null ? branchModificationModel.getP2MeasurementValue().getValue() : null;
        Double q2Value = branchModificationModel.getQ2MeasurementValue() != null ? branchModificationModel.getQ2MeasurementValue().getValue() : null;
        Boolean p1Validity = branchModificationModel.getP1MeasurementValidity() != null ? branchModificationModel.getP1MeasurementValidity().getValue() : null;
        Boolean q1Validity = branchModificationModel.getQ1MeasurementValidity() != null ? branchModificationModel.getQ1MeasurementValidity().getValue() : null;
        Boolean p2Validity = branchModificationModel.getP2MeasurementValidity() != null ? branchModificationModel.getP2MeasurementValidity().getValue() : null;
        Boolean q2Validity = branchModificationModel.getQ2MeasurementValidity() != null ? branchModificationModel.getQ2MeasurementValidity().getValue() : null;
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

    private void upsertMeasurement(Measurements<?> measurements, Measurement.Type type, ThreeSides side, Double value, Boolean requestedValidity, List<ReportNode> reports) {
        if (value == null && requestedValidity == null) {
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
            if (requestedValidity != null) {
                boolean oldValidity = m.isValid();

                ModificationUtils.updateMeasurementValidity(m, requestedValidity);
                reports.add(ModificationUtils.buildModificationReport(oldValidity, requestedValidity, measurementType + VALIDITY, TypedValue.INFO_SEVERITY));
            }
        } else { // add new measurement
            var mAdder = measurements.newMeasurement().setId(UUID.randomUUID().toString()).setType(type).setSide(side);
            if (value != null) {
                mAdder.setValue(value);
                reports.add(ModificationUtils.buildModificationReport(null, value, measurementType + VALUE, TypedValue.INFO_SEVERITY));
            }
            if (requestedValidity != null) {
                mAdder.setValid(requestedValidity);
                reports.add(ModificationUtils.buildModificationReport(null, requestedValidity, measurementType + VALIDITY, TypedValue.INFO_SEVERITY));
            }
            mAdder.add();
        }
    }

    private Measurement getExistingMeasurement(Measurements<?> measurements, Measurement.Type type, ThreeSides side) {
        return measurements.getMeasurements(type).stream().filter(m -> m.getSide() == side).findFirst().orElse(null);
    }

    private void updateConnections(Branch<?> branch, BranchModificationModel branchModificationModel) {
        List<TwoSides> errorSides = new ArrayList<>();
        List<String> errorTypes = new ArrayList<>();
        if (branchModificationModel.getTerminal1Connected() != null && !updateConnection(branch, TwoSides.ONE, modificationModel.getTerminal1Connected().getValue())) {
            errorSides.add(TwoSides.ONE);
            errorTypes.add(Boolean.TRUE.equals(modificationModel.getTerminal1Connected().getValue()) ? "connect" : "disconnect");
        }
        if (branchModificationModel.getTerminal2Connected() != null && !updateConnection(branch, TwoSides.TWO, modificationModel.getTerminal2Connected().getValue())) {
            errorSides.add(TwoSides.TWO);
            errorTypes.add(Boolean.TRUE.equals(modificationModel.getTerminal2Connected().getValue()) ? "connect" : "disconnect");
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

    protected boolean characteristicsModified(BranchModificationModel branchModificationModel) {
        return branchModificationModel.getX() != null
            && branchModificationModel.getX().getValue() != null
            || branchModificationModel.getR() != null
            && branchModificationModel.getR().getValue() != null;
    }

    protected abstract void modifyCharacteristics(Branch<?> branch, BranchModificationModel branchModificationModel,
                                                  ReportNode subReportNode);

    private ReportNode modifyBranchConnectivityAttributes(BranchModificationModel branchModificationModel,
                                                          Branch<?> branch, ReportNode subReportNode) {
        ConnectablePosition<?> connectablePosition = (ConnectablePosition<?>) branch.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<?> connectablePositionAdder = branch.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyBranchConnectivityAttributes(connectablePosition, connectablePositionAdder, branch, branchModificationModel, subReportNode);
    }

    private void modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide1(BranchModificationModel modificationModel,
                                                                           Branch<?> branch, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
            (Connectable<?>) branch, branch.getTerminal1(),
            modificationModel.getVoltageLevelId1(),
            modificationModel.getBusOrBusbarSectionId1(),
            subReportNode
        );
    }

    private void modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide2(BranchModificationModel modificationModel,
                                                                           Branch<?> branch, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
            (Connectable<?>) branch, branch.getTerminal2(),
            modificationModel.getVoltageLevelId2(),
            modificationModel.getBusOrBusbarSectionId2(),
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
