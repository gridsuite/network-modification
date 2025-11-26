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
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.olg.OlgsModification;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.StringUtils;

import java.util.*;
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
        if (branchModificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(
                    branch::setName,
                    () -> branch.getOptionalName().orElse(NO_VALUE),
                    modificationInfos.getEquipmentName(),
                    subReportNode,
                    "Name"
            );
        }

        modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide1(modificationInfos, branch, subReportNode);
        modifyBranchVoltageLevelBusOrBusBarSectionAttributesSide2(modificationInfos, branch, subReportNode);
        modifyBranchConnectivityAttributes(branchModificationInfos, branch, subReportNode);

        if (characteristicsModified(branchModificationInfos)) {
            modifyCharacteristics(branch, branchModificationInfos, subReportNode);
        }

        List<ReportNode> activeOLGReports = new ArrayList<>();

        ReportNode limitsReportNode = null;
        boolean modifyOLG = branchModificationInfos.getEnableOLGModification() == null
            || branchModificationInfos.getEnableOLGModification();

        if (modifyOLG) {
            limitsReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
            ReportNode limitSetsReportNode = limitsReportNode.newReportNode().withMessageTemplate("network.modification.limitsSets").add();
            new OlgsModification(
                    branch,
                    branchModificationInfos.getOperationalLimitsGroups(),
                    limitSetsReportNode
            ).modifyOperationalLimitsGroups(branchModificationInfos.getOperationalLimitsGroupsModificationType());
        }

        applySelectedOLGs(branch, activeOLGReports);

        if (!activeOLGReports.isEmpty()) {
            if (limitsReportNode == null) {
                limitsReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.limits").add();
            }
            ModificationUtils.getInstance().reportModifications(limitsReportNode, activeOLGReports, "network.modification.activeLimitsSets");
        }
        updateConnections(branch, branchModificationInfos);
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
