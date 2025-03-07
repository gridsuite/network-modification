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
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import com.powsybl.iidm.network.extensions.MeasurementsAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.BranchModificationInfos;
import org.gridsuite.modification.dto.CurrentLimitsModificationInfos;
import org.gridsuite.modification.dto.CurrentTemporaryLimitModificationInfos;
import org.gridsuite.modification.dto.TemporaryLimitModificationType;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.NetworkModificationException.Type.BRANCH_MODIFICATION_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public abstract class AbstractBranchModification extends AbstractModification {

    private static final String DURATION = "duration";
    private static final String NAME = "name";
    private static final String VALUE = "value";
    private static final String VALIDITY = "validity";
    protected final BranchModificationInfos modificationInfos;

    protected AbstractBranchModification(BranchModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    protected void modifyBranch(Branch<?> branch, BranchModificationInfos branchModificationInfos, ReportNode subReportNode, String reporterKey, String reporterDefaultMessage) {
        subReportNode.newReportNode()
                .withMessageTemplate(reporterKey, reporterDefaultMessage)
                .withUntypedValue("id", branchModificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        if (branchModificationInfos.getEquipmentName() != null) {
            insertReportNode(subReportNode, ModificationUtils.getInstance().buildModificationReport(Optional.of(branch.getOptionalName()).orElse(null), branchModificationInfos.getEquipmentName().getValue(), "Name", 0));
            branch.setName(branchModificationInfos.getEquipmentName().getValue());
        }

        modifyBranchConnectivityAttributes(branchModificationInfos, branch, subReportNode);

        if (characteristicsModified(branchModificationInfos)) {
            modifyCharacteristics(branch, branchModificationInfos, subReportNode);
        }

        CurrentLimitsModificationInfos currentLimitsInfos1 = modificationInfos.getCurrentLimits1();
        CurrentLimitsModificationInfos currentLimitsInfos2 = modificationInfos.getCurrentLimits2();
        List<ReportNode> side1LimitsReports = new ArrayList<>();
        CurrentLimits currentLimits1 = branch.getCurrentLimits1().orElse(null);
        if (currentLimitsInfos1 != null) {
            modifyCurrentLimits(currentLimitsInfos1, branch.newCurrentLimits1(), currentLimits1, side1LimitsReports);
        }
        List<ReportNode> side2LimitsReports = new ArrayList<>();
        CurrentLimits currentLimits2 = branch.getCurrentLimits2().orElse(null);
        if (currentLimitsInfos2 != null) {
            modifyCurrentLimits(currentLimitsInfos2, branch.newCurrentLimits2(), currentLimits2, side2LimitsReports);
        }
        if (!side1LimitsReports.isEmpty() || !side2LimitsReports.isEmpty()) {
            ReportNode limitsReportNode = subReportNode.newReportNode().withMessageTemplate("limits", "Limits").add();
            ModificationUtils.getInstance().reportModifications(limitsReportNode, side1LimitsReports, "side1LimitsModification",
                    "    Side 1");
            ModificationUtils.getInstance().reportModifications(limitsReportNode, side2LimitsReports, "side2LimitsModification",
                    "    Side 2");
        }

        updateConnections(branch, branchModificationInfos);
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
            estimSubReportNode = subReportNode.newReportNode().withMessageTemplate("StateEstimationData", "State estimation").add();
        }
        if (!side1Reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(estimSubReportNode, side1Reports, "measurementsSide1", "    Side 1");
        }
        if (!side2Reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(estimSubReportNode, side2Reports, "measurementsSide2", "    Side 2");
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
                reports.add(ModificationUtils.buildModificationReport(oldValue, value, measurementType + VALUE, 1, TypedValue.INFO_SEVERITY));
            }
            if (validity != null) {
                boolean oldValidity = m.isValid();
                m.setValid(validity);
                reports.add(ModificationUtils.buildModificationReport(oldValidity, validity, measurementType + VALIDITY, 1, TypedValue.INFO_SEVERITY));
            }
        } else { // add new measurement
            var mAdder = measurements.newMeasurement().setId(UUID.randomUUID().toString()).setType(type).setSide(side);
            if (value != null) {
                mAdder.setValue(value);
                reports.add(ModificationUtils.buildModificationReport(null, value, measurementType + VALUE, 1, TypedValue.INFO_SEVERITY));
            }
            if (validity != null) {
                mAdder.setValid(validity);
                reports.add(ModificationUtils.buildModificationReport(null, validity, measurementType + VALIDITY, 1, TypedValue.INFO_SEVERITY));
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

    protected void modifyCurrentLimits(CurrentLimitsModificationInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder, CurrentLimits currentLimits, List<ReportNode> limitsReports) {
        boolean hasPermanent = currentLimitsInfos.getPermanentLimit() != null;
        if (hasPermanent) {
            limitsReports.add(ModificationUtils.getInstance().buildModificationReport(currentLimits != null ? currentLimits.getPermanentLimit() : Double.NaN,
                    currentLimitsInfos.getPermanentLimit(), "IST", 2));
            limitsAdder.setPermanentLimit(currentLimitsInfos.getPermanentLimit());
        } else {
            if (currentLimits != null) {
                limitsAdder.setPermanentLimit(currentLimits.getPermanentLimit());
            }
        }
        modifyTemporaryLimits(currentLimitsInfos, limitsAdder, currentLimits, limitsReports);
        limitsAdder.add();
    }

    protected void modifyTemporaryLimits(CurrentLimitsModificationInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder,
                                         CurrentLimits currentLimits, List<ReportNode> limitsReports) {
        // we create a mutable list of temporary limits to be able to remove the limits that are modified in current modification
        List<LoadingLimits.TemporaryLimit> branchTemporaryLimits = new ArrayList<>();
        if (currentLimits != null) {
            branchTemporaryLimits.addAll(currentLimits.getTemporaryLimits());
        }
        List<ReportNode> temporaryLimitsReports = new ArrayList<>();
        for (CurrentTemporaryLimitModificationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
            int limitAcceptableDuration = limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration();
            double limitValue = limit.getValue() == null ? Double.MAX_VALUE : limit.getValue();
            String limitDurationToReport = limitAcceptableDuration == Integer.MAX_VALUE ? " " : String.valueOf(limitAcceptableDuration);
            String limitValueToReport = limitValue == Double.MAX_VALUE ? "no value" : String.valueOf(limitValue);
            LoadingLimits.TemporaryLimit limitToModify = null;
            if (currentLimits != null) {
                limitToModify = currentLimits.getTemporaryLimit(limitAcceptableDuration);
                if (limitToModify != null && !limitToModify.getName().equals(limit.getName())) {
                    throw new PowsyblException("2temporary limits have the same duration " + limitAcceptableDuration);
                }
                // we remove the limit to modify from the list of temporary limits so we can get the list of temporary limits coming from previous modifications
                branchTemporaryLimits.removeIf(temporaryLimit -> temporaryLimit.getAcceptableDuration() == limitAcceptableDuration);
            }
            if (limitToModify == null && limit.getModificationType() == TemporaryLimitModificationType.ADDED) {
                temporaryLimitsReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("temporaryLimitAdded" + limit.getName(), "            ${name} (${duration}) added with ${value}")
                        .withUntypedValue(NAME, limit.getName())
                        .withUntypedValue(DURATION, limitDurationToReport)
                        .withUntypedValue(VALUE, limitValueToReport)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());

            } else if (limitToModify != null) {
                if (limit.getModificationType() == TemporaryLimitModificationType.DELETED) {
                    temporaryLimitsReports.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("temporaryLimitDeleted" + limit.getName(), "            ${name} (${duration}) deleted")
                            .withUntypedValue(NAME, limit.getName())
                            .withUntypedValue(DURATION, limitDurationToReport)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                    continue;
                } else if (Double.compare(limitToModify.getValue(), limitValue) != 0 && limit.getModificationType() != null) {
                    temporaryLimitsReports.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("temporaryLimitModified" + limit.getName(), "            ${name} (${duration}) : ${oldValue} -> ${value}")
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
                    .withMessageTemplate("temporaryLimitsModification", "            Temporary current limits :")
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
}
