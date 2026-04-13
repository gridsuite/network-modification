/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.AbstractNetworkModification;
import com.powsybl.iidm.network.*;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author Etienne LESOT <etienne.lesot at rte-france.com>
 */
public final class ModificationLimitsUtils {

    private static final String LIMITSET_NAME = "limitset_name";
    private static final String REPLACING_LINE = "replacing_line";

    private ModificationLimitsUtils() {

    }

    public static void applyRevertModificationWithMergingOfLimits(Network network,
                                                            String lineToAttachTo1Id,
                                                            String lineToAttachTo2Id,
                                                            String replacingLineId,
                                                            AbstractNetworkModification algo,
                                                            ReportNode subReportNode) {
        // to be removed if powsybl integrate it
        // need to get them before applying modification
        Line line1 = network.getLine(lineToAttachTo1Id);
        Line line2 = network.getLine(lineToAttachTo2Id);

        algo.apply(network, true, subReportNode);

        // to be removed if powsybl integrate it
        mergeOperationalLimitsGroups(network.getLine(replacingLineId),
                line1, line2,
                subReportNode);
    }

    public static void mergeOperationalLimitsGroups(Line mergedLine,
                                                    Line line1,
                                                    Line line2,
                                                    ReportNode subReportNode) {
        ReportNode mergingLimitsReportNode = subReportNode.newReportNode()
                .withMessageTemplate("network.modification.mergeLimits")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        // remove all limitsGroups as the modification does not do as wanted
        List<String> groupIds1 = mergedLine.getOperationalLimitsGroups1().stream()
                                .map(OperationalLimitsGroup::getId)
                                .toList();
        groupIds1.forEach(mergedLine::removeOperationalLimitsGroup1);

        List<String> groupIds2 = mergedLine.getOperationalLimitsGroups2().stream()
                                .map(OperationalLimitsGroup::getId)
                                .toList();
        groupIds2.forEach(mergedLine::removeOperationalLimitsGroup2);

        // side one
        createMergedOperationalLimitsGroups(line1.getOperationalLimitsGroups1(), line2.getOperationalLimitsGroups1(),
                mergedLine::newOperationalLimitsGroup1, TwoSides.ONE,
                line1.getId(), line2.getId(), mergedLine.getId(), mergingLimitsReportNode);

        // side two
        createMergedOperationalLimitsGroups(line1.getOperationalLimitsGroups2(), line2.getOperationalLimitsGroups2(),
                mergedLine::newOperationalLimitsGroup2, TwoSides.TWO,
                line1.getId(), line2.getId(), mergedLine.getId(), mergingLimitsReportNode);

        // set selected
        Optional<String> selectedOperationalLimitsGroupLine1Side1 = line1.getSelectedOperationalLimitsGroupId1();
        Optional<String> selectedOperationalLimitsGroupLine2Side1 = line2.getSelectedOperationalLimitsGroupId1();
        if (selectedOperationalLimitsGroupLine1Side1.isPresent() && selectedOperationalLimitsGroupLine2Side1.isPresent() &&
                selectedOperationalLimitsGroupLine1Side1.get().equals(selectedOperationalLimitsGroupLine2Side1.get())) {
            mergedLine.setSelectedOperationalLimitsGroup1(selectedOperationalLimitsGroupLine1Side1.get());
        }
        Optional<String> selectedOperationalLimitsGroupLine1Side2 = line1.getSelectedOperationalLimitsGroupId2();
        Optional<String> selectedOperationalLimitsGroupLine2Side2 = line2.getSelectedOperationalLimitsGroupId2();
        if (selectedOperationalLimitsGroupLine1Side2.isPresent() && selectedOperationalLimitsGroupLine2Side2.isPresent() &&
                selectedOperationalLimitsGroupLine1Side2.get().equals(selectedOperationalLimitsGroupLine2Side2.get())) {
            mergedLine.setSelectedOperationalLimitsGroup2(selectedOperationalLimitsGroupLine1Side2.get());
        }
    }

    public static void createMergedOperationalLimitsGroups(Collection<OperationalLimitsGroup> groupsOnLine1,
                                                           Collection<OperationalLimitsGroup> groupsOnLine2,
                                                           Function<String, OperationalLimitsGroup> operationalLimitsGroupConstructor,
                                                           TwoSides side,
                                                           String line1Id,
                                                           String line2Id,
                                                           String newLineId,
                                                           ReportNode reportNode) {
        Map<String, OperationalLimitsGroup> limitGroupMapOnLine1 = groupsOnLine1.stream().collect(Collectors.toMap(OperationalLimitsGroup::getId, Function.identity()));
        Map<String, OperationalLimitsGroup> limitGroupMapOnLine2 = groupsOnLine2.stream().collect(Collectors.toMap(OperationalLimitsGroup::getId, Function.identity()));
        Set<String> operationalLimitsGroupNames = new HashSet<>();
        operationalLimitsGroupNames.addAll(limitGroupMapOnLine1.keySet());
        operationalLimitsGroupNames.addAll(limitGroupMapOnLine2.keySet());
        operationalLimitsGroupNames.forEach(operationalLimitsGroupId -> {
            OperationalLimitsGroup groupOnLine1 = limitGroupMapOnLine1.get(operationalLimitsGroupId);
            OperationalLimitsGroup groupOnLine2 = limitGroupMapOnLine2.get(operationalLimitsGroupId);
            if (groupOnLine1 != null && groupOnLine2 == null) {
                logDeletedGroups(operationalLimitsGroupId, reportNode, side, line1Id, line2Id, newLineId);
            } else if (groupOnLine1 == null && groupOnLine2 != null) {
                logDeletedGroups(operationalLimitsGroupId, reportNode, side, line2Id, line1Id, newLineId);
            } else if (groupOnLine1 != null) {
                OperationalLimitsGroup newGroup = operationalLimitsGroupConstructor.apply(operationalLimitsGroupId);
                Optional<CurrentLimits> currentLimitsLine1 = groupOnLine1.getCurrentLimits();
                Optional<CurrentLimits> currentLimitsLine2 = groupOnLine2.getCurrentLimits();
                if (currentLimitsLine1.isPresent() && currentLimitsLine2.isPresent()) {
                    mergeCurrentLimits(currentLimitsLine1.get(), currentLimitsLine2.get(), newGroup.newCurrentLimits(),
                            operationalLimitsGroupId, line1Id, line2Id, newLineId, reportNode);
                }
                manageLimitsGroupProperties(newGroup, groupOnLine1, groupOnLine2, newLineId, line1Id, line2Id, reportNode);
            }
        });
    }

    private static void manageLimitsGroupProperties(OperationalLimitsGroup newGroup,
                                                    OperationalLimitsGroup groupOnLine1,
                                                    OperationalLimitsGroup groupOnLine2,
                                                    String newLineId,
                                                    String line1Id,
                                                    String line2Id,
                                                    ReportNode reportNode) {
        groupOnLine1.getPropertyNames().forEach(propertyNameLine1 -> {
            String valueLine2 = groupOnLine2.getProperty(propertyNameLine1);
            if (valueLine2 != null) {
                String valueLine1 = groupOnLine1.getProperty(propertyNameLine1);
                if (valueLine1.equals(valueLine2)) {
                    newGroup.setProperty(propertyNameLine1, valueLine1);
                } else {
                    reportNode.newReportNode().withMessageTemplate("network.modification.propertyOfLimitsGroupDeletedAfterMerge")
                            .withUntypedValue(LIMITSET_NAME, newGroup.getId())
                            .withUntypedValue("property_name", propertyNameLine1)
                            .withUntypedValue("property_value_A", valueLine1)
                            .withUntypedValue("line_A", line1Id)
                            .withUntypedValue("property_value_B", valueLine2)
                            .withUntypedValue("line_B", line2Id)
                            .withUntypedValue(REPLACING_LINE, newLineId)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .add();
                }
            } else {
                String valueLine1 = groupOnLine1.getProperty(propertyNameLine1);
                newGroup.setProperty(propertyNameLine1, valueLine1);
            }
        });
        groupOnLine2.getPropertyNames().forEach(propertyNameLine2 -> {
            if (groupOnLine1.getProperty(propertyNameLine2) == null) {
                String valueLine2 = groupOnLine2.getProperty(propertyNameLine2);
                newGroup.setProperty(propertyNameLine2, valueLine2);
            }
        });
    }

    private static void logDeletedGroups(String deletedGroup, ReportNode reportNode, TwoSides side,
                                         String lineWithLimitSet, String lineWithoutLimitSet, String newLineId) {
        reportNode.newReportNode()
                .withMessageTemplate("network.modification.limitGroupsDeletedAfterMerge")
                .withUntypedValue(LIMITSET_NAME, deletedGroup)
                .withUntypedValue("applicability", side.name())
                .withUntypedValue("line_with_limitset", lineWithLimitSet)
                .withUntypedValue("line_without_limitset", lineWithoutLimitSet)
                .withUntypedValue(REPLACING_LINE, newLineId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private static void mergeCurrentLimits(CurrentLimits currentLimitsLine1,
                                           CurrentLimits currentLimitsLine2,
                                           CurrentLimitsAdder adder,
                                           String operationalLimitsGroupId,
                                           String line1Id,
                                           String line2Id,
                                           String newLineId,
                                           ReportNode reportNode) {
        adder.setPermanentLimit(Math.min(currentLimitsLine1.getPermanentLimit(), currentLimitsLine2.getPermanentLimit()));
        Map<String, LoadingLimits.TemporaryLimit> temporaryLimitsOnLine1 = currentLimitsLine1.getTemporaryLimits()
                .stream().collect(Collectors.toMap(LoadingLimits.TemporaryLimit::getName, Function.identity()));
        Map<String, LoadingLimits.TemporaryLimit> temporaryLimitsOnLine2 = currentLimitsLine2.getTemporaryLimits()
                .stream().collect(Collectors.toMap(LoadingLimits.TemporaryLimit::getName, Function.identity()));
        Set<String> allTemporaryLimitNames = new HashSet<>();
        allTemporaryLimitNames.addAll(temporaryLimitsOnLine1.keySet());
        allTemporaryLimitNames.addAll(temporaryLimitsOnLine2.keySet());
        allTemporaryLimitNames.forEach(temporaryLimitName -> {
            LoadingLimits.TemporaryLimit temporaryLimitLine1 = temporaryLimitsOnLine1.get(temporaryLimitName);
            LoadingLimits.TemporaryLimit temporaryLimitLine2 = temporaryLimitsOnLine2.get(temporaryLimitName);
            if (temporaryLimitLine1 == null && temporaryLimitLine2 != null) {
                logDeletedTemporaryLimit(reportNode, temporaryLimitLine2, operationalLimitsGroupId, line2Id, line1Id, newLineId);
            } else if (temporaryLimitLine1 != null && temporaryLimitLine2 == null) {
                logDeletedTemporaryLimit(reportNode, temporaryLimitLine1, operationalLimitsGroupId, line1Id, line2Id, newLineId);
            } else if (temporaryLimitLine1 != null) {
                if (temporaryLimitLine1.getAcceptableDuration() != temporaryLimitLine2.getAcceptableDuration()) {
                    logDeletedTemporaryLimit(reportNode, temporaryLimitLine1, operationalLimitsGroupId, line1Id, line2Id, newLineId);
                } else {
                    adder.beginTemporaryLimit()
                            .setName(temporaryLimitLine1.getName())
                            .setAcceptableDuration(temporaryLimitLine1.getAcceptableDuration())
                            .setValue(Math.min(temporaryLimitLine1.getValue(), temporaryLimitLine2.getValue()))
                            .endTemporaryLimit();
                }
            }
        });
        adder.add();
    }

    private static void logDeletedTemporaryLimit(ReportNode reportNode, LoadingLimits.TemporaryLimit temporaryLimit,
                                                 String operationalLimitsGroupName, String lineWithTemporaryLimit, String lineWithoutTemporaryLimit, String newLineId) {
        reportNode.newReportNode()
                .withMessageTemplate("network.modification.temporaryLimitsDeletedAfterMerge")
                .withUntypedValue("limit_name", temporaryLimit.getName())
                .withUntypedValue("tempo", temporaryLimit.getAcceptableDuration())
                .withUntypedValue(LIMITSET_NAME, operationalLimitsGroupName)
                .withUntypedValue("line_with_limit", lineWithTemporaryLimit)
                .withUntypedValue("line_without_limit", lineWithoutTemporaryLimit)
                .withUntypedValue(REPLACING_LINE, newLineId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }
}
