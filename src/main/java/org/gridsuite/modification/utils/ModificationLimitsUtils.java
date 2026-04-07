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

    private ModificationLimitsUtils() {

    }

    public static void applyRevertModificationWithMergingOfLimits(Network network,
                                                            String lineToAttachTo1Id,
                                                            String lineToAttachTo2Id,
                                                            String replacingLineId,
                                                            AbstractNetworkModification algo,
                                                            ReportNode subReportNode) {
        // to be removed if powsybl integrate it
        Line line1 = network.getLine(lineToAttachTo1Id);
        String line1Id = line1.getId();
        Optional<String> selectedGroupLine1Side1 = line1.getSelectedOperationalLimitsGroupId1();
        Optional<String> selectedGroupLine1Side2 = line1.getSelectedOperationalLimitsGroupId2();
        Collection<OperationalLimitsGroup> groupsLine1Side1 = line1.getOperationalLimitsGroups1();
        Collection<OperationalLimitsGroup> groupsLine1Side2 = line1.getOperationalLimitsGroups2();
        Line line2 = network.getLine(lineToAttachTo2Id);
        String line2Id = line2.getId();
        Optional<String> selectedGroupLine2Side1 = line2.getSelectedOperationalLimitsGroupId1();
        Optional<String> selectedGroupLine2Side2 = line2.getSelectedOperationalLimitsGroupId2();
        Collection<OperationalLimitsGroup> groupsLine2Side1 = line2.getOperationalLimitsGroups1();
        Collection<OperationalLimitsGroup> groupsLine2Side2 = line2.getOperationalLimitsGroups2();

        algo.apply(network, true, subReportNode);

        // to be removed if powsybl integrate it
        setMergedOperationalLimitsGroups(network.getLine(replacingLineId),
                groupsLine1Side1,
                groupsLine1Side2,
                groupsLine2Side1,
                groupsLine2Side2,
                selectedGroupLine1Side1.orElse(null),
                selectedGroupLine1Side2.orElse(null),
                selectedGroupLine2Side1.orElse(null),
                selectedGroupLine2Side2.orElse(null),
                line1Id,
                line2Id,
                subReportNode);
    }

    public static void setMergedOperationalLimitsGroups(Line mergedLine,
                                                        Collection<OperationalLimitsGroup> groupsLine1Side1,
                                                        Collection<OperationalLimitsGroup> groupsLine1Side2,
                                                        Collection<OperationalLimitsGroup> groupsLine2Side1,
                                                        Collection<OperationalLimitsGroup> groupsLine2Side2,
                                                        String selectedGroupLine1Side1,
                                                        String selectedGroupLine1Side2,
                                                        String selectedGroupLine2Side1,
                                                        String selectedGroupLine2Side2,
                                                        String line1Id,
                                                        String line2Id,
                                                        ReportNode subReportNode) {
        ReportNode mergingLimitsReportNode = subReportNode.newReportNode()
                .withMessageTemplate("network.modification.mergeLimits")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        // remove all limitsGroups as the modification does not do as wanted
        mergedLine.getOperationalLimitsGroups1().forEach(group -> mergedLine.removeOperationalLimitsGroup1(group.getId()));
        mergedLine.getOperationalLimitsGroups2().forEach(group -> mergedLine.removeOperationalLimitsGroup2(group.getId()));

        // side one
        createMergedOperationalLimitsGroups(groupsLine1Side1, groupsLine2Side1, mergedLine::newOperationalLimitsGroup1, TwoSides.ONE,
                line1Id, line2Id, mergedLine.getId(), mergingLimitsReportNode);

        // side two
        createMergedOperationalLimitsGroups(groupsLine1Side2, groupsLine2Side2, mergedLine::newOperationalLimitsGroup2, TwoSides.TWO,
                line1Id, line2Id, mergedLine.getId(), mergingLimitsReportNode);

        // set selected
        if (selectedGroupLine1Side1 != null && selectedGroupLine1Side1.equals(selectedGroupLine2Side1)) {
            mergedLine.setSelectedOperationalLimitsGroup1(selectedGroupLine1Side1);
        }
        if (selectedGroupLine1Side2 != null && selectedGroupLine1Side2.equals(selectedGroupLine2Side2)) {
            mergedLine.setSelectedOperationalLimitsGroup2(selectedGroupLine1Side2);
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
        List<String> deletedGroups = new ArrayList<>();
        Map<String, OperationalLimitsGroup> groupsMapOnLine1 = groupsOnLine1.stream().collect(Collectors.toMap(OperationalLimitsGroup::getId, Function.identity()));
        Map<String, OperationalLimitsGroup> groupsMapOnLine2 = groupsOnLine2.stream().collect(Collectors.toMap(OperationalLimitsGroup::getId, Function.identity()));
        for (String operationalLimitGroupIdLine1 : groupsMapOnLine1.keySet()) {
            if (!groupsMapOnLine2.containsKey(operationalLimitGroupIdLine1)) {
                deletedGroups.add(operationalLimitGroupIdLine1);
            } else {
                OperationalLimitsGroup newGroup = operationalLimitsGroupConstructor.apply(operationalLimitGroupIdLine1);
                OperationalLimitsGroup groupOnLine1 = groupsMapOnLine1.get(operationalLimitGroupIdLine1);
                OperationalLimitsGroup groupOnLine2 = groupsMapOnLine2.get(operationalLimitGroupIdLine1);
                Optional<CurrentLimits> currentLimitsLine1 = groupOnLine1.getCurrentLimits();
                Optional<CurrentLimits> currentLimitsLine2 = groupOnLine2.getCurrentLimits();
                if (currentLimitsLine1.isPresent() && currentLimitsLine2.isPresent()) {
                    mergedCurrentLimits(currentLimitsLine1.get(), currentLimitsLine2.get(), newGroup.newCurrentLimits(),
                            operationalLimitGroupIdLine1, line1Id, line2Id, newLineId, reportNode);
                }
                manageLimitsGroupProperties(newGroup, groupOnLine1, groupOnLine2, newLineId, line1Id, line2Id, reportNode);
            }
        }
        // report deleted groups on branch 1
        logDeletedGroups(deletedGroups, reportNode, side, line1Id, line2Id, newLineId);

        deletedGroups = new ArrayList<>();
        for (String operationalLimitGroupIdLine2 : groupsMapOnLine2.keySet()) {
            if (!groupsMapOnLine1.containsKey(operationalLimitGroupIdLine2)) {
                deletedGroups.add(operationalLimitGroupIdLine2);
            }
        }
        // report deleted groups on branch 2
        logDeletedGroups(deletedGroups, reportNode, side, line2Id, line1Id, newLineId);
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
                            .withUntypedValue("limitset_name", newGroup.getId())
                            .withUntypedValue("property_name", propertyNameLine1)
                            .withUntypedValue("property_value_A", valueLine1)
                            .withUntypedValue("line_A", line1Id)
                            .withUntypedValue("property_value_B", valueLine2)
                            .withUntypedValue("line_B", line2Id)
                            .withUntypedValue("replacing_line", newLineId)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .add();
                }
            }
        });
    }

    private static void logDeletedGroups(List<String> deletedGroups, ReportNode reportNode, TwoSides side,
                                         String lineWithLimitSet, String lineWithoutLimitSet, String newLineId) {
        deletedGroups.forEach(deletedGroup -> reportNode.newReportNode()
                .withMessageTemplate("network.modification.limitGroupsDeletedAfterMerge")
                .withUntypedValue("limitset_name", deletedGroup)
                .withUntypedValue("applicability", side.name())
                .withUntypedValue("line_with_limitset", lineWithLimitSet)
                .withUntypedValue("line_without_limitset", lineWithoutLimitSet)
                .withUntypedValue("replacing_line", newLineId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add());
    }

    private static void mergedCurrentLimits(CurrentLimits currentLimitsLine1,
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
        for (String temporaryLimitName : temporaryLimitsOnLine1.keySet()) {
            if (!temporaryLimitsOnLine2.containsKey(temporaryLimitName)) {
                LoadingLimits.TemporaryLimit removedTemporaryLimit = temporaryLimitsOnLine1.get(temporaryLimitName);
                logDeletedTemporaryLimits(reportNode, removedTemporaryLimit, operationalLimitsGroupId, line1Id, line2Id, newLineId);
            } else {
                LoadingLimits.TemporaryLimit temporaryLimitLine1 = temporaryLimitsOnLine1.get(temporaryLimitName);
                LoadingLimits.TemporaryLimit temporaryLimitLine2 = temporaryLimitsOnLine2.get(temporaryLimitName);
                if (temporaryLimitLine1.getAcceptableDuration() != temporaryLimitLine2.getAcceptableDuration()) {
                    logDeletedTemporaryLimits(reportNode, temporaryLimitLine1, operationalLimitsGroupId, line1Id, line2Id, newLineId);
                } else {
                    adder.beginTemporaryLimit()
                            .setName(temporaryLimitLine1.getName())
                            .setAcceptableDuration(temporaryLimitLine1.getAcceptableDuration())
                            .setValue(Math.min(temporaryLimitLine1.getValue(), temporaryLimitLine2.getValue()))
                            .endTemporaryLimit();
                }
            }
        }

        // logs for limit set deleted on line2
        for (String temporaryLimitName : temporaryLimitsOnLine2.keySet()) {
            if (!temporaryLimitsOnLine2.containsKey(temporaryLimitName)) {
                LoadingLimits.TemporaryLimit removedTemporaryLimit = temporaryLimitsOnLine2.get(temporaryLimitName);
                logDeletedTemporaryLimits(reportNode, removedTemporaryLimit, operationalLimitsGroupId, line2Id, line1Id, newLineId);
            } else {
                LoadingLimits.TemporaryLimit temporaryLimitLine1 = temporaryLimitsOnLine1.get(temporaryLimitName);
                LoadingLimits.TemporaryLimit temporaryLimitLine2 = temporaryLimitsOnLine2.get(temporaryLimitName);
                if (temporaryLimitLine1.getAcceptableDuration() != temporaryLimitLine2.getAcceptableDuration()) {
                    logDeletedTemporaryLimits(reportNode, temporaryLimitLine2, operationalLimitsGroupId, line2Id, line1Id, newLineId);
                }
            }
        }
        adder.add();
    }

    private static void logDeletedTemporaryLimits(ReportNode reportNode, LoadingLimits.TemporaryLimit temporaryLimit,
                                                  String operationalLimitsGroupName, String lineWithTemporaryLimit, String lineWithoutTemporaryLimit, String newLineId) {
        reportNode.newReportNode()
                .withMessageTemplate("network.modification.temporaryLimitsDeletedAfterMerge")
                .withUntypedValue("limit_name", temporaryLimit.getName())
                .withUntypedValue("tempo", temporaryLimit.getAcceptableDuration())
                .withUntypedValue("limitset_name", operationalLimitsGroupName)
                .withUntypedValue("line_with_limit", lineWithTemporaryLimit)
                .withUntypedValue("line_without_limit", lineWithoutTemporaryLimit)
                .withUntypedValue("replacing_line", newLineId)
                .add();
    }
}
