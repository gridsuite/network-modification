/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.ReportNodeAdder;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.AbstractNetworkModification;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.modifications.DeletedLinesLimits;

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
        // to save lines limits infos because they are deleted
        DeletedLinesLimits deletedLinesLimits = new DeletedLinesLimits(network.getLine(lineToAttachTo1Id), network.getLine(lineToAttachTo2Id));

        algo.apply(network, true, subReportNode);

        // to be removed if powsybl integrate it
        mergeOperationalLimitsGroups(network.getLine(replacingLineId),
                deletedLinesLimits,
                subReportNode);
    }

    public static void mergeOperationalLimitsGroups(Line mergedLine,
                                                    DeletedLinesLimits deletedLinesLimits,
                                                    ReportNode subReportNode) {
        ReportNode mergingLimitsReportNode = subReportNode.newReportNode()
                .withMessageTemplate("network.modification.mergeLimits")
                .withUntypedValue(REPLACING_LINE, mergedLine.getId())
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
        createMergedOperationalLimitsGroups(deletedLinesLimits.getLine1Side1Limits(), deletedLinesLimits.getLine2Side1Limits(),
                mergedLine::newOperationalLimitsGroup1, TwoSides.ONE,
                deletedLinesLimits.getLine1Id(), deletedLinesLimits.getLine2Id(), mergedLine.getId(), mergingLimitsReportNode);

        // side two
        createMergedOperationalLimitsGroups(deletedLinesLimits.getLine1Side2Limits(), deletedLinesLimits.getLine2Side2Limits(),
                mergedLine::newOperationalLimitsGroup2, TwoSides.TWO,
                deletedLinesLimits.getLine1Id(), deletedLinesLimits.getLine2Id(), mergedLine.getId(), mergingLimitsReportNode);

        // set selected
        Optional<String> selectedOperationalLimitsGroupLine1Side1 = deletedLinesLimits.getSelectedLimitGroupLine1Side1();
        Optional<String> selectedOperationalLimitsGroupLine2Side1 = deletedLinesLimits.getSelectedLimitGroupLine2Side1();
        if (selectedOperationalLimitsGroupLine1Side1.isPresent() && selectedOperationalLimitsGroupLine2Side1.isPresent() &&
                selectedOperationalLimitsGroupLine1Side1.get().equals(selectedOperationalLimitsGroupLine2Side1.get())) {
            mergedLine.setSelectedOperationalLimitsGroup1(selectedOperationalLimitsGroupLine1Side1.get());
        }
        Optional<String> selectedOperationalLimitsGroupLine1Side2 = deletedLinesLimits.getSelectedLimitGroupLine1Side2();
        Optional<String> selectedOperationalLimitsGroupLine2Side2 = deletedLinesLimits.getSelectedLimitGroupLine2Side2();
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

                List<ReportNode> currentLimitsReportNodes = new ArrayList<>();
                if (currentLimitsLine1.isPresent() && currentLimitsLine2.isPresent()) {
                    currentLimitsReportNodes = mergeCurrentLimits(currentLimitsLine1.get(), currentLimitsLine2.get(), newGroup.newCurrentLimits(),
                            line1Id, line2Id, side);
                }
                List<ReportNode> propertiesLimitsReportNodes = manageLimitsGroupProperties(newGroup, groupOnLine1, groupOnLine2, newLineId, line1Id, line2Id);
                if (!currentLimitsReportNodes.isEmpty() || !propertiesLimitsReportNodes.isEmpty()) {
                    ReportNode limitsGroupReport = reportNode.newReportNode()
                            .withMessageTemplate("network.modification.mergeLimitsOfLimitSet")
                            .withUntypedValue(LIMITSET_NAME, operationalLimitsGroupId)
                            .withUntypedValue("side", side.name())
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .add();
                    copyReportNodes(currentLimitsReportNodes, limitsGroupReport);
                    copyReportNodes(propertiesLimitsReportNodes, limitsGroupReport);
                }

            }
        });
    }

    private static List<ReportNode> manageLimitsGroupProperties(OperationalLimitsGroup newGroup,
                                                                OperationalLimitsGroup groupOnLine1,
                                                                OperationalLimitsGroup groupOnLine2,
                                                                String newLineId,
                                                                String line1Id,
                                                                String line2Id) {
        List<ReportNode> reportNodes = new ArrayList<>();
        groupOnLine1.getPropertyNames().forEach(propertyNameLine1 -> {
            String valueLine2 = groupOnLine2.getProperty(propertyNameLine1);
            if (valueLine2 != null) {
                String valueLine1 = groupOnLine1.getProperty(propertyNameLine1);
                if (valueLine1.equals(valueLine2)) {
                    newGroup.setProperty(propertyNameLine1, valueLine1);
                } else {
                    reportNodes.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("network.modification.propertyOfLimitsGroupDeletedAfterMerge")
                            .withUntypedValue("property_name", propertyNameLine1)
                            .withUntypedValue("property_value_A", valueLine1)
                            .withUntypedValue("line_A", line1Id)
                            .withUntypedValue("property_value_B", valueLine2)
                            .withUntypedValue("line_B", line2Id)
                            .withUntypedValue(REPLACING_LINE, newLineId)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
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
        return reportNodes;
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

    private static List<ReportNode> mergeCurrentLimits(CurrentLimits currentLimitsLine1,
                                                       CurrentLimits currentLimitsLine2,
                                                       CurrentLimitsAdder adder,
                                                       String line1Id,
                                                       String line2Id,
                                                       TwoSides side) {
        List<ReportNode> reportNodes = new ArrayList<>();
        adder.setPermanentLimit(Math.min(currentLimitsLine1.getPermanentLimit(), currentLimitsLine2.getPermanentLimit()));
        logMergingPermanentLimits(reportNodes, currentLimitsLine1.getPermanentLimit(), currentLimitsLine2.getPermanentLimit(),
                line1Id, line2Id);
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
                logDeletedTemporaryLimit(reportNodes, temporaryLimitLine2, line2Id, line1Id, side);
            } else if (temporaryLimitLine1 != null && temporaryLimitLine2 == null) {
                logDeletedTemporaryLimit(reportNodes, temporaryLimitLine1, line1Id, line2Id, side);
            } else if (temporaryLimitLine1 != null) {
                if (temporaryLimitLine1.getAcceptableDuration() != temporaryLimitLine2.getAcceptableDuration()) {
                    logSameTemporaryLimitWithDifferentAcceptableDuration(reportNodes, temporaryLimitLine1.getName(),
                            line1Id, temporaryLimitLine1.getAcceptableDuration(), line2Id,
                            temporaryLimitLine2.getAcceptableDuration(), side);
                } else {
                    adder.beginTemporaryLimit()
                            .setName(temporaryLimitName)
                            .setAcceptableDuration(temporaryLimitLine1.getAcceptableDuration())
                            .setValue(Math.min(temporaryLimitLine1.getValue(), temporaryLimitLine2.getValue()))
                            .endTemporaryLimit();
                    logMergingTemporaryLimits(reportNodes, temporaryLimitName, temporaryLimitLine1.getAcceptableDuration(),
                            temporaryLimitLine1.getValue(), temporaryLimitLine2.getValue(), line1Id, line2Id);
                }
            }
        });
        adder.add();
        return reportNodes;
    }

    private static void logDeletedTemporaryLimit(List<ReportNode> reportNodes, LoadingLimits.TemporaryLimit temporaryLimit,
                                                 String lineWithTemporaryLimit, String lineWithoutTemporaryLimit, TwoSides side) {
        reportNodes.add(ReportNode.newRootReportNode()
                .withMessageTemplate("network.modification.temporaryLimitsDeletedAfterMerge")
                .withUntypedValue("limit_name", temporaryLimit.getName())
                .withUntypedValue("tempo", temporaryLimit.getAcceptableDuration())
                .withUntypedValue("applicability", side.name())
                .withUntypedValue("line_with_limit", lineWithTemporaryLimit)
                .withUntypedValue("line_without_limit", lineWithoutTemporaryLimit)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private static void logMergingPermanentLimits(List<ReportNode> reportNodes, Double permanentLimit1, Double permanentLimit2,
                                                 String line1Id, String line2Id) {
        reportNodes.add(ReportNode.newRootReportNode()
                .withMessageTemplate("network.modification.mergingPermanentLimits")
                .withUntypedValue("value_A", permanentLimit1)
                .withUntypedValue("line_A", line1Id)
                .withUntypedValue("value_B", permanentLimit2)
                .withUntypedValue("line_B", line2Id)
                .withUntypedValue("lowest_value", Math.min(permanentLimit1, permanentLimit2))
                .withSeverity(TypedValue.DETAIL_SEVERITY)
                .build());
    }

    private static void logMergingTemporaryLimits(List<ReportNode> reportNodes, String limitName, int acceptableDuration,
                                                  Double temporaryLimitLine1, Double temporaryLimitLine2, String line1Id, String line2Id) {
        reportNodes.add(ReportNode.newRootReportNode()
                .withMessageTemplate("network.modification.mergingTemporaryLimits")
                .withUntypedValue("limit_name", limitName)
                .withUntypedValue("tempo", acceptableDuration)
                .withUntypedValue("value_A", temporaryLimitLine1)
                .withUntypedValue("value_B", temporaryLimitLine2)
                .withUntypedValue("line_A", line1Id)
                .withUntypedValue("line_B", line2Id)
                .withUntypedValue("lowest_value", Math.min(temporaryLimitLine1, temporaryLimitLine2))
                .withSeverity(TypedValue.DETAIL_SEVERITY)
                .build());
    }

    private static void logSameTemporaryLimitWithDifferentAcceptableDuration(List<ReportNode> reportNodes, String temporaryLimitName,
                                                                             String line1, Integer acceptableDuration1,
                                                                             String line2, Integer acceptableDuration2,
                                                                             TwoSides side) {
        reportNodes.add(ReportNode.newRootReportNode()
                .withMessageTemplate("network.modification.temporaryLimitsWithDifferentAcceptableDuration")
                .withUntypedValue("limit_name", temporaryLimitName)
                .withUntypedValue("applicability", side.name())
                .withUntypedValue("tempo1", acceptableDuration1)
                .withUntypedValue("line1", line1)
                .withUntypedValue("tempo2", acceptableDuration2)
                .withUntypedValue("line2", line2)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private static void copyReportNodes(List<ReportNode> children, ReportNode parentReportNode) {
        children.forEach(currentLimitsReportNode -> {
            ReportNodeAdder reportNodeAdder = parentReportNode.newReportNode()
                    .withMessageTemplate(currentLimitsReportNode.getMessageKey())
                    .withSeverity(TypedValue.INFO_SEVERITY);
            currentLimitsReportNode.getValues().forEach((key, value) -> reportNodeAdder.withUntypedValue(key, String.valueOf(value)));
            reportNodeAdder.add();
        });
    }
}
