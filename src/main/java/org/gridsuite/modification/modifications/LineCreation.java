/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;

import static com.powsybl.iidm.network.TwoSides.ONE;
import static com.powsybl.iidm.network.TwoSides.TWO;
import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_LINE_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.createBranchInNodeBreaker;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@AllArgsConstructor
@Builder
public class LineCreation extends AbstractModification {

    private String equipmentId;
    private List<FreePropertyInfos> properties;
    private String equipmentName;
    private double r;
    private double x;
    private String voltageLevelId1;
    private String voltageLevelId2;
    private String busOrBusbarSectionId1;
    private String busOrBusbarSectionId2;
    private List<OperationalLimitsGroupInfos> operationalLimitsGroups;
    private String selectedOperationalLimitsGroupId1;
    private String selectedOperationalLimitsGroupId2;
    private String connectionName1;
    private ConnectablePosition.Direction connectionDirection1;
    private String connectionName2;
    private ConnectablePosition.Direction connectionDirection2;
    private Integer connectionPosition1;
    private Integer connectionPosition2;
    private boolean connected1;
    private boolean connected2;
    private Double g1;
    private Double b1;
    private Double g2;
    private Double b2;
    private List<LineSegmentInfos> lineSegments;

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getLine(equipmentId) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, equipmentId);
        }
        String errorMessage = "Line '" + equipmentId + "' : ";
        ModificationUtils.getInstance().controlBranchCreation(network,
                voltageLevelId1, busOrBusbarSectionId1, voltageLevelId2, busOrBusbarSectionId2);
        checkIsNotNegativeValue(errorMessage, r, CREATE_LINE_ERROR, "Resistance R");
        checkIsNotNegativeValue(errorMessage, g1, CREATE_LINE_ERROR, "Conductance on side 1 G1");
        checkIsNotNegativeValue(errorMessage, g2, CREATE_LINE_ERROR, "Conductance on side 2 G2");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId1);
        VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId2);

        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.Characteristics").add();
        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER &&
                voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LineAdder lineAdder = ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, equipmentId, equipmentName,
                    voltageLevelId1, voltageLevelId2, busOrBusbarSectionId1, busOrBusbarSectionId2, r, x, g1, b1, g2, b2, false, false);
            createBranchInNodeBreaker(voltageLevel1, voltageLevel2, equipmentId, busOrBusbarSectionId1, busOrBusbarSectionId2, connectionName1, connectionName2,
                    connectionDirection1, connectionDirection2, connectionPosition1, connectionPosition2, network, lineAdder, characteristicsReporter);
        } else {
            addLine(network, voltageLevel1, voltageLevel2, true, true, characteristicsReporter);
        }
        ModificationUtils.getInstance().disconnectBranch(connected1, connected2, equipmentId, network.getLine(equipmentId), characteristicsReporter);
        Line line = network.getLine(equipmentId);

        addLimits(operationalLimitsGroups, selectedOperationalLimitsGroupId1, selectedOperationalLimitsGroupId2, subReportNode, line);

        // properties
        PropertiesUtils.applyProperties(line, characteristicsReporter, properties, "network.modification.LineProperties");
    }

    public static void addLimits(List<OperationalLimitsGroupInfos> operationalLimitsGroups, String selectedOperationalLimitsGroupId1,
                                 String selectedOperationalLimitsGroupId2, ReportNode subReportNode, Line line) {
        // Set permanent and temporary current limits
        ReportNode limitsReporter = null;
        List<OperationalLimitsGroupInfos> opLimitsGroupSide1 = ModificationUtils.getOperationalLimitsGroupsOnSide(operationalLimitsGroups, Applicability.SIDE1);
        List<OperationalLimitsGroupInfos> opLimitsGroupSide2 = ModificationUtils.getOperationalLimitsGroupsOnSide(operationalLimitsGroups, Applicability.SIDE2);
        ReportNode reportNode;
        if (!CollectionUtils.isEmpty(operationalLimitsGroups)) {
            limitsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.limitsCreated").add();
            reportNode = addLimitSetReportNode(limitsReporter);

            for (OperationalLimitsGroupInfos olgInfos : operationalLimitsGroups) {
                ReportNode limitSetNode = reportNode.newReportNode()
                        .withMessageTemplate("network.modification.limitSetAdded")
                        .withUntypedValue("name", olgInfos.getId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();

                if (olgInfos.getApplicability() == Applicability.SIDE1 || olgInfos.getApplicability() == Applicability.EQUIPMENT) {
                    ModificationUtils.getInstance().setCurrentLimitsOnASide(limitSetNode, olgInfos, line, ONE);
                }
                if (olgInfos.getApplicability() == Applicability.SIDE2 || olgInfos.getApplicability() == Applicability.EQUIPMENT) {
                    ModificationUtils.getInstance().setCurrentLimitsOnASide(limitSetNode, olgInfos, line, TWO);
                }
            }
        }

        List<ReportNode> limitSetsOnSideReportNodes = new ArrayList<>();
        if (selectedOperationalLimitsGroupId1 != null) {
            if (!ModificationUtils.hasLimitSet(opLimitsGroupSide1, selectedOperationalLimitsGroupId1)) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetAbsentOnSide1")
                        .withUntypedValue("selectedOperationalLimitsGroup", selectedOperationalLimitsGroupId1)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else {
                line.setSelectedOperationalLimitsGroup1(selectedOperationalLimitsGroupId1);
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetSelectedOnSide1")
                        .withUntypedValue("selectedOperationalLimitsGroup1", selectedOperationalLimitsGroupId1)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        }
        if (selectedOperationalLimitsGroupId2 != null) {
            if (!ModificationUtils.hasLimitSet(opLimitsGroupSide2, selectedOperationalLimitsGroupId2)) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetAbsentOnSide2")
                        .withUntypedValue("selectedOperationalLimitsGroup", selectedOperationalLimitsGroupId2)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else {
                line.setSelectedOperationalLimitsGroup2(selectedOperationalLimitsGroupId2);
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetSelectedOnSide2")
                        .withUntypedValue("selectedOperationalLimitsGroup2", selectedOperationalLimitsGroupId2)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        }

        if (!limitSetsOnSideReportNodes.isEmpty()) {
            if (limitsReporter == null) {
                limitsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.limitsCreated").add();
            }
            ModificationUtils.getInstance().reportModifications(limitsReporter, limitSetsOnSideReportNodes,
                    "network.modification.ActiveLimitSets");
        }
    }

    private static ReportNode addLimitSetReportNode(ReportNode limitsReporter) {
        return limitsReporter.newReportNode()
                .withSeverity(TypedValue.INFO_SEVERITY)
                .withMessageTemplate("network.modification.LimitSets")
                .add();
    }

    @Override
    public String getName() {
        return "LineCreation";
    }

    private void addLine(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, boolean withSwitch1, boolean withSwitch2,
            ReportNode subReportNode) {
        ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, equipmentId, equipmentName,
                voltageLevelId1, voltageLevelId2, busOrBusbarSectionId1, busOrBusbarSectionId2, r, x, g1, b1, g2, b2, withSwitch1, withSwitch2).add();

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.lineCreated")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

}
