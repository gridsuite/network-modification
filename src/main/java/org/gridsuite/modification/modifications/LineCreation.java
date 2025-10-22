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
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LineCreationInfos;
import org.gridsuite.modification.dto.OperationalLimitsGroupInfos;
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
public class LineCreation extends AbstractModification {

    private final LineCreationInfos modificationInfos;

    public LineCreation(LineCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getLine(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }
        String errorMessage = "Line '" + modificationInfos.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().controlBranchCreation(network,
                modificationInfos.getVoltageLevelId1(), modificationInfos.getBusOrBusbarSectionId1(), modificationInfos.getConnectionPosition1(),
                modificationInfos.getVoltageLevelId2(), modificationInfos.getBusOrBusbarSectionId2(), modificationInfos.getConnectionPosition2());
        checkIsNotNegativeValue(errorMessage, modificationInfos.getR(), CREATE_LINE_ERROR, "Resistance R");
        checkIsNotNegativeValue(errorMessage, modificationInfos.getG1(), CREATE_LINE_ERROR, "Conductance on side 1 G1");
        checkIsNotNegativeValue(errorMessage, modificationInfos.getG2(), CREATE_LINE_ERROR, "Conductance on side 2 G2");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId1());
        VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId2());

        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.Characteristics").add();
        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER &&
                voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LineAdder lineAdder = ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, modificationInfos, false, false);
            createBranchInNodeBreaker(voltageLevel1, voltageLevel2, modificationInfos, network, lineAdder, characteristicsReporter);
        } else {
            addLine(network, voltageLevel1, voltageLevel2, modificationInfos, true, true, characteristicsReporter);
        }
        ModificationUtils.getInstance().disconnectBranch(modificationInfos, network.getLine(modificationInfos.getEquipmentId()), characteristicsReporter);
        Line line = network.getLine(modificationInfos.getEquipmentId());

        // Set permanent and temporary current limits
        ReportNode limitsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.limitsCreated").add();
        List<OperationalLimitsGroupInfos> opLimitsGroupSide1 = ModificationUtils.getOperationalLimitsGroupsOnSide(modificationInfos.getOperationalLimitsGroups(), Applicability.SIDE1);
        List<OperationalLimitsGroupInfos> opLimitsGroupSide2 = ModificationUtils.getOperationalLimitsGroupsOnSide(modificationInfos.getOperationalLimitsGroups(), Applicability.SIDE2);
        ReportNode reportNode = null;
        if (!CollectionUtils.isEmpty(opLimitsGroupSide1)) {
            reportNode = limitsReporter.newReportNode()
                .withSeverity(TypedValue.INFO_SEVERITY)
                .withMessageTemplate("network.modification.LimitSets")
                .add();
            ModificationUtils.getInstance().setCurrentLimitsOnASide(reportNode, opLimitsGroupSide1, line, ONE);
        }
        if (!CollectionUtils.isEmpty(opLimitsGroupSide2)) {
            if (reportNode == null) {
                reportNode = limitsReporter.newReportNode()
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .withMessageTemplate("network.modification.LimitSets")
                    .add();
            }
            ModificationUtils.getInstance().setCurrentLimitsOnASide(reportNode, opLimitsGroupSide2, line, TWO);
        }
        List<ReportNode> limitSetsOnSideReportNodes = new ArrayList<>();
        if (modificationInfos.getSelectedOperationalLimitsGroup1() != null) {
            if (!ModificationUtils.hasLimitSet(opLimitsGroupSide1, modificationInfos.getSelectedOperationalLimitsGroup1())) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetAbsentOnSide1")
                    .withUntypedValue("selectedOperationalLimitsGroup", modificationInfos.getSelectedOperationalLimitsGroup1())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            } else {
                line.setSelectedOperationalLimitsGroup1(modificationInfos.getSelectedOperationalLimitsGroup1());
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetSelectedOnSide1")
                    .withUntypedValue("selectedOperationalLimitsGroup1", modificationInfos.getSelectedOperationalLimitsGroup1())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }
        }
        if (modificationInfos.getSelectedOperationalLimitsGroup2() != null) {
            if (!ModificationUtils.hasLimitSet(opLimitsGroupSide2, modificationInfos.getSelectedOperationalLimitsGroup2())) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetAbsentOnSide2")
                    .withUntypedValue("selectedOperationalLimitsGroup", modificationInfos.getSelectedOperationalLimitsGroup2())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            } else {
                line.setSelectedOperationalLimitsGroup2(modificationInfos.getSelectedOperationalLimitsGroup2());
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetSelectedOnSide2")
                    .withUntypedValue("selectedOperationalLimitsGroup2", modificationInfos.getSelectedOperationalLimitsGroup2())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }
        }

        if (!limitSetsOnSideReportNodes.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(limitsReporter, limitSetsOnSideReportNodes,
                "network.modification.ActiveLimitSets");
        }

        // properties
        PropertiesUtils.applyProperties(line, characteristicsReporter, modificationInfos.getProperties(), "network.modification.LineProperties");
    }

    @Override
    public String getName() {
        return "LineCreation";
    }

    private void addLine(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationInfos lineCreationInfos, boolean withSwitch1, boolean withSwitch2, ReportNode subReportNode) {
        ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, lineCreationInfos, withSwitch1, withSwitch2).add();

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.lineCreated")
                .withUntypedValue("id", lineCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

}
