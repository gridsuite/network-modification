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
import org.gridsuite.modification.model.LineCreationModel;
import org.gridsuite.modification.model.OperationalLimitsGroupModel;
import org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability;
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

    private final LineCreationModel modificationModel;

    public LineCreation(LineCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getLine(modificationModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationModel.getEquipmentId());
        }
        String errorMessage = "Line '" + modificationModel.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().controlBranchCreation(network,
            modificationModel.getVoltageLevelId1(), modificationModel.getBusOrBusbarSectionId1(),
            modificationModel.getVoltageLevelId2(), modificationModel.getBusOrBusbarSectionId2());
        checkIsNotNegativeValue(errorMessage, modificationModel.getR(), CREATE_LINE_ERROR, "Resistance R");
        checkIsNotNegativeValue(errorMessage, modificationModel.getG1(), CREATE_LINE_ERROR, "Conductance on side 1 G1");
        checkIsNotNegativeValue(errorMessage, modificationModel.getG2(), CREATE_LINE_ERROR, "Conductance on side 2 G2");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId1());
        VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId2());

        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.Characteristics").add();
        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER &&
            voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LineAdder lineAdder = ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, modificationModel, false, false);
            createBranchInNodeBreaker(voltageLevel1, voltageLevel2, modificationModel, network, lineAdder, characteristicsReporter);
        } else {
            addLine(network, voltageLevel1, voltageLevel2, modificationModel, true, true, characteristicsReporter);
        }
        ModificationUtils.getInstance().disconnectBranch(modificationModel, network.getLine(modificationModel.getEquipmentId()), characteristicsReporter);
        Line line = network.getLine(modificationModel.getEquipmentId());

        addLimits(modificationModel, subReportNode, line);

        // properties
        PropertiesUtils.applyProperties(line, characteristicsReporter, modificationModel.getProperties(), "network.modification.LineProperties");
    }

    public static void addLimits(LineCreationModel modificationModel, ReportNode subReportNode, Line line) {
        // Set permanent and temporary current limits
        ReportNode limitsReporter = null;
        List<OperationalLimitsGroupModel> opLimitsGroupSide1 = ModificationUtils.getOperationalLimitsGroupsOnSide(modificationModel.getOperationalLimitsGroups(), Applicability.SIDE1);
        List<OperationalLimitsGroupModel> opLimitsGroupSide2 = ModificationUtils.getOperationalLimitsGroupsOnSide(modificationModel.getOperationalLimitsGroups(), Applicability.SIDE2);
        ReportNode reportNode;
        if (!CollectionUtils.isEmpty(modificationModel.getOperationalLimitsGroups())) {
            limitsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.limitsCreated").add();
            reportNode = addLimitSetReportNode(limitsReporter);

            for (OperationalLimitsGroupModel olgModel : modificationModel.getOperationalLimitsGroups()) {
                ReportNode limitSetNode = reportNode.newReportNode()
                    .withMessageTemplate("network.modification.limitSetAdded")
                    .withUntypedValue("name", olgModel.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();

                if (olgModel.getApplicability() == Applicability.SIDE1 || olgModel.getApplicability() == Applicability.EQUIPMENT) {
                    ModificationUtils.getInstance().setCurrentLimitsOnASide(limitSetNode, olgModel, line, ONE);
                }
                if (olgModel.getApplicability() == Applicability.SIDE2 || olgModel.getApplicability() == Applicability.EQUIPMENT) {
                    ModificationUtils.getInstance().setCurrentLimitsOnASide(limitSetNode, olgModel, line, TWO);
                }
            }
        }

        List<ReportNode> limitSetsOnSideReportNodes = new ArrayList<>();
        if (modificationModel.getSelectedOperationalLimitsGroupId1() != null) {
            if (!ModificationUtils.hasLimitSet(opLimitsGroupSide1, modificationModel.getSelectedOperationalLimitsGroupId1())) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetAbsentOnSide1")
                    .withUntypedValue("selectedOperationalLimitsGroup", modificationModel.getSelectedOperationalLimitsGroupId1())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            } else {
                line.setSelectedOperationalLimitsGroup1(modificationModel.getSelectedOperationalLimitsGroupId1());
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetSelectedOnSide1")
                    .withUntypedValue("selectedOperationalLimitsGroup1", modificationModel.getSelectedOperationalLimitsGroupId1())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }
        }
        if (modificationModel.getSelectedOperationalLimitsGroupId2() != null) {
            if (!ModificationUtils.hasLimitSet(opLimitsGroupSide2, modificationModel.getSelectedOperationalLimitsGroupId2())) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetAbsentOnSide2")
                    .withUntypedValue("selectedOperationalLimitsGroup", modificationModel.getSelectedOperationalLimitsGroupId2())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            } else {
                line.setSelectedOperationalLimitsGroup2(modificationModel.getSelectedOperationalLimitsGroupId2());
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetSelectedOnSide2")
                    .withUntypedValue("selectedOperationalLimitsGroup2", modificationModel.getSelectedOperationalLimitsGroupId2())
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

    private void addLine(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationModel lineCreationModel, boolean withSwitch1, boolean withSwitch2, ReportNode subReportNode) {
        ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, lineCreationModel, withSwitch1, withSwitch2).add();

        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.lineCreated")
            .withUntypedValue("id", lineCreationModel.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
    }

}
