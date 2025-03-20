/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.CreateBranchFeederBays;
import com.powsybl.iidm.modification.topology.CreateBranchFeederBaysBuilder;
import com.powsybl.iidm.network.*;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LineCreationInfos;
import org.gridsuite.modification.dto.OperationalLimitsGroupInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.List;

import static com.powsybl.iidm.network.TwoSides.ONE;
import static com.powsybl.iidm.network.TwoSides.TWO;
import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_LINE_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;

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

        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER &&
                voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LineAdder lineAdder = ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, modificationInfos, false, false);
            var position1 = ModificationUtils.getInstance().getPosition(modificationInfos.getConnectionPosition1(), modificationInfos.getBusOrBusbarSectionId1(), network, voltageLevel1);
            var position2 = ModificationUtils.getInstance().getPosition(modificationInfos.getConnectionPosition2(), modificationInfos.getBusOrBusbarSectionId2(), network, voltageLevel2);

            CreateBranchFeederBays algo = new CreateBranchFeederBaysBuilder()
                    .withBusOrBusbarSectionId1(modificationInfos.getBusOrBusbarSectionId1())
                    .withBusOrBusbarSectionId2(modificationInfos.getBusOrBusbarSectionId2())
                    .withFeederName1(modificationInfos.getConnectionName1() != null ? modificationInfos.getConnectionName1() : modificationInfos.getEquipmentId())
                    .withFeederName2(modificationInfos.getConnectionName2() != null ? modificationInfos.getConnectionName2() : modificationInfos.getEquipmentId())
                    .withDirection1(modificationInfos.getConnectionDirection1())
                    .withDirection2(modificationInfos.getConnectionDirection2())
                    .withPositionOrder1(position1)
                    .withPositionOrder2(position2)
                    .withBranchAdder(lineAdder).build();
            algo.apply(network, true, subReportNode);
        } else {
            addLine(network, voltageLevel1, voltageLevel2, modificationInfos, true, true, subReportNode);
        }
        ModificationUtils.getInstance().disconnectBranch(modificationInfos, network.getLine(modificationInfos.getEquipmentId()), subReportNode);
        Line line = network.getLine(modificationInfos.getEquipmentId());

        // Set permanent and temporary current limits
        List<OperationalLimitsGroupInfos> opLimitsGroupSide1 = modificationInfos.getOperationalLimitsGroups1();
        List<OperationalLimitsGroupInfos> opLimitsGroupSide2 = modificationInfos.getOperationalLimitsGroups2();
        if (!CollectionUtils.isEmpty(opLimitsGroupSide1)) {
            ModificationUtils.getInstance().setCurrentLimitsOnASide(opLimitsGroupSide1, line, ONE, subReportNode);
        }
        if (!CollectionUtils.isEmpty(opLimitsGroupSide2)) {
            ModificationUtils.getInstance().setCurrentLimitsOnASide(opLimitsGroupSide2, line, TWO, subReportNode);
        }
        // properties
        if (modificationInfos.getSelectedOperationalLimitsGroup1() != null) {
            line.setSelectedOperationalLimitsGroup1(modificationInfos.getSelectedOperationalLimitsGroup1());
            subReportNode.newReportNode()
                    .withMessageTemplate("limit set selected on side 1", "limit set selected on side 1 : ${selectedOperationalLimitsGroup1}")
                    .withUntypedValue("selectedOperationalLimitsGroup1", modificationInfos.getSelectedOperationalLimitsGroup1())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        if (modificationInfos.getSelectedOperationalLimitsGroup2() != null) {
            line.setSelectedOperationalLimitsGroup2(modificationInfos.getSelectedOperationalLimitsGroup2());
            subReportNode.newReportNode()
                    .withMessageTemplate("limit set selected on side 2", "limit set selected on side 2 : ${selectedOperationalLimitsGroup2}")
                    .withUntypedValue("selectedOperationalLimitsGroup2", modificationInfos.getSelectedOperationalLimitsGroup2())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        PropertiesUtils.applyProperties(line, subReportNode, modificationInfos.getProperties(), "LineProperties");
    }

    @Override
    public String getName() {
        return "LineCreation";
    }

    private void addLine(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationInfos lineCreationInfos, boolean withSwitch1, boolean withSwitch2, ReportNode subReportNode) {
        ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, lineCreationInfos, withSwitch1, withSwitch2).add();

        subReportNode.newReportNode()
                .withMessageTemplate("lineCreated", "New line with id=${id} created")
                .withUntypedValue("id", lineCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

}
