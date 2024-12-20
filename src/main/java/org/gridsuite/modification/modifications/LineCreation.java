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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CurrentLimitsInfos;
import org.gridsuite.modification.dto.LineCreationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.*;

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
        ModificationUtils.getInstance().controlBranchCreation(network,
                modificationInfos.getVoltageLevelId1(), modificationInfos.getBusOrBusbarSectionId1(), modificationInfos.getConnectionPosition1(),
                modificationInfos.getVoltageLevelId2(), modificationInfos.getBusOrBusbarSectionId2(), modificationInfos.getConnectionPosition2());
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

        // Set permanent and temporary current limits
        CurrentLimitsInfos currentLimitsInfos1 = modificationInfos.getCurrentLimits1();
        CurrentLimitsInfos currentLimitsInfos2 = modificationInfos.getCurrentLimits2();
        if (currentLimitsInfos1 != null || currentLimitsInfos2 != null) {
            var line = ModificationUtils.getInstance().getLine(network, modificationInfos.getEquipmentId());
            ModificationUtils.getInstance().setCurrentLimits(currentLimitsInfos1, line.newCurrentLimits1());
            ModificationUtils.getInstance().setCurrentLimits(currentLimitsInfos2, line.newCurrentLimits2());
        }
        ModificationUtils.getInstance().disconnectBranch(modificationInfos, network.getLine(modificationInfos.getEquipmentId()), subReportNode);
        // properties
        Line line = network.getLine(modificationInfos.getEquipmentId());
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
