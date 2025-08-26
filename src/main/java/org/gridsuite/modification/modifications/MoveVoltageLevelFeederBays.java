/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.MoveFeederBayBuilder;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ConnectablePositionModificationInfos;
import org.gridsuite.modification.dto.MoveVoltageLevelFeederBaysInfos;

import static org.gridsuite.modification.NetworkModificationException.Type.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class MoveVoltageLevelFeederBays extends AbstractModification {

    private final MoveVoltageLevelFeederBaysInfos modificationInfos;

    public MoveVoltageLevelFeederBays(MoveVoltageLevelFeederBaysInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getVoltageLevelId());
        if (voltageLevel == null) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, "voltage level " +
                modificationInfos.getVoltageLevelId() + " is not found");
        }
        for (ConnectablePositionModificationInfos connectablePositionModificationInfos : modificationInfos.getFeederBaysAttributeList()) {
            if (voltageLevel.getTopologyKind().equals(TopologyKind.NODE_BREAKER) &&
                voltageLevel.getNodeBreakerView().getBusbarSection(connectablePositionModificationInfos.getBusBarSectionId()) == null ||
                voltageLevel.getTopologyKind().equals(TopologyKind.BUS_BREAKER) &&
                    voltageLevel.getBusBreakerView().getBus(connectablePositionModificationInfos.getBusBarSectionId()) == null
            ) {
                throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format("Busbar section %s where connectable %s is supposed to be is not found in voltage Level %s",
                    connectablePositionModificationInfos.getBusBarSectionId(), connectablePositionModificationInfos.getConnectableId(),
                    modificationInfos.getVoltageLevelId()));
            }
            if (voltageLevel.getTopologyKind().equals(TopologyKind.NODE_BREAKER) &&
                voltageLevel.getNodeBreakerView().getBusbarSection(connectablePositionModificationInfos.getTargetBusBarSectionId()) == null ||
                voltageLevel.getTopologyKind().equals(TopologyKind.BUS_BREAKER) &&
                    voltageLevel.getBusBreakerView().getBus(connectablePositionModificationInfos.getTargetBusBarSectionId()) == null
            ) {
                throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format("Target busbar section %s not found in voltage Level %s",
                    connectablePositionModificationInfos.getTargetBusBarSectionId(), modificationInfos.getVoltageLevelId()));
            }
            ConnectablePositionModification connectablePositionModification = new ConnectablePositionModification(connectablePositionModificationInfos);
            connectablePositionModification.check(network);
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        for (ConnectablePositionModificationInfos connectablePositionModificationInfos : modificationInfos.getFeederBaysAttributeList()) {
            ConnectablePositionModification connectablePositionModification = new ConnectablePositionModification(connectablePositionModificationInfos);
            connectablePositionModification.apply(network, subReportNode);
            new MoveFeederBayBuilder().withConnectableId(connectablePositionModificationInfos.getConnectableId())
                .withTargetVoltageLevelId(modificationInfos.getVoltageLevelId())
                .withTerminal(connectablePositionModification.getTerminal(network))
                .withTargetBusOrBusBarSectionId(connectablePositionModificationInfos.getTargetBusBarSectionId())
                .build().apply(network, true, subReportNode);
        }
    }

    @Override
    public String getName() {
        return ModificationType.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS.name();
    }

}
