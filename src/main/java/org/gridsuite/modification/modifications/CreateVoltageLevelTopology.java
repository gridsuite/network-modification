/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.MoveFeederBayBuilder;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BusbarSectionPosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CreateVoltageLevelTopologyInfos;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class CreateVoltageLevelTopology extends AbstractModification {

    private final CreateVoltageLevelTopologyInfos createVoltageLevelTopologyInfos;

    public CreateVoltageLevelTopology(CreateVoltageLevelTopologyInfos createVoltageLevelTopology) {
        this.createVoltageLevelTopologyInfos = createVoltageLevelTopology;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevel voltageLevel = network.getVoltageLevel(createVoltageLevelTopologyInfos.getVoltageLevelId());
        int lowBusOrBusbarIndex = findLowBusOrBusbarIndex(voltageLevel);
        new com.powsybl.iidm.modification.topology.CreateVoltageLevelTopologyBuilder()
            .withVoltageLevelId(createVoltageLevelTopologyInfos.getVoltageLevelId())
            .withSectionCount(createVoltageLevelTopologyInfos.getSectionCount())
            .withAlignedBusesOrBusbarCount(1)
            .withLowBusOrBusbarIndex(lowBusOrBusbarIndex)
            .withSwitchKinds(createVoltageLevelTopologyInfos.getSwitchKinds())
            .build().apply(network, true, subReportNode);
        // add switch with the new bar
        String voltageLevelId = voltageLevel.getId();
        voltageLevel.getConnectableStream().filter(connectable -> connectable.getType() != IdentifiableType.BUSBAR_SECTION).forEach(conn -> {
            Terminal terminalConnectedToVl = (Terminal) conn.getTerminals().stream()
                .filter(terminal -> ((Terminal) terminal).getVoltageLevel().getId().equals(voltageLevelId))
                .findFirst().get();
            String busbarSectionId = ModificationUtils.getInstance().getBusOrBusbarSection(terminalConnectedToVl);
            new MoveFeederBayBuilder().withConnectableId(conn.getId())
                .withTargetVoltageLevelId(voltageLevelId)
                .withTerminal(terminalConnectedToVl)
                .withTargetBusOrBusBarSectionId(busbarSectionId)
                .build().apply(network, false, ReportNode.NO_OP);
        });
    }

    private int findLowBusOrBusbarIndex(VoltageLevel voltageLevel) {
        List<BusbarSection> busbarSections = voltageLevel.getNodeBreakerView()
            .getBusbarSectionStream().toList();
        List<BusbarSectionPosition> busbarExtensions = busbarSections.stream()
            .map(busbarSection -> (BusbarSectionPosition) busbarSection.getExtension(BusbarSectionPosition.class))
            .filter(Objects::nonNull)
            .toList();
        if (!busbarSections.isEmpty() && busbarExtensions.isEmpty()) {
            throw new PowsyblException("busbar section position extension are not available, can not create a new busbar section");
        }
        Optional<BusbarSectionPosition> maxBusBarSectionPosition = busbarExtensions.stream()
            .max(Comparator.comparingInt(BusbarSectionPosition::getBusbarIndex));
        return maxBusBarSectionPosition.map(busbarSectionPosition -> busbarSectionPosition.getBusbarIndex() + 1).orElse(1);
    }

    @Override
    public String getName() {
        return ModificationType.CREATE_VOLTAGE_LEVEL_TOPOLOGY.toString();
    }
}
