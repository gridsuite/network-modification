/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.MoveFeederBayBuilder;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BusbarSectionPosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CreateVoltageLevelTopologyInfos;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class CreateVoltageLevelTopology extends AbstractModification {

    private final CreateVoltageLevelTopologyInfos createVoltageLevelTopologyInfos;

    public CreateVoltageLevelTopology(CreateVoltageLevelTopologyInfos createVoltageLevelTopology) {
        this.createVoltageLevelTopologyInfos = createVoltageLevelTopology;
    }

    @Override
    public void check(Network network) {
        if (createVoltageLevelTopologyInfos == null
            || createVoltageLevelTopologyInfos.getVoltageLevelId() == null
            || createVoltageLevelTopologyInfos.getSwitchKinds() == null
            || createVoltageLevelTopologyInfos.getSectionCount() == null) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR, "Missing required attributes to modify the equipment");
        }
        if (createVoltageLevelTopologyInfos.getSwitchKinds().size() != createVoltageLevelTopologyInfos.getSectionCount() - 1) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR, "The switch kinds list must have a size equal to the section count minus one");
        }
        if (network.getVoltageLevel(createVoltageLevelTopologyInfos.getVoltageLevelId()) == null) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR, "voltage level " +
                createVoltageLevelTopologyInfos.getVoltageLevelId() + " is not found");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        VoltageLevel voltageLevel = network.getVoltageLevel(createVoltageLevelTopologyInfos.getVoltageLevelId());
        createVoltageLevelBusBarSection(network, namingStrategy, subReportNode, voltageLevel);
        moveConnectableToAddSwitchesOnTheNewBar(network, voltageLevel);
    }

    private void createVoltageLevelBusBarSection(Network network, NamingStrategy namingStrategy, ReportNode subReportNode, VoltageLevel voltageLevel) {
        int lowBusOrBusbarIndex = findLowBusOrBusbarIndex(voltageLevel);
        new com.powsybl.iidm.modification.topology.CreateVoltageLevelTopologyBuilder()
            .withVoltageLevelId(createVoltageLevelTopologyInfos.getVoltageLevelId())
            .withSectionCount(createVoltageLevelTopologyInfos.getSectionCount())
            .withAlignedBusesOrBusbarCount(1)
            .withLowBusOrBusbarIndex(lowBusOrBusbarIndex)
            .withSwitchKinds(createVoltageLevelTopologyInfos.getSwitchKinds())
            .build().apply(network, namingStrategy, true, subReportNode);
    }

    private void moveConnectableToAddSwitchesOnTheNewBar(Network network, VoltageLevel voltageLevel) {
        String voltageLevelId = voltageLevel.getId();
        voltageLevel.getConnectableStream().filter(connectable -> connectable.getType() != IdentifiableType.BUSBAR_SECTION).forEach(conn -> {
            Optional<Terminal> terminalConnectedToVl = conn.getTerminals().stream()
                .filter(terminal -> ((Terminal) terminal).getVoltageLevel().getId().equals(voltageLevelId))
                .findFirst();
            if (terminalConnectedToVl.isPresent()) {
                String busbarSectionId = ModificationUtils.getInstance().getBusOrBusbarSection(terminalConnectedToVl.get());
                new MoveFeederBayBuilder().withConnectableId(conn.getId())
                    .withTargetVoltageLevelId(voltageLevelId)
                    .withTerminal(terminalConnectedToVl.get())
                    .withTargetBusOrBusBarSectionId(busbarSectionId)
                    .build().apply(network, false, ReportNode.NO_OP);
            }
        });
    }

    private int findLowBusOrBusbarIndex(VoltageLevel voltageLevel) {
        List<BusbarSection> busbarSections = voltageLevel.getNodeBreakerView()
            .getBusbarSectionStream().toList();
        List<BusbarSectionPosition> busbarExtensions = busbarSections.stream()
            .map(busbarSection -> busbarSection.getExtension(BusbarSectionPosition.class))
            .filter(Objects::nonNull)
            .map(BusbarSectionPosition.class::cast)
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
