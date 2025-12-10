/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.BusbarSectionPosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.CreateVoltageLevelTopologyInfos;

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
    public void check(Network network) {
        if (createVoltageLevelTopologyInfos == null
            || createVoltageLevelTopologyInfos.getVoltageLevelId() == null
            || createVoltageLevelTopologyInfos.getSwitchKinds() == null
            || createVoltageLevelTopologyInfos.getSectionCount() == null) {
            throw new NetworkModificationRunException("Missing required attributes to modify the equipment");
        }
        if (createVoltageLevelTopologyInfos.getSwitchKinds().size() != createVoltageLevelTopologyInfos.getSectionCount() - 1) {
            throw new NetworkModificationRunException("The switch kinds list must have a size equal to the section count minus one");
        }
        if (network.getVoltageLevel(createVoltageLevelTopologyInfos.getVoltageLevelId()) == null) {
            throw new NetworkModificationRunException("voltage level " +
                createVoltageLevelTopologyInfos.getVoltageLevelId() + " is not found");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevel voltageLevel = network.getVoltageLevel(createVoltageLevelTopologyInfos.getVoltageLevelId());
        createVoltageLevelBusBarSection(network, subReportNode, voltageLevel);
    }

    private void createVoltageLevelBusBarSection(Network network, ReportNode subReportNode, VoltageLevel voltageLevel) {
        int lowBusOrBusbarIndex = findLowBusOrBusbarIndex(voltageLevel);
        new com.powsybl.iidm.modification.topology.CreateVoltageLevelTopologyBuilder()
            .withVoltageLevelId(createVoltageLevelTopologyInfos.getVoltageLevelId())
            .withSectionCount(createVoltageLevelTopologyInfos.getSectionCount())
            .withAlignedBusesOrBusbarCount(1)
            .withLowBusOrBusbarIndex(lowBusOrBusbarIndex)
            .withSwitchKinds(createVoltageLevelTopologyInfos.getSwitchKinds())
            .withConnectExistingConnectables(true)
            .build().apply(network, true, subReportNode);
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
