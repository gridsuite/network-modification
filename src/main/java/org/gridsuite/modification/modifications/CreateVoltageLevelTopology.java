/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CreateVoltageLevelTopologyInfos;

import java.util.ArrayList;
import java.util.List;

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
        List<SwitchKind> switchKinds = new ArrayList<>();
        for (int i = 0; i < createVoltageLevelTopologyInfos.getSectionCount() - 1; i++) {
            switchKinds.add(SwitchKind.BREAKER);
        }
        new com.powsybl.iidm.modification.topology.CreateVoltageLevelTopologyBuilder()
            .withVoltageLevelId(createVoltageLevelTopologyInfos.getVoltageLevelId())
            .withSectionCount(createVoltageLevelTopologyInfos.getSectionCount())
            .withAlignedBusesOrBusbarCount(createVoltageLevelTopologyInfos.getAlignedBusesOrBusbarCount())
            .withSwitchKinds(switchKinds)
            .build().apply(network, true, subReportNode);
    }

    @Override
    public String getName() {
        return ModificationType.CREATE_VOLTAGE_LEVEL_TOPOLOGY.toString();
    }
}
