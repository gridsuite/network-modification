/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.ModificationType;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@NoArgsConstructor
@Getter
@AllArgsConstructor
@Builder
public class CreateCouplingDevice extends AbstractModification {

    private String voltageLevelId;
    private String busbarSectionId1;
    private String busbarSectionId2;

    /**
     * @param network
     * @param subReportNode
     */
    @Override
    public void apply(Network network, ReportNode subReportNode) {
        new com.powsybl.iidm.modification.topology.CreateCouplingDeviceBuilder()
            .withBusOrBusbarSectionId1(busbarSectionId1)
            .withBusOrBusbarSectionId2(busbarSectionId2)
            .build().apply(network, false, subReportNode);
    }

    @Override
    public String getName() {
        return ModificationType.CREATE_COUPLING_DEVICE.toString();
    }
}
