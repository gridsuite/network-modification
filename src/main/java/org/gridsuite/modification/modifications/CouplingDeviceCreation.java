/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.CreateCouplingDevice;
import com.powsybl.iidm.modification.topology.CreateCouplingDeviceBuilder;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CouplingDeviceCreationInfos;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class CouplingDeviceCreation extends AbstractModification {

    private final CouplingDeviceCreationInfos creationInfos;

    public CouplingDeviceCreation(CouplingDeviceCreationInfos creationInfos) {
        this.creationInfos = creationInfos;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        CreateCouplingDeviceBuilder builder = new CreateCouplingDeviceBuilder();
        CreateCouplingDevice createCouplingDevice = builder
            .withBusOrBusbarSectionId1(creationInfos.getBusOrBbsId1())
            .withBusOrBusbarSectionId2(creationInfos.getBusOrBbsId2())
            .withSwitchPrefixId(creationInfos.getSwitchPrefixId())
            .build();
        createCouplingDevice.apply(network, true, subReportNode);
    }

    @Override
    public String getName() {
        return ModificationType.COUPLING_DEVICE_CREATION.toString();
    }
}
