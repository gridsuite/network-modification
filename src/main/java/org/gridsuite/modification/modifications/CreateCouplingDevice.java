/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.CreateCouplingDeviceBuilder;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CreateCouplingDeviceInfos;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class CreateCouplingDevice extends AbstractModification {

    private final CreateCouplingDeviceInfos createCouplingDeviceInfos;

    public CreateCouplingDevice(CreateCouplingDeviceInfos createCouplingDeviceInfos) {
        this.createCouplingDeviceInfos = createCouplingDeviceInfos;
    }

    /**
     * @param network
     * @param subReportNode
     */
    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        new CreateCouplingDeviceBuilder()
            .withBusOrBusbarSectionId1(createCouplingDeviceInfos.getCouplingDeviceInfos().getBusbarSectionId1())
            .withBusOrBusbarSectionId2(createCouplingDeviceInfos.getCouplingDeviceInfos().getBusbarSectionId2())
            .build().apply(network, namingStrategy, subReportNode);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public String getName() {
        return ModificationType.CREATE_COUPLING_DEVICE.toString();
    }
}
