/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.LOAD_ALREADY_EXISTS;
import static org.gridsuite.modification.utils.ModificationUtils.createInjectionInNodeBreaker;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Getter
@Setter
public class LoadCreation extends AbstractInjectionCreation {

    private LoadType loadType;
    private double p0;
    private double q0;

    @Builder
    public LoadCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName,
                        String voltageLevelId,
                        String busOrBusbarSectionId, String connectionName,
                        ConnectablePosition.Direction connectionDirection, Integer connectionPosition,
                        boolean terminalConnected, LoadType loadType, double p0, double q0) {
        super(equipmentId, properties, equipmentName, voltageLevelId, busOrBusbarSectionId, connectionName,
            connectionDirection, connectionPosition, terminalConnected);
        this.loadType = loadType;
        this.p0 = p0;
        this.q0 = q0;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getLoad(equipmentId) != null) {
            throw new NetworkModificationException(LOAD_ALREADY_EXISTS, equipmentId);
        }
        ModificationUtils.getInstance().controlConnectivity(network, voltageLevelId,
                busOrBusbarSectionId);
    }

    @Override
    public void apply(Network network, ReportNode subReporter) {
        // create the load in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId);
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LoadAdder loadAdder = createLoadAdderInNodeBreaker(voltageLevel);
            createInjectionInNodeBreaker(voltageLevel, equipmentId, busOrBusbarSectionId, connectionName, connectionDirection, connectionPosition, network, loadAdder, subReporter);
        } else {
            createLoadInBusBreaker(voltageLevel);
            subReporter.newReportNode()
                .withMessageTemplate("network.modification.loadCreated")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        }
        reportElementaryCreations(subReporter);
        ModificationUtils.getInstance().disconnectCreatedInjection(terminalConnected, equipmentId, network.getLoad(equipmentId), subReporter);

        // properties
        Load load = network.getLoad(equipmentId);
        PropertiesUtils.applyProperties(load, subReporter, properties, "network.modification.LoadProperties");
    }

    @Override
    public String getName() {
        return "LoadCreation";
    }

    private void reportElementaryCreations(ReportNode subReportNode) {
        if (equipmentName != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, equipmentName, "Name");
        }

        if (loadType != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, loadType, "Type");
        }

        ModificationUtils.getInstance()
                .reportElementaryCreation(subReportNode, p0, "Active power");

        ModificationUtils.getInstance()
                .reportElementaryCreation(subReportNode, q0, "Reactive power");
    }

    private LoadAdder createLoadAdderInNodeBreaker(VoltageLevel voltageLevel) {
        // creating the load adder
        return voltageLevel.newLoad()
            .setId(equipmentId)
            .setName(equipmentName)
            .setLoadType(loadType)
            .setP0(p0)
            .setQ0(q0);
    }

    private Load createLoadInBusBreaker(VoltageLevel voltageLevel) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, busOrBusbarSectionId);

        // creating the load
        return voltageLevel.newLoad()
            .setId(equipmentId)
            .setName(equipmentName)
            .setLoadType(loadType)
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setP0(p0)
            .setQ0(q0).add();
    }

}
