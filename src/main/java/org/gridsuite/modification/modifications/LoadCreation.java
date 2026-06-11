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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.LoadCreationModel;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.LOAD_ALREADY_EXISTS;
import static org.gridsuite.modification.utils.ModificationUtils.createInjectionInNodeBreaker;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LoadCreation extends AbstractModification {

    private final LoadCreationModel modificationModel;

    public LoadCreation(LoadCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getLoad(modificationModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(LOAD_ALREADY_EXISTS, modificationModel.getEquipmentId());
        }
        ModificationUtils.getInstance().controlConnectivity(network, modificationModel.getVoltageLevelId(),
            modificationModel.getBusOrBusbarSectionId());
    }

    @Override
    public void apply(Network network, ReportNode subReporter) {
        // create the load in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LoadAdder loadAdder = createLoadAdderInNodeBreaker(voltageLevel, modificationModel);
            createInjectionInNodeBreaker(voltageLevel, modificationModel, network, loadAdder, subReporter);
        } else {
            createLoadInBusBreaker(voltageLevel, modificationModel);
            subReporter.newReportNode()
                .withMessageTemplate("network.modification.loadCreated")
                .withUntypedValue("id", modificationModel.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        }
        reportElementaryCreations(subReporter);
        ModificationUtils.getInstance().disconnectCreatedInjection(modificationModel, network.getLoad(modificationModel.getEquipmentId()), subReporter);

        // properties
        Load load = network.getLoad(modificationModel.getEquipmentId());
        PropertiesUtils.applyProperties(load, subReporter, modificationModel.getProperties(), "network.modification.LoadProperties");
    }

    @Override
    public String getName() {
        return "LoadCreation";
    }

    private void reportElementaryCreations(ReportNode subReportNode) {
        if (modificationModel.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                .reportElementaryCreation(subReportNode, modificationModel.getEquipmentName(), "Name");
        }

        if (modificationModel.getLoadType() != null) {
            ModificationUtils.getInstance()
                .reportElementaryCreation(subReportNode, modificationModel.getLoadType(), "Type");
        }

        ModificationUtils.getInstance()
            .reportElementaryCreation(subReportNode, modificationModel.getP0(), "Active power");

        ModificationUtils.getInstance()
            .reportElementaryCreation(subReportNode, modificationModel.getQ0(), "Reactive power");
    }

    private LoadAdder createLoadAdderInNodeBreaker(VoltageLevel voltageLevel, LoadCreationModel loadCreationModel) {
        // creating the load adder
        return voltageLevel.newLoad()
            .setId(loadCreationModel.getEquipmentId())
            .setName(loadCreationModel.getEquipmentName())
            .setLoadType(loadCreationModel.getLoadType())
            .setP0(loadCreationModel.getP0())
            .setQ0(loadCreationModel.getQ0());
    }

    private Load createLoadInBusBreaker(VoltageLevel voltageLevel, LoadCreationModel loadCreationModel) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, loadCreationModel.getBusOrBusbarSectionId());

        // creating the load
        return voltageLevel.newLoad()
            .setId(loadCreationModel.getEquipmentId())
            .setName(loadCreationModel.getEquipmentName())
            .setLoadType(loadCreationModel.getLoadType())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setP0(loadCreationModel.getP0())
            .setQ0(loadCreationModel.getQ0()).add();
    }
}
