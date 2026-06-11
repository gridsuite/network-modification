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
import org.gridsuite.modification.model.ShuntCompensatorCreationModel;
import org.gridsuite.modification.model.ShuntCompensatorType;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_SHUNT_COMPENSATOR_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.SHUNT_COMPENSATOR_ALREADY_EXISTS;
import static org.gridsuite.modification.utils.ModificationUtils.createInjectionInNodeBreaker;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class ShuntCompensatorCreation extends AbstractModification {

    private final ShuntCompensatorCreationModel modificationModel;

    public ShuntCompensatorCreation(ShuntCompensatorCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getShuntCompensator(modificationModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(SHUNT_COMPENSATOR_ALREADY_EXISTS, modificationModel.getEquipmentId());
        }

        if (modificationModel.getMaximumSectionCount() < 1) {
            throw new NetworkModificationException(CREATE_SHUNT_COMPENSATOR_ERROR, "Maximum section count should be greater or equal to 1");
        }

        if (modificationModel.getSectionCount() < 0 || modificationModel.getSectionCount() > modificationModel.getMaximumSectionCount()) {
            throw new NetworkModificationException(CREATE_SHUNT_COMPENSATOR_ERROR, String.format("Section count should be between 0 and Maximum section count (%d), actual : %d",
                modificationModel.getMaximumSectionCount(),
                modificationModel.getSectionCount()));
        }
        ModificationUtils.getInstance().controlConnectivity(network, modificationModel.getVoltageLevelId(),
            modificationModel.getBusOrBusbarSectionId());
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the shunt compensator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId());
        if (modificationModel.getMaxQAtNominalV() != null && modificationModel.getShuntCompensatorType() != null) {
            double maxSusceptance = (modificationModel.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2);
            modificationModel.setMaxSusceptance(
                modificationModel.getShuntCompensatorType() == ShuntCompensatorType.CAPACITOR
                    ? maxSusceptance
                    : -maxSusceptance);
        }
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            ShuntCompensatorAdder shuntCompensatorAdder = createShuntAdderInNodeBreaker(voltageLevel, modificationModel);
            createInjectionInNodeBreaker(voltageLevel, modificationModel, network, shuntCompensatorAdder, subReportNode);
        } else {
            createShuntInBusBreaker(voltageLevel, modificationModel);
            subReportNode.newReportNode()
                .withMessageTemplate("network.modification.shuntCompensatorCreated")
                .withUntypedValue("id", modificationModel.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        }
        ModificationUtils.getInstance().disconnectCreatedInjection(modificationModel, network.getShuntCompensator(modificationModel.getEquipmentId()), subReportNode);
        // properties
        ShuntCompensator shuntCompensator = network.getShuntCompensator(modificationModel.getEquipmentId());
        PropertiesUtils.applyProperties(shuntCompensator, subReportNode, modificationModel.getProperties(), "network.modification.ShuntCompensatorProperties");
    }

    @Override
    public String getName() {
        return "ShuntCompensatorCreation";
    }

    private ShuntCompensatorAdder createShuntAdderInNodeBreaker(VoltageLevel voltageLevel, ShuntCompensatorCreationModel shuntCompensatorModel) {
        // creating the shunt compensator
        ShuntCompensatorAdder shuntAdder = voltageLevel.newShuntCompensator()
            .setId(shuntCompensatorModel.getEquipmentId())
            .setName(shuntCompensatorModel.getEquipmentName())
            .setSectionCount(shuntCompensatorModel.getSectionCount());

        /* when we create non-linear shunt, this is where we branch ;) */
        shuntAdder.newLinearModel()
            .setBPerSection(shuntCompensatorModel.getMaxSusceptance() / shuntCompensatorModel.getMaximumSectionCount())
            .setMaximumSectionCount(shuntCompensatorModel.getMaximumSectionCount())
            .add();

        return shuntAdder;
    }

    private void createShuntInBusBreaker(VoltageLevel voltageLevel, ShuntCompensatorCreationModel shuntCompensatorModel) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, shuntCompensatorModel.getBusOrBusbarSectionId());
        /* creating the shunt compensator */
        voltageLevel.newShuntCompensator()
            .setId(shuntCompensatorModel.getEquipmentId())
            .setName(shuntCompensatorModel.getEquipmentName())
            .setSectionCount(shuntCompensatorModel.getSectionCount())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .newLinearModel()
            .setBPerSection(shuntCompensatorModel.getMaxSusceptance() / shuntCompensatorModel.getMaximumSectionCount())
            .setMaximumSectionCount(shuntCompensatorModel.getMaximumSectionCount())
            .add()
            .add();
    }
}
