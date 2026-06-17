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
import org.gridsuite.modification.dto.ShuntCompensatorType;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_SHUNT_COMPENSATOR_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.SHUNT_COMPENSATOR_ALREADY_EXISTS;
import static org.gridsuite.modification.utils.ModificationUtils.createInjectionInNodeBreaker;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Getter
@Setter
public class ShuntCompensatorCreation extends AbstractInjectionCreation {

    private Integer maximumSectionCount;
    private Integer sectionCount;
    private Double maxSusceptance;
    private Double maxQAtNominalV;
    private ShuntCompensatorType shuntCompensatorType;

    @Builder
    public ShuntCompensatorCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName,
                                    String voltageLevelId, String busOrBusbarSectionId, String connectionName,
                                    ConnectablePosition.Direction connectionDirection, Integer connectionPosition,
                                    boolean terminalConnected, Integer maximumSectionCount, Integer sectionCount,
                                    Double maxSusceptance, Double maxQAtNominalV,
                                    ShuntCompensatorType shuntCompensatorType) {
        super(equipmentId, properties, equipmentName, voltageLevelId, busOrBusbarSectionId, connectionName,
            connectionDirection, connectionPosition, terminalConnected);
        this.maximumSectionCount = maximumSectionCount;
        this.sectionCount = sectionCount;
        this.maxSusceptance = maxSusceptance;
        this.maxQAtNominalV = maxQAtNominalV;
        this.shuntCompensatorType = shuntCompensatorType;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getShuntCompensator(equipmentId) != null) {
            throw new NetworkModificationException(SHUNT_COMPENSATOR_ALREADY_EXISTS, equipmentId);
        }

        if (maximumSectionCount < 1) {
            throw new NetworkModificationException(CREATE_SHUNT_COMPENSATOR_ERROR, "Maximum section count should be greater or equal to 1");
        }

        if (sectionCount < 0 || sectionCount > maximumSectionCount) {
            throw new NetworkModificationException(CREATE_SHUNT_COMPENSATOR_ERROR, String.format("Section count should be between 0 and Maximum section count (%d), actual : %d",
                    maximumSectionCount,
                    sectionCount));
        }
        ModificationUtils.getInstance().controlConnectivity(network, voltageLevelId,
                busOrBusbarSectionId);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the shunt compensator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId);
        if (maxQAtNominalV != null && shuntCompensatorType != null) {
            double computedMaxSusceptance = maxQAtNominalV / Math.pow(voltageLevel.getNominalV(), 2);
            maxSusceptance = shuntCompensatorType == ShuntCompensatorType.CAPACITOR
                            ? computedMaxSusceptance
                            : -computedMaxSusceptance;
        }
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            ShuntCompensatorAdder shuntCompensatorAdder = createShuntAdderInNodeBreaker(voltageLevel);
            createInjectionInNodeBreaker(voltageLevel, this, network, shuntCompensatorAdder, subReportNode);
        } else {
            createShuntInBusBreaker(voltageLevel);
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.shuntCompensatorCreated")
                    .withUntypedValue("id", equipmentId)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        ModificationUtils.getInstance().disconnectCreatedInjection(this, network.getShuntCompensator(equipmentId), subReportNode);
        // properties
        ShuntCompensator shuntCompensator = network.getShuntCompensator(equipmentId);
        PropertiesUtils.applyProperties(shuntCompensator, subReportNode, properties, "network.modification.ShuntCompensatorProperties");
    }

    @Override
    public String getName() {
        return "ShuntCompensatorCreation";
    }

    private ShuntCompensatorAdder createShuntAdderInNodeBreaker(VoltageLevel voltageLevel) {
        // creating the shunt compensator
        ShuntCompensatorAdder shuntAdder = voltageLevel.newShuntCompensator()
                .setId(equipmentId)
                .setName(equipmentName)
                .setSectionCount(sectionCount);

        /* when we create non-linear shunt, this is where we branch ;) */
        shuntAdder.newLinearModel()
                .setBPerSection(maxSusceptance / maximumSectionCount)
                .setMaximumSectionCount(maximumSectionCount)
                .add();

        return shuntAdder;
    }

    private void createShuntInBusBreaker(VoltageLevel voltageLevel) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, busOrBusbarSectionId);
        /* creating the shunt compensator */
        voltageLevel.newShuntCompensator()
            .setId(equipmentId)
            .setName(equipmentName)
            .setSectionCount(sectionCount)
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .newLinearModel()
            .setBPerSection(maxSusceptance / maximumSectionCount)
            .setMaximumSectionCount(maximumSectionCount)
            .add()
            .add();
    }

}
