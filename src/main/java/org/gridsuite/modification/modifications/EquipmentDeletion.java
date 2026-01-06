/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.*;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.dto.HvdcLccDeletionInfos;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class EquipmentDeletion extends AbstractModification {

    private final EquipmentDeletionInfos modificationInfos;

    public EquipmentDeletion(EquipmentDeletionInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) {
        Identifiable<?> identifiable = ModificationUtils.getInstance().getEquipmentByIdentifiableType(network, modificationInfos.getEquipmentType(), modificationInfos.getEquipmentId());
        if (identifiable == null) {
            throw new NetworkModificationException(modificationInfos.getEquipmentType() + " with id :" + modificationInfos.getEquipmentId() + " not found");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Identifiable<?> identifiable = ModificationUtils.getInstance().getEquipmentByIdentifiableType(network, modificationInfos.getEquipmentType(), modificationInfos.getEquipmentId());

        // Report node is pushed to network instance to allow deletion logs from other libraries to be added
        network.getReportNodeContext().pushReportNode(subReportNode);
        if (identifiable instanceof Connectable) {
            new RemoveFeederBay(modificationInfos.getEquipmentId()).apply(network, true, subReportNode);
        } else if (identifiable instanceof HvdcLine) {
            removeHvdcLine(network, subReportNode);
        } else if (identifiable instanceof VoltageLevel) {
            new RemoveVoltageLevel(modificationInfos.getEquipmentId()).apply(network, true, subReportNode);
        } else if (identifiable instanceof Substation) {
            RemoveSubstation rs = new RemoveSubstationBuilder().withSubstationId(modificationInfos.getEquipmentId()).build();
            rs.apply(network, true, subReportNode);
        }
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.equipmentDeleted")
                .withUntypedValue("type", modificationInfos.getEquipmentType().name())
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    @Override
    public String getName() {
        return "EquipmentDeletion";
    }

    private void removeHvdcLine(Network network, ReportNode subReportNode) {
        HvdcLccDeletionInfos specificInfos = (HvdcLccDeletionInfos) modificationInfos.getEquipmentInfos();
        List<String> shuntCompensatorIds = List.of();
        if (specificInfos != null) {
            shuntCompensatorIds = Stream.concat(
                            specificInfos.getMcsOnSide1() != null ? specificInfos.getMcsOnSide1().stream() : Stream.of(),
                            specificInfos.getMcsOnSide2() != null ? specificInfos.getMcsOnSide2().stream() : Stream.of())
                    .filter(mcsInfo -> {
                        // isConnectedToHvdc means: selected to be removed (can be changed by the Front)
                        if (mcsInfo.isConnectedToHvdc() && network.getShuntCompensator(mcsInfo.getId()) == null) {
                            subReportNode.newReportNode()
                                    .withMessageTemplate("network.modification.shuntCompensatorNotDeleted")
                                    .withUntypedValue("id", mcsInfo.getId())
                                    .withSeverity(TypedValue.WARN_SEVERITY)
                                    .add();
                            return false;
                        } else {
                            return mcsInfo.isConnectedToHvdc();
                        }
                    })
                    .map(HvdcLccDeletionInfos.ShuntCompensatorInfos::getId)
                    .collect(Collectors.toList());
        }
        RemoveHvdcLine algo = new RemoveHvdcLineBuilder()
                .withHvdcLineId(modificationInfos.getEquipmentId())
                .withShuntCompensatorIds(shuntCompensatorIds)
                .build();
        algo.apply(network, true, subReportNode);
    }
}
