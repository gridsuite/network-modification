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
import org.gridsuite.modification.model.EquipmentDeletionModel;
import org.gridsuite.modification.model.HvdcLccDeletionModel;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class EquipmentDeletion extends AbstractModification {

    private final EquipmentDeletionModel modificationModel;

    public EquipmentDeletion(EquipmentDeletionModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Identifiable<?> identifiable = ModificationUtils.getInstance().getEquipmentByIdentifiableType(network, modificationModel.getEquipmentType(), modificationModel.getEquipmentId());
        if (identifiable == null) {
            throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=" + modificationModel.getEquipmentId() + " not found or of bad type");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Identifiable<?> identifiable = ModificationUtils.getInstance().getEquipmentByIdentifiableType(network, modificationModel.getEquipmentType(), modificationModel.getEquipmentId());

        // Report node is pushed to network instance to allow deletion logs from other libraries to be added
        network.getReportNodeContext().pushReportNode(subReportNode);
        if (identifiable instanceof Connectable) {
            new RemoveFeederBay(modificationModel.getEquipmentId()).apply(network, true, subReportNode);
        } else if (identifiable instanceof HvdcLine) {
            removeHvdcLine(network, subReportNode);
        } else if (identifiable instanceof VoltageLevel) {
            new RemoveVoltageLevel(modificationModel.getEquipmentId()).apply(network, true, subReportNode);
        } else if (identifiable instanceof Substation) {
            RemoveSubstation rs = new RemoveSubstationBuilder().withSubstationId(modificationModel.getEquipmentId()).build();
            rs.apply(network, true, subReportNode);
        }
        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.equipmentDeleted")
            .withUntypedValue("type", modificationModel.getEquipmentType().name())
            .withUntypedValue("id", modificationModel.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
    }

    @Override
    public String getName() {
        return "EquipmentDeletion";
    }

    private void removeHvdcLine(Network network, ReportNode subReportNode) {
        HvdcLccDeletionModel specificModel = (HvdcLccDeletionModel) modificationModel.getEquipmentInfos();
        List<String> shuntCompensatorIds = List.of();
        if (specificModel != null) {
            shuntCompensatorIds = Stream.concat(
                    specificModel.getMcsOnSide1() != null ? specificModel.getMcsOnSide1().stream() : Stream.of(),
                    specificModel.getMcsOnSide2() != null ? specificModel.getMcsOnSide2().stream() : Stream.of())
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
                .map(HvdcLccDeletionModel.ShuntCompensatorModel::getId)
                .collect(Collectors.toList());
        }
        RemoveHvdcLine algo = new RemoveHvdcLineBuilder()
            .withHvdcLineId(modificationModel.getEquipmentId())
            .withShuntCompensatorIds(shuntCompensatorIds)
            .build();
        algo.apply(network, true, subReportNode);
    }
}
