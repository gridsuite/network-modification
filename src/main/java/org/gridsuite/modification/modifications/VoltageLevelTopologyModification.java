/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.EquipmentAttributeModificationModel;
import org.gridsuite.modification.model.VoltageLevelTopologyModificationModel;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_VOLTAGE_LEVEL_TOPOLOGY_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;

/**
 * @author REHILI Ghazwa <ghazwarhili@gmail.com>
 */

public class VoltageLevelTopologyModification extends AbstractModification {
    private final VoltageLevelTopologyModificationModel modificationModel;

    public VoltageLevelTopologyModification(VoltageLevelTopologyModificationModel voltageLevelTopologyModificationModel) {
        this.modificationModel = voltageLevelTopologyModificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationModel.getEquipmentId());
        if (voltageLevel == null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, modificationModel.getEquipmentId());
        }
        if (!modificationModel.getEquipmentAttributeModificationList().isEmpty()) {
            for (EquipmentAttributeModificationModel equipmentAttributeModificationModel : modificationModel.getEquipmentAttributeModificationList()) {
                EquipmentAttributeModification equipmentAttributeModification = new EquipmentAttributeModification(equipmentAttributeModificationModel);
                equipmentAttributeModification.check(network);
            }
        } else {
            throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_TOPOLOGY_ERROR, "Missing required switches to modify the voltage level topology");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        for (EquipmentAttributeModificationModel equipmentAttributeModificationModel : modificationModel.getEquipmentAttributeModificationList()) {
            EquipmentAttributeModification equipmentAttributeModification = new EquipmentAttributeModification(equipmentAttributeModificationModel);
            equipmentAttributeModification.apply(network, subReportNode);
        }
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevelTopologyModified")
                .withUntypedValue("id", modificationModel.getEquipmentId())
                .withSeverity(TypedValue.DEBUG_SEVERITY)
                .add();
    }

    @Override
    public String getName() {
        return ModificationType.VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION.toString();
    }
}

