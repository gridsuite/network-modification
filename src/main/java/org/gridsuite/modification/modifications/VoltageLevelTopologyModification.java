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
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelTopologyModificationInfos;
import org.gridsuite.modification.utils.ModificationUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;

/**
 * @author REHILI Ghazwa <ghazwarhili@gmail.com>
 */

public class VoltageLevelTopologyModification extends AbstractModification {
    private final VoltageLevelTopologyModificationInfos modificationInfos;

    public VoltageLevelTopologyModification(VoltageLevelTopologyModificationInfos voltageLevelTopologyModificationInfos) {
        this.modificationInfos = voltageLevelTopologyModificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (!modificationInfos.getEquipmentAttributeModification().isEmpty()) {
            // Check the voltage level exists in the network
            VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getEquipmentId());
            if (voltageLevel == null) {
                throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, modificationInfos.getEquipmentId());
            }

            // Check each equipment attribute modification
            for (EquipmentAttributeModificationInfos equipmentModInfo : modificationInfos.getEquipmentAttributeModification()) {
                // Use the check method from EquipmentAttributeModification
                EquipmentAttributeModification equipmentMod = new EquipmentAttributeModification(equipmentModInfo);
                equipmentMod.check(network);
            }
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        for (EquipmentAttributeModificationInfos equipmentModInfo : modificationInfos.getEquipmentAttributeModification()) {
            EquipmentAttributeModification equipmentMod = new EquipmentAttributeModification(equipmentModInfo);
            equipmentMod.apply(network, subReportNode);
        }
        subReportNode.newReportNode()
                .withMessageTemplate("voltageLevelTopologyModified", "Voltage level '${id}' topology has been modified")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    @Override
    public String getName() {
        return ModificationType.VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION.toString();
    }
}

