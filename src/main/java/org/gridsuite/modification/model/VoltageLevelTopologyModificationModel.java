/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.model;

import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.VoltageLevelTopologyModification;

import java.util.List;

/**
 * @author REHILI Ghazwa <ghazwarhili@gmail.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Voltage level topology modification")
@ModificationErrorTypeName("MODIFY_VOLTAGE_LEVEL_TOPOLOGY_ERROR")
public class VoltageLevelTopologyModificationModel extends EquipmentModificationModel implements ModificationModel {

    @Schema(description = "Switch attribute modification list")
    private List<EquipmentAttributeModificationModel> equipmentAttributeModificationList;

    @Override
    public AbstractModification toModification() {
        return new VoltageLevelTopologyModification(this);
    }

    @Override
    public ModificationType getType() {
        return ModificationType.VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION;
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION")
                .withUntypedValue("voltageLevelId", getEquipmentId())
                .add();
    }
}
