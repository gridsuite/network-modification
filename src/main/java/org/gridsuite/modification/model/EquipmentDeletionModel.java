/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.EquipmentDeletion;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Equipment deletion")
@ModificationErrorTypeName("DELETE_EQUIPMENT_ERROR")
public class EquipmentDeletionModel extends EquipmentModificationModel implements ModificationModel {
    @Schema(description = "Equipment type")
    private IdentifiableType equipmentType;

    @Schema(description = "Equipment specific infos (optional)")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private AbstractEquipmentDeletionModel equipmentInfos;

    @Override
    public AbstractModification toModification() {
        return new EquipmentDeletion(this);
    }

    @Override
    public ModificationType getType() {
        return ModificationType.EQUIPMENT_DELETION;
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.equipmentDeletion")
                .withUntypedValue("equipmentId", this.getEquipmentId())
                .add();
    }
}
