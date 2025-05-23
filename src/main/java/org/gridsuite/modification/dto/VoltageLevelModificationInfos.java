/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.VoltageLevelModification;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Voltage level modification")
@JsonTypeName("VOLTAGE_LEVEL_MODIFICATION")
@ModificationErrorTypeName("MODIFY_VOLTAGE_LEVEL_ERROR")
public class VoltageLevelModificationInfos extends BasicEquipmentModificationInfos {
    @Schema(description = "nominal voltage in kV")
    private AttributeModification<Double> nominalV;

    @Schema(description = "low voltage limit in kV")
    private AttributeModification<Double> lowVoltageLimit;

    @Schema(description = "high voltage limit  in kV")
    private AttributeModification<Double> highVoltageLimit;

    @Schema(description = "low short-circuit current limit in A")
    private AttributeModification<Double> ipMin;

    @Schema(description = "high short-circuit current limit in A")
    private AttributeModification<Double> ipMax;

    @Override
    public AbstractModification toModification() {
        return new VoltageLevelModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevel.modification")
                .withUntypedValue("voltageLevelId", getEquipmentId())
                .add();
    }

}
