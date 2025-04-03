/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.HvdcLine;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.LccModification;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "LCC modification")
@JsonTypeName("LCC_MODIFICATION")
@ModificationErrorTypeName("MODIFY_LCC_ERROR")
public class LccModificationInfos extends BasicEquipmentModificationInfos {
    @Schema(description = "DC nominal voltage")
    private AttributeModification<Double> nominalV;

    @Schema(description = "DC resistance")
    private AttributeModification<Double> r;

    @Schema(description = "Maximum active power")
    private AttributeModification<Double> maxP;

    @Schema(description = "Converters mode")
    private AttributeModification<HvdcLine.ConvertersMode> convertersMode;

    @Schema(description = "Active power setpoint")
    private AttributeModification<Double> activePowerSetpoint;

    @Schema(description = "Converter station 1")
    private LccConverterStationModificationInfos converterStation1;

    @Schema(description = "Converter station 2")
    private LccConverterStationModificationInfos converterStation2;

    @Override
    public AbstractModification toModification() {
        return new LccModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
            .withMessageTemplate(getType().name(), "Lcc creation ${lccId}")
            .withUntypedValue("lccId", getEquipmentId())
            .add();
    }
}
