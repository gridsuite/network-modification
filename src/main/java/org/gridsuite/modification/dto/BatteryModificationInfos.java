/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;

import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.BatteryModification;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Data
@ToString(callSuper = true)
@Schema(description = "Battery modification")
@JsonTypeName("BATTERY_MODIFICATION")
@ModificationErrorTypeName("MODIFY_BATTERY_ERROR")
public class BatteryModificationInfos extends InjectionModificationInfos {
    @Schema(description = "Minimum active power")
    private AttributeModification<Double> minP;

    @Schema(description = "Maximum active power")
    private AttributeModification<Double> maxP;

    @Schema(description = "Active power set point")
    private AttributeModification<Double> targetP;

    @Schema(description = "Reactive power set point")
    private AttributeModification<Double> targetQ;

    @Schema(description = "Participate")
    private AttributeModification<Boolean> participate;

    @Schema(description = "Droop")
    private AttributeModification<Float> droop;

    @Schema(description = "Minimum reactive power")
    private AttributeModification<Double> minQ;

    @Schema(description = "Maximum reactive power")
    private AttributeModification<Double> maxQ;

    @Schema(description = "Reactive capability curve points")
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;

    @Schema(description = "Reactive capability curve")
    private AttributeModification<Boolean> reactiveCapabilityCurve;

    @Override
    public AbstractModification toModification() {
        return new BatteryModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.battery.modification")
                .withUntypedValue("batteryId", this.getEquipmentId())
                .add();
    }
}
