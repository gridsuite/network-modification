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
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.BatteryCreation;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Battery creation")
@JsonTypeName("BATTERY_CREATION")
@ModificationErrorTypeName("CREATE_BATTERY_ERROR")
public class BatteryCreationInfos extends InjectionCreationInfos implements ReactiveLimitsHolderInfos {

    @Schema(description = "Minimum active power")
    private double minP;

    @Schema(description = "Maximum active power")
    private double maxP;

    @Schema(description = "Minimum reactive power")
    private Double minQ;

    @Schema(description = "Maximum reactive power")
    private Double maxQ;

    @Schema(description = "Reactive capability curve points")
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;

    @Schema(description = "Active power set point")
    private double targetP;

    @Schema(description = "Reactive power set point")
    private Double targetQ;

    @Schema(description = "Participate")
    private Boolean participate;

    @Schema(description = "Droop")
    private Float droop;

    @Schema(description = "Reactive capability curve")
    private Boolean reactiveCapabilityCurve;

    @Override
    public AbstractModification toModification() {
        return new BatteryCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.batteryCreation").withUntypedValue("batteryId", this.getEquipmentId()).add();
    }
}
