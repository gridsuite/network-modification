/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.BatteryModification;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Data
@ToString(callSuper = true)
@JsonTypeName("BATTERY_MODIFICATION")
@ModificationErrorTypeName("MODIFY_BATTERY_ERROR")
public class BatteryModificationModel extends InjectionModificationModel {
    private AttributeModification<Double> minP;

    private AttributeModification<Double> maxP;

    private AttributeModification<Double> targetP;

    private AttributeModification<Double> targetQ;

    private AttributeModification<Boolean> participate;

    private AttributeModification<Float> droop;

    private AttributeModification<Double> directTransX;

    private AttributeModification<Double> stepUpTransformerX;

    private AttributeModification<Double> minQ;

    private AttributeModification<Double> maxQ;

    private List<ReactiveCapabilityCurvePointsModel> reactiveCapabilityCurvePoints;

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
