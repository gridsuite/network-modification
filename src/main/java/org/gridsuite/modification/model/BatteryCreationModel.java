/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
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
@JsonTypeName("BATTERY_CREATION")
@ModificationErrorTypeName("CREATE_BATTERY_ERROR")
public class BatteryCreationModel extends InjectionCreationModel implements ReactiveLimitsHolderModel {

    private double minP;

    private double maxP;

    private Double minQ;

    private Double maxQ;

    private List<ReactiveCapabilityCurvePointsModel> reactiveCapabilityCurvePoints;

    private double targetP;

    private Double targetQ;

    private Boolean participate;

    private Float droop;

    private Double directTransX;

    private Double stepUpTransformerX;

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
