/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.EnergySource;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.GeneratorModification;

import java.util.List;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Data
@ToString(callSuper = true)
@JsonTypeName("GENERATOR_MODIFICATION")
@ModificationErrorTypeName("MODIFY_GENERATOR_ERROR")
public class GeneratorModificationModel extends InjectionModificationModel {
    private AttributeModification<EnergySource> energySource;

    private AttributeModification<Double> minP;

    private AttributeModification<Double> maxP;

    private AttributeModification<Double> ratedS;

    private AttributeModification<Double> targetP;

    private AttributeModification<Double> targetQ;

    private AttributeModification<Boolean> voltageRegulationOn;

    private AttributeModification<Double> targetV;

    private AttributeModification<Double> plannedActivePowerSetPoint;

    private AttributeModification<Double> marginalCost;

    private AttributeModification<Double> plannedOutageRate;

    private AttributeModification<Double> forcedOutageRate;

    private AttributeModification<Double> minQ;

    private AttributeModification<Double> maxQ;

    private List<ReactiveCapabilityCurvePointsModel> reactiveCapabilityCurvePoints;

    private AttributeModification<Boolean> participate;

    private AttributeModification<Float> droop;

    private AttributeModification<Double> directTransX;

    private AttributeModification<Double> stepUpTransformerX;

    private AttributeModification<VoltageRegulationType> voltageRegulationType;

    private AttributeModification<String> regulatingTerminalId;

    private AttributeModification<String> regulatingTerminalType;

    private AttributeModification<String> regulatingTerminalVlId;

    // As this attribute has only one lower case letter at its start (xXXXX), the getters is parsed as getQPercent and the field for Jackson is parsed as qpercent
    // while we expect qPercent. JsonProperty let fix the json field to qPercent
    @JsonProperty("qPercent")
    private AttributeModification<Double> qPercent;

    private AttributeModification<Boolean> reactiveCapabilityCurve;

    @Override
    public AbstractModification toModification() {
        return new GeneratorModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.generator.modification")
                .withUntypedValue("generatorId", this.getEquipmentId())
                .add();
    }
}
