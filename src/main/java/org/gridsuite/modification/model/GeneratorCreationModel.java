/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.EnergySource;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.GeneratorCreation;

import java.util.List;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("GENERATOR_CREATION")
@ModificationErrorTypeName("CREATE_GENERATOR_ERROR")
public class GeneratorCreationModel extends InjectionCreationModel implements ReactiveLimitsHolderModel {
    private EnergySource energySource;

    private double minP;

    private double maxP;

    private Double ratedS;

    private double targetP;

    private Double targetQ;

    private boolean voltageRegulationOn;

    private Double targetV;

    private Double plannedActivePowerSetPoint;

    private Double marginalCost;

    private Double plannedOutageRate;

    private Double forcedOutageRate;

    private Double minQ;

    private Double maxQ;

    private List<ReactiveCapabilityCurvePointsModel> reactiveCapabilityCurvePoints;

    private Boolean participate;

    private Float droop;

    private Double directTransX;

    private Double stepUpTransformerX;

    private String regulatingTerminalId;

    private String regulatingTerminalType;

    private String regulatingTerminalVlId;

    // As this attribute has only one lower case letter at its start (xXXXX), the getters is parsed as getQPercent and the field for Jackson is parsed as qpercent
    // while we expect qPercent. JsonProperty let fix the json field to qPercent
    @JsonProperty("qPercent")
    private Double qPercent;

    private Boolean reactiveCapabilityCurve;

    @Override
    public AbstractModification toModification() {
        return new GeneratorCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.generator.creation")
                .withUntypedValue("generatorId", this.getEquipmentId())
                .add();
    }
}
