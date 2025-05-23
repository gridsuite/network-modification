/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
import org.gridsuite.modification.modifications.GenerationDispatch;

import java.util.List;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Generation dispatch creation")
@JsonTypeName("GENERATION_DISPATCH")
@ModificationErrorTypeName("GENERATION_DISPATCH_ERROR")
public class GenerationDispatchInfos extends ModificationInfos {
    @Schema(description = "loss coefficient")
    private Double lossCoefficient;

    @Schema(description = "default outage rate")
    private Double defaultOutageRate;

    @Schema(description = "generators without outage")
    private List<GeneratorsFilterInfos> generatorsWithoutOutage;

    @Schema(description = "generators with fixed supply")
    private List<GeneratorsFilterInfos> generatorsWithFixedSupply;

    @Schema(description = "generators frequency reserve")
    private List<GeneratorsFrequencyReserveInfos> generatorsFrequencyReserve;

    @Schema(description = "substations hierarchy for ordering generators with marginal cost")
    private List<SubstationsGeneratorsOrderingInfos> substationsGeneratorsOrdering;

    @Override
    public AbstractModification toModification() {
        return new GenerationDispatch(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.generationDispatch")
                .add();
    }
}
