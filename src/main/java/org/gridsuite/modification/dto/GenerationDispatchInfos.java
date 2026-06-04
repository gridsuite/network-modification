/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.SubstationsGeneratorsOrderingModel;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public class GenerationDispatchInfos implements ModificationInfos {
    @Schema(description = "Modification id")
    private UUID uuid;

    @Schema(description = "Modification date")
    private Instant date;

    @Schema(description = "Modification flag")
    @Builder.Default
    private Boolean stashed = false;

    @Schema(description = "Message type")
    private String messageType;

    @Schema(description = "Message values")
    private String messageValues;

    @Schema(description = "Modification activated (defaults to true at creation when not provided)")
    private Boolean activated;

    @Schema(description = "User description")
    private String description;

    @Schema(description = "loss coefficient")
    private Double lossCoefficient;

    @Schema(description = "default outage rate")
    private Double defaultOutageRate;

    @Schema(description = "generators without outage")
    private List<FilterInfos> generatorsWithoutOutage;

    @Schema(description = "generators with fixed supply")
    private List<FilterInfos> generatorsWithFixedSupply;

    @Schema(description = "generators frequency reserve")
    private List<GeneratorsFrequencyReserveInfos> generatorsFrequencyReserve;

    @Schema(description = "substations hierarchy for ordering generators with marginal cost")
    private List<SubstationsGeneratorsOrderingModel> substationsGeneratorsOrdering;

    @Override
    public ModificationType getType() {
        return ModificationType.GENERATION_DISPATCH;
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        return Map.of();
    }
}
