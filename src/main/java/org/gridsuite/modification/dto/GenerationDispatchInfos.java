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
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.context.FilterLoader;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.GenerationDispatch;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

import static org.gridsuite.modification.context.FilterUtils.loadFilterWithNames;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Generation dispatch creation")
@JsonTypeName("GENERATION_DISPATCH")
public class GenerationDispatchInfos extends ModificationInfos {
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
    private List<SubstationsGeneratorsOrderingInfos> substationsGeneratorsOrdering;

    @Override
    public AbstractModification toModification(ModificationContext modificationContext) {
        List<UUID> referencedFilterIds = streamReferencedFilters().map(FilterInfos::getId).distinct().toList();
        Map<UUID, Filter> filtersById = referencedFilterIds.isEmpty()
                ? Map.of()
                : modificationContext.filterLoader().load(referencedFilterIds);
        // the filters of all the sections are loaded at once, each section then names and orders its own ones
        FilterLoader loadedFilters = filterUuids -> filtersById;

        return GenerationDispatch.builder()
                .lossCoefficient(getLossCoefficient())
                .defaultOutageRate(getDefaultOutageRate())
                .generatorsWithoutOutage(loadFilterWithNames(nullToEmpty(getGeneratorsWithoutOutage()), loadedFilters))
                .generatorsWithFixedSupply(loadFilterWithNames(nullToEmpty(getGeneratorsWithFixedSupply()), loadedFilters))
                .generatorsFrequencyReserve(nullToEmpty(getGeneratorsFrequencyReserve()).stream()
                        .map(reserve -> new GenerationDispatch.GeneratorsFrequencyReserve(
                                loadFilterWithNames(nullToEmpty(reserve.getGeneratorsFilters()), loadedFilters),
                                reserve.getFrequencyReserve()))
                        .toList())
                .substationsGeneratorsOrdering(nullToEmpty(getSubstationsGeneratorsOrdering()).stream()
                        .map(SubstationsGeneratorsOrderingInfos::getSubstationIds)
                        .toList())
                .missingFiltersCount((int) referencedFilterIds.stream().filter(id -> !filtersById.containsKey(id)).count())
                .build();
    }

    private Stream<FilterInfos> streamReferencedFilters() {
        return Stream.of(
                nullToEmpty(getGeneratorsWithoutOutage()).stream(),
                nullToEmpty(getGeneratorsWithFixedSupply()).stream(),
                nullToEmpty(getGeneratorsFrequencyReserve()).stream().flatMap(reserve -> nullToEmpty(reserve.getGeneratorsFilters()).stream())
        ).flatMap(Function.identity());
    }

    private static <T> List<T> nullToEmpty(List<T> list) {
        return list == null ? List.of() : list;
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.generationDispatch")
                .add();
    }
}
