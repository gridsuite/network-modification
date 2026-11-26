/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Filters referenced by modifications, whose names are resolved from the directory by the caller.
 *
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
class ModificationFiltersTest {

    private static FilterInfos filter() {
        return FilterInfos.builder().id(UUID.randomUUID()).build();
    }

    @Test
    void testReferencedFilters() {
        FilterInfos deletionFilter = filter();
        assertEquals(List.of(deletionFilter),
                ByFilterDeletionInfos.builder().filters(List.of(deletionFilter)).build().getFiltersStream().toList());

        FilterInfos scalingFilter1 = filter();
        FilterInfos scalingFilter2 = filter();
        assertEquals(List.of(scalingFilter1, scalingFilter2),
                GeneratorScalingInfos.builder().variations(List.of(
                        ScalingVariationInfos.builder().filters(List.of(scalingFilter1)).build(),
                        ScalingVariationInfos.builder().filters(List.of(scalingFilter2)).build())).build()
                    .getFiltersStream().toList());

        FilterInfos withoutOutage = filter();
        FilterInfos withFixedSupply = filter();
        FilterInfos frequencyReserve = filter();
        assertEquals(List.of(withoutOutage, withFixedSupply, frequencyReserve),
                GenerationDispatchInfos.builder()
                    .generatorsWithoutOutage(List.of(withoutOutage))
                    .generatorsWithFixedSupply(List.of(withFixedSupply))
                    .generatorsFrequencyReserve(List.of(GeneratorsFrequencyReserveInfos.builder().generatorsFilters(List.of(frequencyReserve)).build()))
                    .build()
                    .getFiltersStream().toList());

        FilterInfos formulaFilter = filter();
        assertEquals(List.of(formulaFilter),
                ByFormulaModificationInfos.builder().formulaInfosList(List.of(FormulaInfos.builder().filters(List.of(formulaFilter)).build())).build()
                    .getFiltersStream().toList());

        FilterInfos assignmentFilter = filter();
        assertEquals(List.of(assignmentFilter),
                ModificationByAssignmentInfos.builder().assignmentInfosList(List.of(DoubleAssignmentInfos.builder().filters(List.of(assignmentFilter)).build())).build()
                    .getFiltersStream().toList());
    }

    @Test
    void testNoReferencedFilters() {
        assertEquals(0, new ModificationInfos().getFiltersStream().count());
        assertEquals(0, new ByFilterDeletionInfos().getFiltersStream().count());
        assertEquals(0, new GeneratorScalingInfos().getFiltersStream().count());
        assertEquals(0, GeneratorScalingInfos.builder().variations(List.of(new ScalingVariationInfos())).build().getFiltersStream().count());
        assertEquals(0, new GenerationDispatchInfos().getFiltersStream().count());
        assertEquals(0, new ByFormulaModificationInfos().getFiltersStream().count());
        assertEquals(0, new ModificationByAssignmentInfos().getFiltersStream().count());
    }
}
