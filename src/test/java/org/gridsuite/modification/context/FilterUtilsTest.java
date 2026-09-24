/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.context;

import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.dto.FilterInfos;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
class FilterUtilsTest {

    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();

    private static Filter aFilter() {
        return IdentifierListFilter.builder()
                .equipmentType(EquipmentType.GENERATOR)
                .equipmentIds(Set.of("GEN_1"))
                .build();
    }

    @Test
    void resolvedFiltersCarryTheNameTheyAreReferencedBy() {
        FilterLoader filterLoader = filterUuids -> Map.of(FILTER_ID_1, aFilter());

        List<Filter> filters = FilterUtils.loadFilterWithNames(List.of(new FilterInfos(FILTER_ID_1, "filter1")), filterLoader);

        assertEquals(1, filters.size());
        assertEquals("filter1", filters.getFirst().getName());
    }

    @Test
    void aFilterMissingFromTheLoaderIsLeftOutInsteadOfFailing() {
        // the loader contract omits the filters it cannot find, e.g. deleted since the modification was created
        FilterLoader filterLoader = filterUuids -> Map.of(FILTER_ID_2, aFilter());
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "deletedFilter"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        List<Filter> filters = assertDoesNotThrow(() -> FilterUtils.loadFilterWithNames(filterInfosList, filterLoader),
                "A filter that no longer exists must not fail the whole resolution");

        assertEquals(1, filters.size(), "Only the filter that still exists is returned");
        assertEquals("filter2", filters.getFirst().getName(), "The remaining filter is still named after its reference");
    }

    @Test
    void noFilterResolvedAtAllYieldsAnEmptyList() {
        FilterLoader filterLoader = filterUuids -> Map.of();
        List<FilterInfos> filterInfosList = List.of(new FilterInfos(FILTER_ID_1, "deletedFilter"));

        List<Filter> filters = assertDoesNotThrow(() -> FilterUtils.loadFilterWithNames(filterInfosList, filterLoader));

        assertTrue(filters.isEmpty());
    }
}
