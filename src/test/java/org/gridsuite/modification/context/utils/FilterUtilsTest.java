/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.context.utils;

import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.gridsuite.modification.context.loaders.FilterLoader;
import org.gridsuite.modification.dto.FilterInfos;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
class FilterUtilsTest {

    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();

    private static Filter aFilter() {
        return IdentifierListFilter.builder()
                .equipmentType(EquipmentType.GENERATOR)
                .equipmentIds(Set.of("GEN_1"))
                .build();
    }

    private static List<String> namesOf(List<Filter> filters) {
        return filters.stream().map(Filter::getName).toList();
    }

    private static Map<UUID, FilterWithDistributionKeys> alreadyResolved(Map<UUID, Filter> filters) {
        return filters.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> FilterWithDistributionKeys.builder().filter(entry.getValue()).build()));
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

    @Test
    void alreadyResolvedFiltersCarryTheNameTheyAreReferencedBy() {
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = alreadyResolved(Map.of(FILTER_ID_1, aFilter()));

        List<Filter> filters = FilterUtils.loadFilterWithNames(List.of(new FilterInfos(FILTER_ID_1, "filter1")), filtersWithDistributionKeys);

        assertEquals(1, filters.size());
        assertEquals("filter1", filters.getFirst().getName());
    }

    @Test
    void aFilterMissingFromTheResolvedMapIsLeftOutInsteadOfFailing() {
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = alreadyResolved(Map.of(FILTER_ID_2, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "deletedFilter"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        List<Filter> filters = assertDoesNotThrow(() -> FilterUtils.loadFilterWithNames(filterInfosList, filtersWithDistributionKeys),
                "A filter that no longer exists must not fail the whole resolution");

        assertEquals(1, filters.size(), "Only the filter that still exists is returned");
        assertEquals("filter2", filters.getFirst().getName(), "The remaining filter is still named after its reference");
    }

    @Test
    void aResolvedFilterThatNoReferencePointsToIsLeftOut() {
        // a shared resolution map may hold filters belonging to other modifications, they are not resolved here
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = alreadyResolved(Map.of(
                FILTER_ID_1, aFilter(),
                FILTER_ID_2, aFilter(),
                FILTER_ID_3, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "filter1"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        List<Filter> filters = FilterUtils.loadFilterWithNames(filterInfosList, filtersWithDistributionKeys);

        assertEquals(List.of("filter1", "filter2"), namesOf(filters));
    }

    @Test
    void aNullResolvedMapIsRejectedWithANullPointerException() {
        List<FilterInfos> filterInfosList = List.of(new FilterInfos(FILTER_ID_1, "filter1"));

        assertThrows(NullPointerException.class, () -> FilterUtils.loadFilterWithNames(filterInfosList, (Map<UUID, FilterWithDistributionKeys>) null));
    }

    @Test
    void aResolvedEntryWithoutAnyValueIsLeftOutInsteadOfFailing() {
        // Map.of does not allow null values, a HashMap is needed to reach the null check
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = new HashMap<>();
        filtersWithDistributionKeys.put(FILTER_ID_1, null);
        filtersWithDistributionKeys.put(FILTER_ID_2, FilterWithDistributionKeys.builder().filter(aFilter()).build());
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "noFilterAtAll"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        List<Filter> filters = assertDoesNotThrow(() -> FilterUtils.loadFilterWithNames(filterInfosList, filtersWithDistributionKeys),
                "An entry without any resolved filter must not fail the whole resolution");

        assertEquals(List.of("filter2"), namesOf(filters));
    }

    @Test
    void aResolvedEntryWithoutAnyValueIsNotNamedAfterItsReference() {
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = new HashMap<>();
        filtersWithDistributionKeys.put(FILTER_ID_1, null);

        List<Filter> filters = FilterUtils.loadFilterWithNames(List.of(new FilterInfos(FILTER_ID_1, "filter1")), filtersWithDistributionKeys);

        assertTrue(filters.isEmpty(), "A null entry yields no filter to name");
    }

    @Test
    void aResolvedFilterWithoutAStandaloneFilterIsRejectedWithANullPointerException() {
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = Map.of(FILTER_ID_1, FilterWithDistributionKeys.builder().build());
        List<FilterInfos> filterInfosList = List.of(new FilterInfos(FILTER_ID_1, "filter1"));

        assertThrows(NullPointerException.class, () -> FilterUtils.loadFilterWithNames(filterInfosList, filtersWithDistributionKeys));
    }

    @Test
    void bothOverloadsReturnTheSameFiltersForTheSameResolvedFilters() {
        // the loaders are given in a hash order, the returned filters must not depend on it
        FilterLoader filterLoader = filterUuids -> filterUuids.stream()
                .collect(Collectors.toMap(Function.identity(), uuid -> aFilter()));
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = alreadyResolved(filterLoader.load(List.of(FILTER_ID_1, FILTER_ID_2)));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "filter1"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        List<Filter> filtersFromLoader = FilterUtils.loadFilterWithNames(filterInfosList, filterLoader);
        List<Filter> filtersFromResolvedMap = FilterUtils.loadFilterWithNames(filterInfosList, filtersWithDistributionKeys);

        assertEquals(2, filtersFromLoader.size());
        assertEquals(namesOf(filtersFromLoader), namesOf(filtersFromResolvedMap));
    }

    @Test
    void resolvedFiltersKeepTheOrderTheyAreReferencedIn() {
        FilterLoader filterLoader = filterUuids -> filterUuids.stream()
                .collect(Collectors.toMap(Function.identity(), uuid -> aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "filter1"),
                new FilterInfos(FILTER_ID_2, "filter2"),
                new FilterInfos(FILTER_ID_3, "filter3"),
                new FilterInfos(FILTER_ID_4, "filter4"));

        assertEquals(List.of("filter1", "filter2", "filter3", "filter4"), namesOf(FilterUtils.loadFilterWithNames(filterInfosList, filterLoader)));
    }

    @Test
    void alreadyResolvedFiltersKeepTheOrderTheyAreReferencedIn() {
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = alreadyResolved(Map.of(
                FILTER_ID_1, aFilter(),
                FILTER_ID_2, aFilter(),
                FILTER_ID_3, aFilter(),
                FILTER_ID_4, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_4, "filter4"),
                new FilterInfos(FILTER_ID_3, "filter3"),
                new FilterInfos(FILTER_ID_1, "filter1"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        assertEquals(List.of("filter4", "filter3", "filter1", "filter2"), namesOf(FilterUtils.loadFilterWithNames(filterInfosList, filtersWithDistributionKeys)));
    }

    @Test
    void aMissingFilterDoesNotShiftThePositionOfTheOthers() {
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = alreadyResolved(Map.of(
                FILTER_ID_2, aFilter(),
                FILTER_ID_3, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "deletedFilter"),
                new FilterInfos(FILTER_ID_2, "filter2"),
                new FilterInfos(FILTER_ID_3, "filter3"));

        assertEquals(List.of("filter2", "filter3"), namesOf(FilterUtils.loadFilterWithNames(filterInfosList, filtersWithDistributionKeys)));
    }

    @Test
    void aFilterReferencedSeveralTimesIsResolvedOnlyOnce() {
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = alreadyResolved(Map.of(FILTER_ID_1, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "filter1"),
                new FilterInfos(FILTER_ID_1, "filter1Renamed"));

        List<Filter> filters = FilterUtils.loadFilterWithNames(filterInfosList, filtersWithDistributionKeys);

        assertEquals(List.of("filter1Renamed"), namesOf(filters));
    }
}
