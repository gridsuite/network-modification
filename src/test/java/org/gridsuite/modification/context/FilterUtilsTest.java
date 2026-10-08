/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.context;

import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.modifications.data.scaling.VariationFilterData;
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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
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

    private static final Map<String, Double> DISTRIBUTION_KEYS = Map.of("GEN_1", 1.0);

    private static Filter aFilter() {
        return IdentifierListFilter.builder()
                .equipmentType(EquipmentType.GENERATOR)
                .equipmentIds(Set.of("GEN_1"))
                .build();
    }

    private static List<String> namesOf(List<Filter> filters) {
        return filters.stream().map(Filter::getName).toList();
    }

    private static List<String> namesOfPairedFilters(List<VariationFilterData> filters) {
        return filters.stream()
                .map(filter -> filter.filter() != null ? filter.filter().getName() : "unresolved")
                .toList();
    }

    private static Map<UUID, FilterWithDistributionKeys> alreadyResolved(Map<UUID, Filter> filters) {
        return filters.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> FilterWithDistributionKeys.builder()
                        .filter(entry.getValue())
                        .distributionKeys(DISTRIBUTION_KEYS)
                        .build()));
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
    void alreadyResolvedFiltersCarryTheirNameAndKeys() {
        Map<UUID, FilterWithDistributionKeys> resolved = alreadyResolved(Map.of(FILTER_ID_1, aFilter()));

        List<VariationFilterData> filters = FilterUtils.loadFiltersWithDistributionKeys(List.of(new FilterInfos(FILTER_ID_1, "filter1")), resolved);

        assertEquals(1, filters.size());
        assertEquals("filter1", filters.getFirst().filter().getName());
        assertEquals(DISTRIBUTION_KEYS, filters.getFirst().distributionKeys(), "the keys are carried, not interpreted");
        assertNotNull(filters.getFirst());
    }

    @Test
    void aFilterMissingFromTheResolvedMapIsKeptAsAnUnresolvedOne() {
        // it must stay visible: the modification is the one deciding that a missing filter is an error
        Map<UUID, FilterWithDistributionKeys> resolved = alreadyResolved(Map.of(FILTER_ID_2, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "deletedFilter"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        List<VariationFilterData> filters = FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, resolved);

        assertEquals(2, filters.size(), "A filter that no longer exists is kept, so that it can be reported");
        assertNull(filters.getFirst().filter());
        assertEquals(Map.of(), filters.getFirst().distributionKeys());
        assertNotNull(filters.get(1).filter());
        assertEquals("filter2", filters.get(1).filter().getName(), "the resolved filter is still named after its reference");
    }

    @Test
    void aResolvedFilterThatNoReferencePointsToIsLeftOut() {
        // a shared resolution map may hold filters belonging to other modifications, they are not resolved here
        Map<UUID, FilterWithDistributionKeys> resolved = alreadyResolved(Map.of(
                FILTER_ID_1, aFilter(),
                FILTER_ID_2, aFilter(),
                FILTER_ID_3, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "filter1"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        List<VariationFilterData> filters = FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, resolved);

        assertEquals(List.of("filter1", "filter2"), namesOfPairedFilters(filters));
    }

    @Test
    void aNullResolvedMapIsRejectedWithANullPointerException() {
        List<FilterInfos> filterInfosList = List.of(new FilterInfos(FILTER_ID_1, "filter1"));

        assertThrows(NullPointerException.class, () -> FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, null));
    }

    @Test
    void aResolvedEntryWithoutAnyValueIsKeptAsAnUnresolvedFilter() {
        // Map.of does not allow null values, a HashMap is needed to reach the null check
        Map<UUID, FilterWithDistributionKeys> resolved = new HashMap<>();
        resolved.put(FILTER_ID_1, null);
        resolved.put(FILTER_ID_2, FilterWithDistributionKeys.builder().filter(aFilter()).build());
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "noFilterAtAll"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        List<VariationFilterData> filters = assertDoesNotThrow(() -> FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, resolved),
                "An entry without any resolved filter must not fail the whole resolution");

        assertEquals(2, filters.size());
        assertNull(filters.getFirst().filter());
        assertEquals(List.of("unresolved", "filter2"), namesOfPairedFilters(filters));
    }

    @Test
    void aResolvedFilterWithoutAStandaloneFilterIsKeptWithNoKeys() {
        Map<UUID, FilterWithDistributionKeys> resolved = Map.of(FILTER_ID_1, FilterWithDistributionKeys.builder().build());
        List<FilterInfos> filterInfosList = List.of(new FilterInfos(FILTER_ID_1, "filter1"));

        List<VariationFilterData> filters = FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, resolved);

        assertEquals(1, filters.size());
        assertNull(filters.getFirst().filter(), "no standalone filter means an unresolved filter");
        assertEquals(Map.of(), filters.getFirst().distributionKeys(), "a null key map is normalised to an empty one");
    }

    @Test
    void alreadyResolvedFiltersKeepTheOrderTheyAreReferencedIn() {
        // the filters are given in a hash order, the pairing must not depend on it
        Map<UUID, FilterWithDistributionKeys> resolved = alreadyResolved(Map.of(
                FILTER_ID_1, aFilter(),
                FILTER_ID_2, aFilter(),
                FILTER_ID_3, aFilter(),
                FILTER_ID_4, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_4, "filter4"),
                new FilterInfos(FILTER_ID_3, "filter3"),
                new FilterInfos(FILTER_ID_1, "filter1"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        assertEquals(List.of("filter4", "filter3", "filter1", "filter2"),
                namesOfPairedFilters(FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, resolved)));
    }

    @Test
    void aMissingFilterDoesNotShiftThePositionOfTheOthers() {
        Map<UUID, FilterWithDistributionKeys> resolved = alreadyResolved(Map.of(
                FILTER_ID_2, aFilter(),
                FILTER_ID_3, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "deletedFilter"),
                new FilterInfos(FILTER_ID_2, "filter2"),
                new FilterInfos(FILTER_ID_3, "filter3"));

        assertEquals(List.of("unresolved", "filter2", "filter3"),
                namesOfPairedFilters(FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, resolved)),
                "the missing filter keeps the position it is referenced at");
    }

    @Test
    void aFilterReferencedSeveralTimesIsPairedOnlyOnce() {
        // one filter must never look like two filters keying the same equipments
        Map<UUID, FilterWithDistributionKeys> resolved = alreadyResolved(Map.of(FILTER_ID_1, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "filter1"),
                new FilterInfos(FILTER_ID_1, "filter1Renamed"));

        List<VariationFilterData> filters = FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, resolved);

        assertEquals(1, filters.size());
        assertEquals("filter1Renamed", filters.getFirst().filter().getName());
    }

    @Test
    void noFilterResolvedAtAllYieldsAnEmptyListOfPairs() {
        List<FilterInfos> filterInfosList = List.of();

        assertTrue(FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, Map.of()).isEmpty());
    }

    @Test
    void aSharedResolutionGivesTheSamePairsAsALoaderWouldGiveFilters() {
        FilterLoader filterLoader = filterUuids -> filterUuids.stream()
                .collect(Collectors.toMap(Function.identity(), uuid -> aFilter()));
        Map<UUID, FilterWithDistributionKeys> resolved = alreadyResolved(Map.of(FILTER_ID_1, aFilter(), FILTER_ID_2, aFilter()));
        List<FilterInfos> filterInfosList = List.of(
                new FilterInfos(FILTER_ID_1, "filter1"),
                new FilterInfos(FILTER_ID_2, "filter2"));

        List<Filter> filtersFromLoader = FilterUtils.loadFilterWithNames(filterInfosList, filterLoader);
        List<VariationFilterData> paired = FilterUtils.loadFiltersWithDistributionKeys(filterInfosList, resolved);

        assertEquals(2, filtersFromLoader.size());
        assertEquals(2, paired.size());
        assertEquals(namesOf(filtersFromLoader), namesOfPairedFilters(paired));
    }
}
