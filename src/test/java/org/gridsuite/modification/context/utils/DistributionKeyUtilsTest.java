/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.context.utils;

import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.utils.FilterType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
class DistributionKeyUtilsTest {

    private static final UUID FILTER_1 = UUID.randomUUID();
    private static final UUID FILTER_2 = UUID.randomUUID();
    private static final UUID MISSING_FILTER = UUID.randomUUID();

    private static final String GEN_1 = "gen1";
    private static final String GEN_2 = "gen2";
    private static final String GEN_3 = "gen3";
    private static final String GEN_4 = "gen4";

    /** (equipmentId, key) couples, so that a null key can be built, which Map.of does not allow. */
    private static Map<String, Double> keys(Object... equipmentIdAndKey) {
        Map<String, Double> keys = new HashMap<>();
        for (int i = 0; i < equipmentIdAndKey.length; i += 2) {
            keys.put((String) equipmentIdAndKey[i], (Double) equipmentIdAndKey[i + 1]);
        }
        return keys;
    }

    private static Filter anIdentifierListFilter(Set<String> equipmentIds) {
        return IdentifierListFilter.builder()
                .equipmentType(EquipmentType.GENERATOR)
                .equipmentIds(equipmentIds)
                .build();
    }

    private static FilterWithDistributionKeys aFilterWithKeys(Map<String, Double> distributionKeys) {
        return FilterWithDistributionKeys.builder()
                .filter(anIdentifierListFilter(distributionKeys.keySet()))
                .distributionKeys(distributionKeys)
                .build();
    }

    private static Filter aFilterThatIsNotAnIdentifierListFilter() {
        Filter filter = mock(Filter.class);
        when(filter.getFilterType()).thenReturn(FilterType.EXPERT);
        return filter;
    }

    @Test
    void theDistributionKeysOfEveryRequestedFilterAreGathered() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0, GEN_2, 2.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_3, 3.0, GEN_4, 0.5)));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1, FILTER_2), filters);

        assertEquals(Map.of(GEN_1, 1.0, GEN_2, 2.0, GEN_3, 3.0, GEN_4, 0.5), distributionKeys);
    }

    @Test
    void aFilterThatIsNotFoundInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1, MISSING_FILTER), filters);

        assertTrue(distributionKeys.isEmpty(), "A filter the loader could not find must invalidate every distribution key");
    }

    @Test
    void aFilterWithNullDistributionKeysInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, FilterWithDistributionKeys.builder().filter(anIdentifierListFilter(Set.of(GEN_2))).build());

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(distributionKeys.isEmpty());
    }

    @Test
    void aFilterWithoutAnyDistributionKeyInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, aFilterWithKeys(Map.of()));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(distributionKeys.isEmpty(), "A filter whose equipments have no distribution key at all is not usable");
    }

    @Test
    void anEquipmentWithANullDistributionKeyInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_2, null)));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(distributionKeys.isEmpty(), "A null distribution key is not a valid distribution key");
    }

    @Test
    void anEquipmentSelectedByTwoFiltersInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0, GEN_2, 2.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_2, 5.0, GEN_3, 3.0)));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(distributionKeys.isEmpty(), "An equipment selected twice cannot be weighted once, so the keys are discarded");
    }

    @Test
    void aDuplicateFilterIdentifierInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1, FILTER_1), filters);

        assertTrue(distributionKeys.isEmpty(), "filterUuids is documented as distinct: a duplicate selects the same equipments twice");
    }

    @Test
    void aZeroDistributionKeyIsAValidDistributionKey() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 0.0)));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1), filters);

        assertEquals(Map.of(GEN_1, 0.0), distributionKeys, "Only a null key is rejected, not a zero one");
    }

    @Test
    void anInvalidFilterDiscardsTheKeysGatheredFromTheValidFiltersBeforeIt() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0, GEN_2, 2.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_3, null)));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(distributionKeys.isEmpty(), "All or nothing: a partially gathered map must never leak out");
    }

    @Test
    void aMissingFilterIsDetectedWhicheverItsPositionInTheList() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)));

        assertTrue(DistributionKeyUtils.loadIfValid(List.of(MISSING_FILTER, FILTER_1), filters).isEmpty());
        assertTrue(DistributionKeyUtils.loadIfValid(List.of(FILTER_1, MISSING_FILTER), filters).isEmpty(),
                "A missing filter is detected whether it short-circuits the first or the last iteration");
    }

    @Test
    void aFilterThatWasNotRequestedIsIgnored() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_2, 2.0)));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1), filters);

        assertEquals(Map.of(GEN_1, 1.0), distributionKeys, "Only the requested filters are gathered");
    }

    @Test
    void noFilterAtAllYieldsAnEmptyMap() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)));

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(), filters);

        assertTrue(distributionKeys.isEmpty());
    }

    @Test
    void noFilterAtAllAndNoFilterLoadedYieldsAnEmptyMap() {
        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(), Map.of());

        assertTrue(distributionKeys.isEmpty());
    }

    @Test
    void aFilterThatIsNotAnIdentifierListFilterIsStillConsideredValid() {
        // loadIfValid never reads getFilter(): the javadoc rule that a non identifier list filter
        // invalidates the keys is not enforced here
        FilterWithDistributionKeys filterWithKeys = FilterWithDistributionKeys.builder()
                .filter(aFilterThatIsNotAnIdentifierListFilter())
                .distributionKeys(keys(GEN_1, 1.0))
                .build();

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1), Map.of(FILTER_1, filterWithKeys));

        assertEquals(Map.of(GEN_1, 1.0), distributionKeys, "The filter type is not inspected, whatever the javadoc promises");
    }

    @Test
    void aFilterWithDistributionKeysAndNoFilterAtAllIsStillConsideredValid() {
        FilterWithDistributionKeys filterWithKeys = FilterWithDistributionKeys.builder()
                .filter(null)
                .distributionKeys(keys(GEN_1, 1.0))
                .build();

        Map<String, Double> distributionKeys = DistributionKeyUtils.loadIfValid(List.of(FILTER_1), Map.of(FILTER_1, filterWithKeys));

        assertEquals(Map.of(GEN_1, 1.0), distributionKeys, "The embedded filter is never dereferenced, it may even be null");
    }

    @Test
    void aNullFiltersMapIsNotSupported() {
        assertThrows(NullPointerException.class, () -> DistributionKeyUtils.loadIfValid(List.of(FILTER_1), null));
    }

    @Test
    void aNullFilterUuidListIsNotSupported() {
        assertThrows(NullPointerException.class, () -> DistributionKeyUtils.loadIfValid(null, Map.of()));
    }
}
