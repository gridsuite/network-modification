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
import org.gridsuite.modification.modifications.data.scaling.DistributionKeyStatus;
import org.gridsuite.modification.modifications.data.scaling.LoadedDistributionKeys;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertAll;
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
    private static final UUID FILTER_3 = UUID.randomUUID();
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
        return aFilterWithKeys(distributionKeys, distributionKeys.keySet());
    }

    /** To build a filter selecting equipments it does not give a key to, or the other way round. */
    private static FilterWithDistributionKeys aFilterWithKeys(Map<String, Double> distributionKeys, Set<String> equipmentIds) {
        return FilterWithDistributionKeys.builder()
                .filter(anIdentifierListFilter(equipmentIds))
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

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, FILTER_2), filters);

        assertEquals(Map.of(GEN_1, 1.0, GEN_2, 2.0, GEN_3, 3.0, GEN_4, 0.5), loadedKeys.distributionKeys());
        assertEquals(DistributionKeyStatus.VALID_KEYS, loadedKeys.status());
        assertEquals(2.0, loadedKeys.getDistributionKey(GEN_2), "The gathered keys are reachable by equipment id");
    }

    @Test
    void aFilterThatIsNotFoundInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, MISSING_FILTER), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty(), "A filter the loader could not find must invalidate every distribution key");
        assertEquals(DistributionKeyStatus.MISSING_FILTER, loadedKeys.status());
    }

    @Test
    void aFilterWithNullDistributionKeysInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, FilterWithDistributionKeys.builder().filter(anIdentifierListFilter(Set.of(GEN_2))).build());

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty());
        assertEquals(DistributionKeyStatus.FILTER_HAS_NO_KEYS, loadedKeys.status(), "A null key map is reported as a filter without any key");
    }

    @Test
    void aFilterWithoutAnyDistributionKeyInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, aFilterWithKeys(Map.of()));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty(), "A filter whose equipments have no distribution key at all is not usable");
        assertEquals(DistributionKeyStatus.FILTER_HAS_NO_KEYS, loadedKeys.status());
    }

    @Test
    void anEquipmentWithANullDistributionKeyInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_2, null)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty(), "A null distribution key is not a valid distribution key");
        assertEquals(DistributionKeyStatus.MISSING_EQUIPMENT_KEY, loadedKeys.status(),
                "A null key is not a duplication, the two must stay distinguishable to report the right reason");
    }

    @Test
    void anEquipmentSelectedByTwoFiltersInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0, GEN_2, 2.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_2, 5.0, GEN_3, 3.0)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty(), "An equipment selected twice cannot be weighted once, so the keys are discarded");
        assertEquals(DistributionKeyStatus.DUPLICATED_EQUIPMENT_KEY, loadedKeys.status());
    }

    @Test
    void aDuplicateFilterIdentifierInvalidatesAllTheDistributionKeys() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, FILTER_1), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty(), "filterUuids is documented as distinct: a duplicate selects the same equipments twice");
        assertEquals(DistributionKeyStatus.DUPLICATED_EQUIPMENT_KEY, loadedKeys.status(),
                "A repeated filter is detected as an equipment keyed twice");
    }

    @Test
    void aZeroDistributionKeyIsAValidDistributionKey() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 0.0)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1), filters);

        assertEquals(Map.of(GEN_1, 0.0), loadedKeys.distributionKeys(), "Only a null key is rejected, not a zero one");
        assertEquals(DistributionKeyStatus.VALID_KEYS, loadedKeys.status());
    }

    @Test
    void anInvalidFilterDiscardsTheKeysGatheredFromTheValidFiltersBeforeIt() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0, GEN_2, 2.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_3, null)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, FILTER_2), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty(), "All or nothing: a partially gathered map must never leak out");
        assertEquals(DistributionKeyStatus.MISSING_EQUIPMENT_KEY, loadedKeys.status());
    }

    @Test
    void aMissingFilterIsDetectedWhicheverItsPositionInTheList() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)));

        LoadedDistributionKeys first = DistributionKeyUtils.reduceDistributionKeys(List.of(MISSING_FILTER, FILTER_1), filters);
        LoadedDistributionKeys last = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, MISSING_FILTER), filters);

        assertAll(
                () -> assertTrue(first.distributionKeys().isEmpty()),
                () -> assertEquals(DistributionKeyStatus.MISSING_FILTER, first.status()),
                () -> assertTrue(last.distributionKeys().isEmpty()),
                () -> assertEquals(DistributionKeyStatus.MISSING_FILTER, last.status()),
                () -> assertEquals(first, last, "A missing filter is detected whether it short-circuits the first or the last iteration"));
    }

    @Test
    void theStatusOfTheFirstUnusableFilterIsReported() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_1, 5.0)));

        // FILTER_2 is loaded and would give GEN_1 a second key, but FILTER_1 is missing, so the
        // iteration never reaches the duplication
        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, MISSING_FILTER, FILTER_2), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty());
        assertEquals(DistributionKeyStatus.MISSING_FILTER, loadedKeys.status(),
                "The reduction short-circuits: the first unusable filter dictates the reported reason");
    }

    @Test
    void aDuplicatedEquipmentKeyIsReportedEvenWhenALaterFilterHasNoKeyAtAll() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_1, 5.0)),
                FILTER_3, aFilterWithKeys(Map.of()));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1, FILTER_2, FILTER_3), filters);

        assertEquals(DistributionKeyStatus.DUPLICATED_EQUIPMENT_KEY, loadedKeys.status(),
                "The keys are discarded at the first problem met, so the later empty key map is never reached");
    }

    @Test
    void aFilterThatWasNotRequestedIsIgnored() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_2, 2.0)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1), filters);

        assertEquals(Map.of(GEN_1, 1.0), loadedKeys.distributionKeys(), "Only the requested filters are gathered");
        assertEquals(DistributionKeyStatus.VALID_KEYS, loadedKeys.status());
    }

    @Test
    void anUnusableFilterThatWasNotRequestedIsIgnored() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)),
                FILTER_2, aFilterWithKeys(keys(GEN_2, null)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1), filters);

        assertEquals(Map.of(GEN_1, 1.0), loadedKeys.distributionKeys());
        assertEquals(DistributionKeyStatus.VALID_KEYS, loadedKeys.status(),
                "A filter another variation uses must not invalidate this variation's keys");
    }

    @Test
    void noFilterAtAllYieldsAnEmptyMap() {
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty());
        assertEquals(DistributionKeyStatus.VALID_KEYS, loadedKeys.status(), "Nothing to gather is not a failure");
    }

    @Test
    void noFilterAtAllAndNoFilterLoadedYieldsAnEmptyMap() {
        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(), Map.of());

        assertTrue(loadedKeys.distributionKeys().isEmpty());
        assertEquals(DistributionKeyStatus.VALID_KEYS, loadedKeys.status());
    }

    @Test
    void aFilterThatIsNotAnIdentifierListFilterIsStillConsideredValid() {
        // reduceIfValid never reads getFilter(): the javadoc rule that a non identifier list filter
        // invalidates the keys is not enforced here
        FilterWithDistributionKeys filterWithKeys = FilterWithDistributionKeys.builder()
                .filter(aFilterThatIsNotAnIdentifierListFilter())
                .distributionKeys(keys(GEN_1, 1.0))
                .build();

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1), Map.of(FILTER_1, filterWithKeys));

        assertEquals(Map.of(GEN_1, 1.0), loadedKeys.distributionKeys(), "The filter type is not inspected, whatever the javadoc promises");
        assertEquals(DistributionKeyStatus.VALID_KEYS, loadedKeys.status());
    }

    @Test
    void aFilterWithDistributionKeysAndNoFilterAtAllIsStillConsideredValid() {
        FilterWithDistributionKeys filterWithKeys = FilterWithDistributionKeys.builder()
                .filter(null)
                .distributionKeys(keys(GEN_1, 1.0))
                .build();

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1), Map.of(FILTER_1, filterWithKeys));

        assertEquals(Map.of(GEN_1, 1.0), loadedKeys.distributionKeys(), "The embedded filter is never dereferenced, it may even be null");
        assertEquals(DistributionKeyStatus.VALID_KEYS, loadedKeys.status());
    }

    @Test
    void aFilterWithOnlyNullDistributionKeysIsReportedAsHavingNoKeys() {
        // every equipment of the filter has a null key: the key map is not empty, so the null keys
        // are met first, one by one
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, null, GEN_2, null)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1), filters);

        assertTrue(loadedKeys.distributionKeys().isEmpty());
        assertEquals(DistributionKeyStatus.MISSING_EQUIPMENT_KEY, loadedKeys.status());
    }

    @Test
    void anEquipmentIdWithoutAnyDistributionKeyIsNotDetected() {
        // a key map covering only part of the equipments a filter selects: what is not in the map
        // is invisible, this test documents the limitation rather than endorsing it
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(
                FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0, GEN_2, null), Set.of(GEN_1, GEN_2)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1), filters);

        assertEquals(DistributionKeyStatus.MISSING_EQUIPMENT_KEY, loadedKeys.status(),
                "GEN_2 carries a null key, so the whole set is discarded");
    }

    @Test
    void aFilterWhoseEquipmentIdsAreUnknownIsStillAccepted() {
        // the embedded filter selects GEN_1 and GEN_2, but only GEN_1 gets a key: the id sets are
        // never compared, so the mismatch is not detected
        Map<UUID, FilterWithDistributionKeys> filters = Map.of(FILTER_1, aFilterWithKeys(keys(GEN_1, 1.0), Set.of(GEN_1, GEN_2)));

        LoadedDistributionKeys loadedKeys = DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1), filters);

        assertEquals(Map.of(GEN_1, 1.0), loadedKeys.distributionKeys(), "The keys are only read through the key map");
        assertEquals(DistributionKeyStatus.VALID_KEYS, loadedKeys.status());
    }

    @Test
    void aNullFiltersMapIsNotSupported() {
        assertThrows(NullPointerException.class, () -> DistributionKeyUtils.reduceDistributionKeys(List.of(FILTER_1), null));
    }

    @Test
    void aNullFilterUuidListIsNotSupported() {
        assertThrows(NullPointerException.class, () -> DistributionKeyUtils.reduceDistributionKeys(null, Map.of()));
    }
}
