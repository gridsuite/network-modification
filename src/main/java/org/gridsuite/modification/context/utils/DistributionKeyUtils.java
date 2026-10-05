/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.context.utils;

import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.gridsuite.modification.modifications.data.scaling.DistributionKeyStatus;
import org.gridsuite.modification.modifications.data.scaling.LoadedDistributionKeys;

import java.util.*;

/**
 * Gathers the distribution keys of the equipments selected by a set of filters, and checks that they
 * can be used as a whole.
 *
 * @author Kamil MARUT <kamil.marut at rte-france.com>
 */
public final class DistributionKeyUtils {

    private DistributionKeyUtils() {
        // Should not be instantiated
    }

    /**
     * Reduces the distribution keys of the equipments selected by the filters matching the given identifiers.
     *
     * <p>The keys are valid only if every requested filter was found, is an {@code IdentifierListFilter} and
     * gives a non-{@code null} key to each of its equipments, and if no equipment is given a key by more
     * than one filter. The result is therefore all or nothing: an empty map when the keys are not valid.
     *
     * @param filterUuids                 the distinct identifiers of the filters to gather the keys of; a filter listed
     *                                    twice would be seen as selecting the same equipments twice, and would invalidate the keys
     * @param filtersWithDistributionKeys the filters found, with their keys, indexed by identifier
     * @return the distribution keys per equipment id, or an empty map if they are not valid
     */
    public static LoadedDistributionKeys reduceDistributionKeys(List<UUID> filterUuids, Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys) {
        Map<String, Double> distributionKeys = new HashMap<>();

        for (UUID filterUuid : filterUuids) {
            DistributionKeyStatus status = DistributionKeyUtils.reduceIfValid(distributionKeys, filterUuid, filtersWithDistributionKeys);
            if (!DistributionKeyStatus.VALID_KEYS.equals(status)) {
                return new LoadedDistributionKeys(Collections.emptyMap(), status);
            }
        }

        return new LoadedDistributionKeys(distributionKeys, DistributionKeyStatus.VALID_KEYS);
    }

    /**
     * Adds the distribution keys of a single filter to the given map.
     *
     * <p>The map is left inconsistent when this returns {@code false}, so the caller must then discard it.
     *
     * @return {@code true} if the keys were added, {@code false} if the filter was not found or unusable
     */
    private static DistributionKeyStatus reduceIfValid(Map<String, Double> distributionKeys, UUID filterUuid, Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys) {
        if (!filtersWithDistributionKeys.containsKey(filterUuid)) {
            return DistributionKeyStatus.MISSING_FILTER;
        }

        FilterWithDistributionKeys filterWithDistributionKeys = filtersWithDistributionKeys.get(filterUuid);
        if (filterWithDistributionKeys.getDistributionKeys() == null || filterWithDistributionKeys.getDistributionKeys().isEmpty()) {
            return DistributionKeyStatus.FILTER_HAS_NO_KEYS;
        }

        for (Map.Entry<String, Double> entrySet : filterWithDistributionKeys.getDistributionKeys().entrySet()) {
            String equipmentId = entrySet.getKey();
            Double distributionKey = entrySet.getValue();

            if (distributionKeys.containsKey(equipmentId)) {
                return DistributionKeyStatus.DUPLICATED_EQUIPMENT_KEY;
            } else if (distributionKey == null) {
                return DistributionKeyStatus.MISSING_EQUIPMENT_KEY;

            } else {
                distributionKeys.put(equipmentId, distributionKey);
            }
        }
        return DistributionKeyStatus.VALID_KEYS;
    }
}
