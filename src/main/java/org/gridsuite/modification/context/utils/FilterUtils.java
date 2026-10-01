/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.context.utils;

import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.gridsuite.modification.context.loaders.FilterLoader;
import org.gridsuite.modification.dto.FilterInfos;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public final class FilterUtils {

    private FilterUtils() {
        // Should not be instantiated
    }

    /**
     * Resolves the given filter references, each resolved filter being named after the reference pointing to it.
     *
     * <p>Filters are returned in the order they are referenced in, so the result does not depend on the
     * iteration order of the map the loader returned. A reference that cannot be resolved is silently left
     * out; a reference listed several times yields a single filter, named after the last of its references.
     *
     * @param filterInfosList the references to the filters to resolve
     * @param filterLoader the loader resolving the filters
     * @return the resolved filters, named and ordered after the given references
     */
    public static List<Filter> loadFilterWithNames(List<FilterInfos> filterInfosList, FilterLoader filterLoader) {
        Map<UUID, Filter> filterMap = filterLoader.load(filterInfosList.stream().map(FilterInfos::getId).distinct().toList());
        // The order of the filters is only meaningful because they are collected here and not taken from
        // filterMap.values(), whose iteration order is unspecified.
        Map<UUID, Filter> resolvedFilters = new LinkedHashMap<>();
        filterInfosList.forEach(filterInfos -> {
            Filter filter = filterMap.get(filterInfos.getId());
            if (filter != null) {
                filter.setName(filterInfos.getName());
                resolvedFilters.putIfAbsent(filterInfos.getId(), filter);
            }
        });
        return List.copyOf(resolvedFilters.values());
    }

    /**
     * Same as {@link #loadFilterWithNames(List, FilterLoader)}, the filters being already resolved.
     *
     * <p>Filters absent from the given map are omitted, as required by the {@link FilterLoader} contract.
     */
    public static List<Filter> loadFilterWithNames(List<FilterInfos> filterInfosList, Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys) {
        return loadFilterWithNames(filterInfosList, alreadyLoaded(filtersWithDistributionKeys));
    }

    /**
     * Builds a {@link FilterLoader} serving already resolved filters, keeping the omission of the filters
     * that cannot be found defined in a single place.
     */
    private static FilterLoader alreadyLoaded(Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys) {
        Objects.requireNonNull(filtersWithDistributionKeys, "The already resolved filters must not be null");
        return filterUuids -> filterUuids.stream()
                .filter(filtersWithDistributionKeys::containsKey)
                .collect(Collectors.toMap(Function.identity(), uuid -> filtersWithDistributionKeys.get(uuid).getFilter()));
    }
}
