/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.context.utils;

import lombok.NonNull;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.gridsuite.modification.context.loaders.FilterLoader;
import org.gridsuite.modification.dto.FilterInfos;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
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
     * <p>Filters are returned in the order they are referenced in. A reference that cannot be resolved is silently left
     * out; a reference listed several times yields a single filter, named after the last of its references.
     *
     * @param filterInfosList the references to the filters to resolve
     * @param filterLoader the loader resolving the filters
     * @return the resolved filters, named and ordered after the given references
     */
    public static List<Filter> loadFilterWithNames(List<FilterInfos> filterInfosList, FilterLoader filterLoader) {
        Map<UUID, Filter> filterMap = filterLoader.load(filterInfosList.stream().map(FilterInfos::getId).distinct().toList());
        return getNamedFilters(filterInfosList, filterMap::get);
    }

    /**
     * Same as {@link #loadFilterWithNames(List, FilterLoader)}, the filters being already resolved.
     *
     * <p>Filters absent from the given map are omitted, as required by the {@link FilterLoader} contract.
     */
    public static List<Filter> loadFilterWithNames(List<FilterInfos> filterInfosList, Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys) {
        Map<UUID, Filter> filterMap = filtersWithDistributionKeys.entrySet().stream()
                .filter(e -> e.getValue() != null)
                .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().getFilter()));

        return getNamedFilters(filterInfosList, filterMap::get);
    }

    private static @NonNull List<Filter> getNamedFilters(List<FilterInfos> filterInfosList, SimpleFilterLoader filterLoader) {
        Map<UUID, Filter> resolvedFilters = new LinkedHashMap<>();
        // It keeps the initial filterInfosList order
        filterInfosList.forEach(filterInfos -> {
            Filter filter = filterLoader.load(filterInfos.getId());
            if (filter != null) {
                filter.setName(filterInfos.getName());
                resolvedFilters.putIfAbsent(filterInfos.getId(), filter);
            }
        });
        return List.copyOf(resolvedFilters.values());
    }

    @FunctionalInterface
    interface SimpleFilterLoader {
        Filter load(UUID uuid);
    }
}
