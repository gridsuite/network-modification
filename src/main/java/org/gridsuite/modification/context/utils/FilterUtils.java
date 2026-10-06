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
import org.gridsuite.modification.modifications.data.scaling.VariationFilterData;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

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
     * Pairs each referenced filter with the distribution keys loaded for it.
     *
     * <p>One entry per distinct reference, in the order it is referenced in and named after its
     * reference. A reference listed several times yields a single entry, named after the last one. A
     * reference the loader could not resolve yields an unresolved entry, so that the modification can
     * report it.
     *
     * @param filterInfosList the references to pair, in order
     * @param resolvedFilters the filters found with their keys, indexed by identifier
     * @return one entry per distinct reference, resolved or not
     */
    public static List<VariationFilterData> loadFiltersWithDistributionKeys(List<FilterInfos> filterInfosList,
                                                                          Map<UUID, FilterWithDistributionKeys> resolvedFilters) {
        Map<UUID, VariationFilterData> inReferenceOrder = new LinkedHashMap<>();
        filterInfosList.forEach(filterInfos -> {
            UUID filterId = filterInfos.getId();
            FilterWithDistributionKeys resolved = resolvedFilters.get(filterId);
            if (resolved != null && resolved.getFilter() != null) {
                resolved.getFilter().setName(filterInfos.getName());
            }
            inReferenceOrder.putIfAbsent(filterId, new VariationFilterData(
                    resolved != null ? resolved.getFilter() : null,
                    resolved != null ? resolved.getDistributionKeys() : null));
        });
        return List.copyOf(inReferenceOrder.values());
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
