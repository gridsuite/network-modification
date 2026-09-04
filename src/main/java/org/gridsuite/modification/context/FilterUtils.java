/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.context;

import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.dto.FilterInfos;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public final class FilterUtils {

    private FilterUtils() {
        // Should not be instantiated
    }

    public static List<Filter> loadFilterWithNames(List<FilterInfos> filterInfosList, FilterLoader filterLoader) {
        Map<UUID, Filter> filterMap = filterLoader.load(filterInfosList.stream().map(FilterInfos::getId).distinct().toList());
        filterInfosList.forEach(filterInfos ->
                Optional.ofNullable(filterMap.get(filterInfos.getId()))
                        .ifPresent(filter -> filter.setName(filterInfos.getName())));
        return filterMap.values().stream().toList();
    }
}
