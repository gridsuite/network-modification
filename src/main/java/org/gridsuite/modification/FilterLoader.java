/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification;

import org.gridsuite.filter.wip.Filter;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Resolves filter definitions from their identifiers.
 *
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
public interface FilterLoader {

    /**
     * Loads the filters matching the given identifiers.
     *
     * @param filterUuids the identifiers of the filters to load
     * @return the filters found, indexed by their identifier
     */
    Map<UUID, Filter> load(List<UUID> filterUuids);
}
