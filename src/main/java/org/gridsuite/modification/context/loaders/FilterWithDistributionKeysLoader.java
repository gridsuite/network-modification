/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.context.loaders;

import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Resolves filters from their identifiers, together with their distribution keys.
 *
 * <p>Only {@link IdentifierListFilter}s carry distribution keys; any other kind is resolved with an empty
 * distribution key map, and so is an identifier list filter whose equipments have none.
 *
 * <p>Filters that cannot be found are silently omitted rather than resolving to {@code null}.
 *
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
@FunctionalInterface
public interface FilterWithDistributionKeysLoader {

    /**
     * Loads the filters matching the given identifiers, each one associated with its distribution keys.
     *
     * @param filterUuids the identifiers of the filters to load
     * @return the filters found, with their distribution keys, indexed by identifier
     */
    Map<UUID, FilterWithDistributionKeys> load(List<UUID> filterUuids);
}
