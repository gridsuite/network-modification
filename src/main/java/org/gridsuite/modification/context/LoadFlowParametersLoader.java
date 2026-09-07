/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.context;

import org.gridsuite.modification.dto.LoadFlowParametersInfos;

import java.util.Optional;
import java.util.UUID;

/**
 * Resolves load flow parameters from their identifier.
 *
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
@FunctionalInterface
public interface LoadFlowParametersLoader {

    /**
     * Loads the load flow parameters matching the given identifier.
     *
     * @param parametersUuid the identifier of the parameters to load
     * @return the parameters found, or {@link Optional#empty()} if they do not exist
     */
    Optional<LoadFlowParametersInfos> load(UUID parametersUuid);
}
