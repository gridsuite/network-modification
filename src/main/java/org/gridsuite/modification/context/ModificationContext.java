/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.context;

import lombok.Builder;

/**
 * Everything a modification may need to be built from its description.
 *
 * <p>Dependencies are resolved <b>once, before the modification is built</b>, and then held as plain data:
 * a loader is never kept as a field of a modification. This is what makes a built modification a value —
 * serializable, replayable, and testable without any mock.
 *
 * <p>Adding a new kind of dependency means adding one component here. Existing call sites keep compiling,
 * since the builder leaves unset components absent, and each modification only reads what it needs.
 *
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
@Builder
public record ModificationContext(FilterLoader filterLoader,
                                  LoadFlowParametersLoader loadFlowParametersLoader) {

    private static final FilterLoader NO_FILTER_LOADER = filterUuids -> {
        throw missingLoader("filter loader");
    };

    private static final LoadFlowParametersLoader NO_LOAD_FLOW_PARAMETERS_LOADER = parametersUuid -> {
        throw missingLoader("load flow parameters loader");
    };

    /**
     * Unset components are replaced by loaders failing explicitly, so that a missing one surfaces as a clear
     * wiring error when a modification actually needs it, rather than as a silently empty resolution.
     */
    public ModificationContext {
        filterLoader = filterLoader != null ? filterLoader : NO_FILTER_LOADER;
        loadFlowParametersLoader = loadFlowParametersLoader != null ? loadFlowParametersLoader : NO_LOAD_FLOW_PARAMETERS_LOADER;
    }

    /** Context for modifications that resolve nothing. */
    public static ModificationContext empty() {
        return ModificationContext.builder().build();
    }

    private static IllegalStateException missingLoader(String loaderName) {
        return new IllegalStateException("This modification requires a " + loaderName + ", none was provided in the modification context");
    }
}
