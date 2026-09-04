/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.context;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
class ModificationContextTest {

    @Test
    void providedLoadersAreReturnedAsIs() {
        FilterLoader filterLoader = filterUuids -> Map.of();
        LoadFlowParametersLoader loadFlowParametersLoader = parametersUuid -> Optional.empty();

        ModificationContext context = ModificationContext.builder()
                .filterLoader(filterLoader)
                .loadFlowParametersLoader(loadFlowParametersLoader)
                .build();

        assertSame(filterLoader, context.filterLoader());
        assertSame(loadFlowParametersLoader, context.loadFlowParametersLoader());
    }

    @Test
    void anEmptyContextFailsExplicitlyWhenAFilterLoaderIsUsed() {
        FilterLoader filterLoader = ModificationContext.empty().filterLoader();
        List<UUID> filterUuids = List.of(UUID.randomUUID());

        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> filterLoader.load(filterUuids));

        assertEquals("This modification requires a filter loader, none was provided in the modification context",
                exception.getMessage(),
                "A missing loader must surface as a wiring error, not as a silently empty resolution");
    }

    @Test
    void anEmptyContextFailsExplicitlyWhenLoadFlowParametersAreUsed() {
        LoadFlowParametersLoader loadFlowParametersLoader = ModificationContext.empty().loadFlowParametersLoader();
        UUID parametersUuid = UUID.randomUUID();

        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> loadFlowParametersLoader.load(parametersUuid));

        assertEquals("This modification requires a load flow parameters loader, none was provided in the modification context",
                exception.getMessage());
    }

    @Test
    void aPartiallyConfiguredContextOnlyFailsOnTheMissingLoader() {
        ModificationContext context = ModificationContext.builder()
                .filterLoader(filterUuids -> Map.of())
                .build();

        assertEquals(Map.of(), context.filterLoader().load(List.of(UUID.randomUUID())),
                "A component set in the builder must be usable even when the others are left out");

        LoadFlowParametersLoader loadFlowParametersLoader = context.loadFlowParametersLoader();
        UUID parametersUuid = UUID.randomUUID();
        assertThrows(IllegalStateException.class, () -> loadFlowParametersLoader.load(parametersUuid));
    }
}
