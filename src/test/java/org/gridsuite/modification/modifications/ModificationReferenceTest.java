/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.utils.ModificationCreation;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
class ModificationReferenceTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, false);
    }

    @Override
    public void checkModification() {
        assertEquals("ModificationReference", buildModification().toModification().getName());
    }

    @Override
    protected void initApplicationContext(AbstractModification modification) {
        modification.initApplicationContext(null, null);
    }

    @Override
    protected ModificationInfos buildModification() {
        ModificationInfos modificationInfo = ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED);
        return ModificationReferenceInfos.builder()
            .referenceType(ModificationReferenceInfos.Type.SAMPLE)
            .referenceId(UUID.randomUUID())
            .referenceInfos(modificationInfo)
            .stashed(false)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Load load = getNetwork().getLoad("idLoad");
        assertNotNull(load);
        assertEquals("nameLoad", load.getOptionalName().orElseThrow());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.MODIFICATION_REFERENCE.name(), modificationInfos.getMessageType());
    }

    @Test
    void testMapMessageValues() {
        ModificationInfos modifications = buildModification();
        assertTrue(modifications.getMapMessageValues().isEmpty());
    }
}
