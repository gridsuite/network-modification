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
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.utils.ModificationCreation;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
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
        assertEquals(ModificationType.MODIFICATION_REFERENCE.name(), buildModification().toModification().getName());
    }

    @Override
    protected void initApplicationContext(AbstractModification modification) {
        modification.initApplicationContext(null, null);
    }

    @Override
    protected ModificationInfos buildModification() {
        ModificationInfos compositeInfo = buildCompositeModification();
        return ModificationReferenceInfos.builder()
            .referenceType(ModificationReferenceInfos.Type.BASIC)
            .referenceId(UUID.randomUUID())
            .referenceInfos(compositeInfo)
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
        assertEquals(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }

    private ModificationInfos buildCompositeModification() {
        List<ModificationInfos> modifications = List.of(
            ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED)
        );
        return CompositeModificationInfos.builder()
            .name("composite")
            .modificationsInfos(modifications)
            .stashed(false)
            .build();
    }
}
