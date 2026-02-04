/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;

import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.ModificationCreation;
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class CompositeModificationsTest extends AbstractNetworkModificationTest {

    @Override
    public void checkModification() {
        // TODO
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, false);
    }

    @Override
    protected ModificationInfos buildModification() {
        CompositeModificationInfos subSubCompo = CompositeModificationInfos.builder()
                .modifications(
                        List.of(ModificationCreation.getModificationGenerator( "idGenerator", "other idGenerator name again"))
                ).build();
        CompositeModificationInfos subCompo1 = CompositeModificationInfos.builder()
                .modifications(
                        List.of(ModificationCreation.getModificationGenerator( "idGenerator", "other idGenerator name"))
                ).build();
        CompositeModificationInfos subCompo2 = CompositeModificationInfos.builder()
                .modifications(
                        List.of(
                                subSubCompo,
                                ModificationCreation.getModificationGenerator( "idGenerator", "even newer idGenerator name")
                        )
                ).build();
        List<ModificationInfos> modifications = List.of(
                subCompo1,
                ModificationCreation.getModificationGenerator( "idGenerator", "new idGenerator name"),
                ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED),
                ModificationCreation.getCreationBattery("v1", "idBattery", "nameBattery", "1.1"),
                subCompo2
        );
        return CompositeModificationInfos.builder()
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Generator gen = getNetwork().getGenerator("idGenerator");
        assertNotNull(gen);
        assertEquals("even newer idGenerator name", gen.getOptionalName().orElseThrow());
        assertNotNull(getNetwork().getLoad("idLoad"));
        assertNotNull(getNetwork().getBattery("idBattery"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }
}
