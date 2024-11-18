/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;

import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.ModificationCreation;
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class CompositeModificationsTest extends AbstractNetworkModificationTest {

    @Override
    public void checkModification() {
    }

    @Override
    public void testApply() throws Exception {
        CompositeModificationInfos compositeModificationInfos = (CompositeModificationInfos) buildModification();
        compositeModificationInfos.getModifications().forEach(modificationInfos -> modificationInfos.toModification().apply(getNetwork()));
        assertAfterNetworkModificationApplication();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, false);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                ModificationCreation.getCreationGenerator("v1", "idGenerator", "nameGenerator", "1B", "v2load", "LOAD",
                        "v1"),
                ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED),
                ModificationCreation.getCreationBattery("v1", "idBattery", "nameBattry", "1.1"));
        return CompositeModificationInfos.builder()
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getGenerator("idGenerator"));
        assertNotNull(getNetwork().getLoad("idLoad"));
        assertNotNull(getNetwork().getBattery("idBattery"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }
}
