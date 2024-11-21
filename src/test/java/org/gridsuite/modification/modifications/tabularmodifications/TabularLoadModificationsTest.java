/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularLoadModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                LoadModificationInfos.builder().equipmentId("v1load").q0(new AttributeModification<>(300., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v2load").q0(new AttributeModification<>(300., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v3load").q0(new AttributeModification<>(300., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.LOAD_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(300., getNetwork().getLoad("v1load").getQ0(), 0.001);
        assertEquals(300., getNetwork().getLoad("v2load").getQ0(), 0.001);
        assertEquals(300., getNetwork().getLoad("v3load").getQ0(), 0.001);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.LOAD_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void checkModification() {
    }
}
