/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class EquipmentAttributeModificationTest extends AbstractNetworkModificationTest {

    @Override
    public void checkModification() {
        EquipmentAttributeModificationInfos switchStatusModificationInfos = (EquipmentAttributeModificationInfos) buildModification();
        switchStatusModificationInfos.setEquipmentId("notFound");
        assertThrows(NetworkModificationException.class, () -> switchStatusModificationInfos.toModification().check(getNetwork()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected EquipmentAttributeModificationInfos buildModification() {
        return EquipmentAttributeModificationInfos.builder()
            .stashed(false)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertTrue(getNetwork().getSwitch("v1b1").isOpen());
    }

    @Test
    void testWithErrors() throws Exception {
        // bad equipment attribute name
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .stashed(false)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("close") // bad
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> switchStatusModificationInfos.toModification().apply(getNetwork()));
        assertEquals("SWITCH attribute 'close' not editable", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("EQUIPMENT_ATTRIBUTE_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("open", createdValues.get("equipmentAttributeName"));
        assertEquals("v1b1", createdValues.get("equipmentId"));
        assertEquals("true", createdValues.get("equipmentAttributeValue"));
    }
}
