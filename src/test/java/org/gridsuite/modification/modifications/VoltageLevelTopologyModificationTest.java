/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelTopologyModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.*;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author REHILI Ghazwa <ghazwarhili@gmail.com>
 */
class VoltageLevelTopologyModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<EquipmentAttributeModificationInfos> equipmentAttributeModificationInfos = new ArrayList<>(
                Arrays.asList(
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1d1")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build(),
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1b")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build(),
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1blcc")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build(),
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1dlcc")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build()
                )
        );
        return VoltageLevelTopologyModificationInfos.builder()
                .stashed(false)
                .equipmentId("v1")
                .equipmentAttributeModification(equipmentAttributeModificationInfos)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        VoltageLevel voltageLevel = getNetwork().getVoltageLevel("v1");
        assertNotNull(voltageLevel);

        // Assert switch states after modification
        Switch switch1 = getNetwork().getSwitch("v1d1");
        assertNotNull(switch1);
        assertFalse(switch1.isOpen());

        Switch switch2 = getNetwork().getSwitch("v1b1");
        assertNotNull(switch2);
        assertFalse(switch2.isOpen());

        Switch switch3 = getNetwork().getSwitch("v1blcc");
        assertNotNull(switch3);
        assertFalse(switch3.isOpen());

        Switch switch4 = getNetwork().getSwitch("v1dlcc");
        assertNotNull(switch4);
        assertFalse(switch4.isOpen());
    }

    private void applyModification(VoltageLevelModificationInfos infos) {
        infos.toModification().apply(getNetwork());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1", createdValues.get("equipmentId"));
    }

    private void testCheckWithVoltageLevelNotFound() {
        VoltageLevelTopologyModificationInfos modificationInfos = VoltageLevelTopologyModificationInfos.builder()
                .equipmentId("v1NotFound")
                .equipmentAttributeModification(List.of())
                .build();

        VoltageLevelTopologyModification voltageLevelTopologyModification = new VoltageLevelTopologyModification(modificationInfos);

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> voltageLevelTopologyModification.check(getNetwork()));

        assertEquals(VOLTAGE_LEVEL_NOT_FOUND, exception.getType());
    }

    private void testCheckWithEmptyEquipmentAttributeModifications() {
        List<EquipmentAttributeModificationInfos> emptyEquipmentAttributeModifications = new ArrayList<>();

        VoltageLevelTopologyModificationInfos modificationInfos = VoltageLevelTopologyModificationInfos.builder()
                .equipmentId("v1")
                .equipmentAttributeModification(emptyEquipmentAttributeModifications)
                .build();

        VoltageLevelTopologyModification voltageLevelTopologyModification = new VoltageLevelTopologyModification(modificationInfos);

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> voltageLevelTopologyModification.check(getNetwork()));

        assertEquals(MODIFY_VOLTAGE_LEVEL_TOPOLOGY_ERROR, exception.getType());
        assertTrue(exception.getMessage().contains("Missing required switches"));
    }

    private void testCheckWithEquipmentAttributeNotFound() {
        List<EquipmentAttributeModificationInfos> equipmentAttributeModifications = new ArrayList<>(
                Arrays.asList(EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1d1NotFound")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build()
                )
        );

        VoltageLevelTopologyModificationInfos modificationInfos = VoltageLevelTopologyModificationInfos.builder()
                .equipmentId("v1")
                .equipmentAttributeModification(equipmentAttributeModifications)
                .build();

        VoltageLevelTopologyModification voltageLevelTopologyModification = new VoltageLevelTopologyModification(modificationInfos);

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> voltageLevelTopologyModification.check(getNetwork()));

        assertEquals(EQUIPMENT_NOT_FOUND, exception.getType());
    }

    @Override
    protected void checkModification() {
        testCheckWithVoltageLevelNotFound();
        testCheckWithEquipmentAttributeNotFound();
        testCheckWithEmptyEquipmentAttributeModifications();
    }
}
