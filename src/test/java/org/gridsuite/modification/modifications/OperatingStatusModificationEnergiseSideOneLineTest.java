/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class OperatingStatusModificationEnergiseSideOneLineTest extends AbstractNetworkModificationTest {
    private static final String TARGET_LINE_ID = "line2";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a disconnection for terminal1, a connection for terminal2 (must be reconnected/disconnected after testCreate)
        Line line = network.getLine(TARGET_LINE_ID);
        assertNotNull(line);
        line.getTerminal1().disconnect();
        line.getTerminal2().connect();
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(TARGET_LINE_ID)
                .energizedVoltageLevelId("vl1")
                .action(OperatingStatusModificationInfos.ActionType.ENERGISE_END_ONE).build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        // terminal1 is now connected, terminal2 is now disconnected
        Line line = getNetwork().getLine(TARGET_LINE_ID);
        assertNotNull(line);
        assertTrue(line.getTerminal1().isConnected());
        assertFalse(line.getTerminal2().isConnected());
    }

    @Override
    protected void checkModification() {
        // Add a line that can't be disconnected
        Line line = getNetwork().newLine()
                .setId("cantdisconnect")
                .setVoltageLevel1("v1")
                .setVoltageLevel2("v3")
                .setNode1(3)
                .setNode2(7)
                .setX(12)
                .setR(7)
                .add();
        assertNotNull(line);
        OperatingStatusModificationInfos modificationInfos = (OperatingStatusModificationInfos) buildModification();
        modificationInfos.setEquipmentId("cantdisconnect");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> modificationInfos.toModification().apply(getNetwork()));
        assertEquals("OPERATING_STATUS_MODIFICATION_ERROR : Unable to energise equipment end", exception.getMessage());

    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("vl1", createdValues.get("energizedVoltageLevelId"));
        assertEquals("ENERGISE_END_ONE", createdValues.get("action"));
        assertEquals("line2", createdValues.get("equipmentId"));
    }
}
