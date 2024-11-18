/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LineSplitWithVoltageLevelTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineSplitWithVoltageLevelInfos.builder()
            .stashed(false)
            .lineToSplitId("line2")
            .percent(10.0)
            .mayNewVoltageLevelInfos(null)
            .existingVoltageLevelId("v4")
            .bbsOrBusId("1.A")
            .newLine1Id("nl1v")
            .newLine1Name("NewLine1")
            .newLine2Id("nl2v")
            .newLine2Name("NewLine2")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNull(getNetwork().getLine("line2"));
        assertNotNull(getNetwork().getLine("nl1v"));
        assertNotNull(getNetwork().getLine("nl2v"));
    }

    @Override
    protected void checkModification() {
        // try to create an already existing line
        LineSplitWithVoltageLevelInfos tryWithNewLine1Id = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithNewLine1Id.setNewLine1Id("line1");
        Exception exception = assertThrows(NetworkModificationException.class, () -> tryWithNewLine1Id.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(LINE_ALREADY_EXISTS, "line1").getMessage(),
                exception.getMessage());

        // same test with "newLine2Id"
        LineSplitWithVoltageLevelInfos tryWithNewLine2Id = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithNewLine2Id.setNewLine2Id("line1");
        exception = assertThrows(NetworkModificationException.class, () -> tryWithNewLine2Id.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(LINE_ALREADY_EXISTS, "line1").getMessage(),
                exception.getMessage());

        // testCreateWithWrongBusBar
        // not existing busbar
        LineSplitWithVoltageLevelInfos tryWithBadId = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithBadId.setBbsOrBusId("999A");
        exception = assertThrows(NetworkModificationException.class, () -> tryWithBadId.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "999A").getMessage(),
                exception.getMessage());

        // try with a switch, not a busbar
        LineSplitWithVoltageLevelInfos tryWithSwitchId = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithSwitchId.setBbsOrBusId("v1d1");
        exception = assertThrows(NetworkModificationException.class, () -> tryWithSwitchId.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "v1d1").getMessage(),
                exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_SPLIT_WITH_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line2", createdValues.get("lineToSplitId"));
    }
}
