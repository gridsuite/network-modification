/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.DeleteAttachingLineInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkWithTeePoint;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class DeleteAttachingLineTest extends AbstractNetworkModificationTest {

    @Override
    public void checkModification() {
        DeleteAttachingLineInfos deleteAttachingLineInfos = (DeleteAttachingLineInfos) buildModification();
        deleteAttachingLineInfos.setLineToAttachTo1Id("notFoundLine");
        assertThrows(NetworkModificationException.class, () -> deleteAttachingLineInfos.toModification().check(getNetwork()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return DeleteAttachingLineInfos.builder()
                .stashed(false)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .replacingLine1Id("replacingLineId")
                .replacingLine1Name("replacingLine")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("l2"));
        assertNotNull(getNetwork().getLine("replacingLineId"));
    }

    @Test
    void createWithNoAttachmentPointTest() throws Exception {
        DeleteAttachingLineInfos deleteAttachingLineInfos = DeleteAttachingLineInfos.builder()
                .stashed(false)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l3")
                .attachedLineId("l1")
                .replacingLine1Id("replacementLineId")
                .build();
        // assert throws NetworkModificationException and check error message
        PowsyblException exception = assertThrows(PowsyblException.class, () -> deleteAttachingLineInfos.toModification().apply(getNetwork()));
        assertEquals("Unable to find the attachment point and the tapped voltage level from lines l1, l3 and l1", exception.getMessage());
    }

    @Test
    void createNewLineWithExistingIdTest() throws Exception {
        // try to create an already existing line
        DeleteAttachingLineInfos deleteAttachingLineInfos = (DeleteAttachingLineInfos) buildModification();
        deleteAttachingLineInfos.setReplacingLine1Id("l2");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> deleteAttachingLineInfos.toModification().check(getNetwork()));
        assertEquals("LINE_ALREADY_EXISTS : l2", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("DELETE_ATTACHING_LINE", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("l3", createdValues.get("attachedLineId"));
        assertEquals("l1", createdValues.get("lineToAttachTo1Id"));
        assertEquals("l2", createdValues.get("lineToAttachTo2Id"));
    }
}
