/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.OperationalLimitsGroup;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.DeleteAttachingLineModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.utils.NetworkWithTeePoint;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.utils.MergingLimitsTestUtils.testModificationMergedLimits;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class DeleteAttachingLineTest extends AbstractNetworkModificationTest {

    @Override
    public void checkModification() {
        DeleteAttachingLineModel deleteAttachingLineModel = (DeleteAttachingLineModel) buildModification();
        deleteAttachingLineModel.setLineToAttachTo1Id("notFoundLine");
        assertThrows(NetworkModificationException.class, () -> deleteAttachingLineModel.toModification().check(getNetwork()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected ModificationModel buildModification() {
        return DeleteAttachingLineModel.builder()
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
        Line line = getNetwork().getLine("replacingLineId");
        // side 1
        assertNotNull(line);
        assertNotNull(line.getOperationalLimitsGroups1());
        assertEquals(3, line.getOperationalLimitsGroups1().size());
        Optional<OperationalLimitsGroup> group2 = line.getOperationalLimitsGroup1("group2");
        assertTrue(group2.isPresent());

        // side 2
        assertNotNull(line.getOperationalLimitsGroups2());
        assertEquals(4, line.getOperationalLimitsGroups2().size());
        Optional<OperationalLimitsGroup> group3 = line.getOperationalLimitsGroup2("group3");
        assertTrue(group3.isPresent());
    }

    @Test
    void createWithNoAttachmentPointTest() {
        DeleteAttachingLineModel deleteAttachingLineModel = DeleteAttachingLineModel.builder()
            .lineToAttachTo1Id("l1")
            .lineToAttachTo2Id("l3")
            .attachedLineId("l1")
            .replacingLine1Id("replacementLineId")
            .build();
        // assert throws NetworkModificationException and check error message
        PowsyblException exception = assertThrows(PowsyblException.class, () -> deleteAttachingLineModel.toModification().apply(getNetwork()));
        assertEquals("Unable to find the attachment point and the tapped voltage level from lines l1, l3 and l1", exception.getMessage());
    }

    @Test
    void createNewLineWithExistingIdTest() {
        // try to create an already existing line
        DeleteAttachingLineModel deleteAttachingLineModel = (DeleteAttachingLineModel) buildModification();
        deleteAttachingLineModel.setReplacingLine1Id("l2");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> deleteAttachingLineModel.toModification().check(getNetwork()));
        assertEquals("LINE_ALREADY_EXISTS : l2", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("DELETE_ATTACHING_LINE", modificationModel.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        });
        assertEquals("l3", createdValues.get("attachedLineId"));
        assertEquals("l1", createdValues.get("lineToAttachTo1Id"));
        assertEquals("l2", createdValues.get("lineToAttachTo2Id"));
    }

    @Test
    void testMergedLimits() throws IOException {
        testModificationMergedLimits(getNetwork(), buildModification(), "replacingLineId", "/report/delete-attaching-line-report.txt");
    }
}
