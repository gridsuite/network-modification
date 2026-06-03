/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.LinesAttachToSplitLinesModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.utils.NetworkWithTeePoint;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LinesAttachToSplitLinesTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected ModificationModel buildModification() {
        return LinesAttachToSplitLinesModel.builder()
                .stashed(false)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .voltageLevelId("v4")
                .bbsBusId("bbs4")
                .replacingLine1Id("nl1")
                .replacingLine1Name("NewLine1")
                .replacingLine2Id("nl2")
                .replacingLine2Name("NewLine2")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        // 3 lines are gone
        assertNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("l2"));
        assertNull(getNetwork().getLine("l3"));
        // v2 is still here
        assertNotNull(getNetwork().getVoltageLevel("v2"));
        assertEquals(4, getNetwork().getVoltageLevelCount());
        // new lines:
        assertNotNull(getNetwork().getLine("nl1"));
        assertNotNull(getNetwork().getLine("nl2"));
    }

    @Override
    protected void checkModification() {
        // use an unexisting line
        LinesAttachToSplitLinesModel linesAttachToSplitLinesModel = (LinesAttachToSplitLinesModel) buildModification();
        linesAttachToSplitLinesModel.setLineToAttachTo1Id("absent_line_id");
        Exception exception = assertThrows(NetworkModificationException.class, () -> linesAttachToSplitLinesModel.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage(),
                exception.getMessage());
        // try to create an already existing line
        LinesAttachToSplitLinesModel linesAttachToSplitLinesModel1 = (LinesAttachToSplitLinesModel) buildModification();
        linesAttachToSplitLinesModel1.setReplacingLine1Id("l1");
        exception = assertThrows(NetworkModificationException.class, () -> linesAttachToSplitLinesModel1.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(LINE_ALREADY_EXISTS, "l1").getMessage(),
                exception.getMessage());
        // same test on 'replacingLine2Id'
        LinesAttachToSplitLinesModel linesAttachToSplitLinesModel2 = (LinesAttachToSplitLinesModel) buildModification();
        linesAttachToSplitLinesModel2.setReplacingLine2Id("l1");
        exception = assertThrows(NetworkModificationException.class, () -> linesAttachToSplitLinesModel2.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(LINE_ALREADY_EXISTS, "l1").getMessage(),
                exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("LINES_ATTACH_TO_SPLIT_LINES", modificationModel.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() { });
        assertEquals("l3", createdValues.get("attachedLineId"));
    }
}
