/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.LinesAttachToSplitLinesInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkWithTeePoint;
import java.util.Map;
import java.util.UUID;

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
    protected ModificationInfos buildModification() {
        return LinesAttachToSplitLinesInfos.builder()
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
        // v2 is gone
        assertNull(getNetwork().getVoltageLevel("v2"));
        assertEquals(3, getNetwork().getVoltageLevelCount());
        // new lines:
        assertNotNull(getNetwork().getLine("nl1"));
        assertNotNull(getNetwork().getLine("nl2"));
    }

    @Override
    protected void checkModification() {
        // use an unexisting line
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos = (LinesAttachToSplitLinesInfos) buildModification();
        linesAttachToSplitLinesInfos.setLineToAttachTo1Id("absent_line_id");
        Exception exception = assertThrows(NetworkModificationRunException.class, () -> linesAttachToSplitLinesInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationRunException("absent_line_id").getMessage(),
                exception.getMessage());
        // try to create an already existing line
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos1 = (LinesAttachToSplitLinesInfos) buildModification();
        linesAttachToSplitLinesInfos1.setReplacingLine1Id("l1");
        exception = assertThrows(NetworkModificationRunException.class, () -> linesAttachToSplitLinesInfos1.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationRunException("l1").getMessage(),
                exception.getMessage());
        // same test on 'replacingLine2Id'
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos2 = (LinesAttachToSplitLinesInfos) buildModification();
        linesAttachToSplitLinesInfos2.setReplacingLine2Id("l1");
        exception = assertThrows(NetworkModificationRunException.class, () -> linesAttachToSplitLinesInfos2.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationRunException("l1").getMessage(),
                exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINES_ATTACH_TO_SPLIT_LINES", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("l3", createdValues.get("attachedLineId"));
    }
}
