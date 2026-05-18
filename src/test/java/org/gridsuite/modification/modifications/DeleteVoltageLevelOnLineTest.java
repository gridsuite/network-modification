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
import com.powsybl.iidm.network.OperationalLimitsGroup;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.DeleteVoltageLevelOnLineInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;
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
class DeleteVoltageLevelOnLineTest extends AbstractNetworkModificationTest {

    @Override
    public void checkModification() {
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = (DeleteVoltageLevelOnLineInfos) buildModification();
        deleteVoltageLevelOnLineInfos.setLineToAttachTo1Id("notFoundLine");
        assertThrows(NetworkModificationException.class, () -> deleteVoltageLevelOnLineInfos.toModification().check(getNetwork()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createForDeleteVoltageLevelOnLine(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return DeleteVoltageLevelOnLineInfos.builder()
               .stashed(false)
               .lineToAttachTo1Id("l1")
               .lineToAttachTo2Id("l2")
               .replacingLine1Id("replacementLineId")
               .replacingLine1Name("replacementLine")
               .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getVoltageLevel("v1"));
        assertNotNull(getNetwork().getSubstation("s1"));
        assertNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("l2"));
        Line replacementLine = getNetwork().getLine("replacementLineId");
        assertNotNull(replacementLine);
        assertEquals(3, replacementLine.getOperationalLimitsGroups1().size());
        Optional<OperationalLimitsGroup> group2 = replacementLine.getOperationalLimitsGroup1("group2");
        assertTrue(group2.isPresent());
        assertEquals(4, replacementLine.getOperationalLimitsGroups2().size());
        Optional<OperationalLimitsGroup> group3 = replacementLine.getOperationalLimitsGroup2("group3");
        assertTrue(group3.isPresent());
    }

    @Test
    void createWithInvalidLineIdTest() throws Exception {
        // test create with incorrect line id
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = DeleteVoltageLevelOnLineInfos.builder()
                .stashed(false)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("ll")
                .replacingLine1Id("replacementLineId")
                .build();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> deleteVoltageLevelOnLineInfos.toModification().check(getNetwork()));
        assertEquals("LINE_NOT_FOUND : ll", exception.getMessage());
    }

    @Test
    void createNewLineWithExistingIdTest() throws Exception {
        // try to create an already existing line
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = (DeleteVoltageLevelOnLineInfos) buildModification();
        deleteVoltageLevelOnLineInfos.setReplacingLine1Id("l2");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> deleteVoltageLevelOnLineInfos.toModification().check(getNetwork()));
        assertEquals("LINE_ALREADY_EXISTS : l2", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("DELETE_VOLTAGE_LEVEL_ON_LINE", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("l1", createdValues.get("lineToAttachTo1Id"));
        assertEquals("l2", createdValues.get("lineToAttachTo2Id"));
    }

    @Test
    void testMergedLimits() throws IOException {
        testModificationMergedLimits(getNetwork(), buildModification(), "replacementLineId", "/report/delete-voltagelevel-on-line-report.txt");
    }
}
