/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LineAttachToVoltageLevelTest extends AbstractNetworkModificationTest {
    private static LineCreationInfos getAttachmentLine(String lineName) {
        return LineCreationInfos.builder()
                .stashed(false)
                .equipmentId(lineName)
                .r(50.6)
                .x(25.3)
                .build();
    }

    private static VoltageLevelCreationInfos getNewVoltageLevel() {
        return VoltageLevelCreationInfos.builder()
                .stashed(false)
                .equipmentId("newVoltageLevel")
                .equipmentName("NewVoltageLevel")
                .nominalV(379.3)
                .substationId("s1")
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(List.of(SwitchKind.BREAKER))
                .couplingDevices(Collections.singletonList(CouplingDeviceInfos.builder().busbarSectionId1("bbs.nw").busbarSectionId2("bbs.ne").build()))
                .build();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineAttachToVoltageLevelInfos.builder()
                .stashed(false)
                .lineToAttachToId("line3")
                .percent(10.0)
                .attachmentPointId("AttPointId")   // created VL
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(getNewVoltageLevel())
                .existingVoltageLevelId(null)     // use existing VL
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine("attachmentLine"))   // created Line
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        // new equipments in the network:
        assertNotNull(getNetwork().getLine("attachmentLine"));
        assertNotNull(getNetwork().getLine("nl1"));
        assertNotNull(getNetwork().getLine("nl2"));
        assertNotNull(getNetwork().getVoltageLevel("AttPointId"));
        // replaced line is gone
        assertNull(getNetwork().getLine("line3"));
    }

    private void tryToCreateLineWithExistingId(LineAttachToVoltageLevelInfos tryWithExistingLine, String existingLineId) {
        NetworkModificationRunException exception = assertThrows(NetworkModificationRunException.class, () -> tryWithExistingLine.toModification().check(getNetwork()));
        assertEquals("Line already exists: " + existingLineId, exception.getMessage());
    }

    @Override
    protected void checkModification() {
        LineAttachToVoltageLevelInfos lineAttachToAbsentLine = (LineAttachToVoltageLevelInfos) buildModification();
        lineAttachToAbsentLine.setLineToAttachToId("absent_line_id");
        NetworkModificationRunException exception = assertThrows(NetworkModificationRunException.class, () -> lineAttachToAbsentLine.toModification().check(getNetwork()));
        assertEquals("Line not found: absent_line_id", exception.getMessage());
    }

    @Test
    void testCreateWithExistingEquipments() throws Exception {
        // try to create an already existing line
        LineAttachToVoltageLevelInfos tryWithNewLine1Id = (LineAttachToVoltageLevelInfos) buildModification();
        tryWithNewLine1Id.setNewLine1Id("line1");
        tryToCreateLineWithExistingId(tryWithNewLine1Id, "line1");
        // same test with "newLine2Id"
        LineAttachToVoltageLevelInfos tryWithNewLine2Id = (LineAttachToVoltageLevelInfos) buildModification();
        tryWithNewLine2Id.setNewLine1Id("line3");
        tryToCreateLineWithExistingId(tryWithNewLine2Id, "line3");
        // same test with "attachmentLine"
        LineAttachToVoltageLevelInfos tryWithEquipmentId = (LineAttachToVoltageLevelInfos) buildModification();
        tryWithEquipmentId.setAttachmentLine(getAttachmentLine("line2"));
        tryToCreateLineWithExistingId(tryWithEquipmentId, "line2");
        // try to create an already existing VL
        LineAttachToVoltageLevelInfos tryWithAttachmentPointId = (LineAttachToVoltageLevelInfos) buildModification();
        tryWithAttachmentPointId.setAttachmentPointId("v5");
        NetworkModificationRunException exception = assertThrows(NetworkModificationRunException.class, () -> tryWithAttachmentPointId.toModification().check(getNetwork()));
        assertEquals("Voltage level already exists: v5", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_ATTACH_TO_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line3", createdValues.get("lineToAttachToId"));
    }
}
