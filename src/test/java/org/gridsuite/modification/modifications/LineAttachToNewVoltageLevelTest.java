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
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.Arrays;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LineAttachToNewVoltageLevelTest extends AbstractNetworkModificationTest {
    private static LineCreationInfos getAttachmentLine() {
        return LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("attachmentLine")
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
                .switchKinds(Arrays.asList(SwitchKind.BREAKER))
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("1A").busbarSectionId2("1B").build()))
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
                .percent(20.0)
                .attachmentPointId("AttPointId")
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(getNewVoltageLevel())  // create another new VL
                .existingVoltageLevelId(null)
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine())
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
        assertNotNull(getNetwork().getVoltageLevel("newVoltageLevel"));
        // replaced line is gone
        assertNull(getNetwork().getLine("line3"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_ATTACH_TO_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line3", createdValues.get("lineToAttachToId"));
    }

    @Override
    protected void checkModification() {
    }
}
