/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class OperatingStatusModificationEnergiseSideTwoLineTest extends AbstractNetworkModificationTest {
    private static final String TARGET_LINE_ID = "line2";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a connection for terminal1, a disconnection for terminal2 (must be disconnected/reconnected after testCreate)
        Line line = network.getLine(TARGET_LINE_ID);
        assertNotNull(line);
        line.getTerminal1().connect();
        line.getTerminal2().disconnect();
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(TARGET_LINE_ID)
                .energizedVoltageLevelId("vl2")
                .action(OperatingStatusModificationInfos.ActionType.ENERGISE_END_TWO).build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        // terminal1 is now disconnected, terminal2 is now reconnected
        Line line = getNetwork().getLine(TARGET_LINE_ID);
        assertNotNull(line);
        assertFalse(line.getTerminal1().isConnected());
        assertTrue(line.getTerminal2().isConnected());
    }

    @Override
    protected void checkModification() {
        // Add a line that can't be disconnected
        Line line = getNetwork().newLine()
                .setId("cantdisconnect")
                .setVoltageLevel1("v1")
                .setVoltageLevel2("v3")
                .setNode1(100)
                .setNode2(100)
                .setX(12)
                .setR(7)
                .add();
        assertNotNull(line);
        OperatingStatusModificationInfos modificationInfos = (OperatingStatusModificationInfos) buildModification();
        modificationInfos.setEquipmentId("cantdisconnect");
        NetworkModificationRunException exception = assertThrows(NetworkModificationRunException.class, () -> modificationInfos.toModification().apply(getNetwork()));
        assertEquals("OPERATING_STATUS_MODIFICATION_ERROR : Unable to energise equipment end", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("vl2", createdValues.get("energizedVoltageLevelId"));
        assertEquals("ENERGISE_END_TWO", createdValues.get("action"));
        assertEquals("line2", createdValues.get("equipmentId"));
    }

    @Test
    void testCreateSubReportNode() {
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build();

        OperatingStatusModificationInfos modification = (OperatingStatusModificationInfos) buildModification();

        modification.createSubReportNode(reportNode);
        assertLogMessage("Energise " + TARGET_LINE_ID, "network.modification.OPERATING_STATUS_MODIFICATION_ENERGISE_END_TWO", reportNode);
    }
}
