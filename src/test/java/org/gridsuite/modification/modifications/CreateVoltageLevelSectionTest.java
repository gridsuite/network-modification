/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CreateVoltageLevelSectionInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkWithTeePoint;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Rehili Ghazwa <ghazwa.rehili at rte-france.com>
 */
class CreateVoltageLevelSectionTest extends AbstractNetworkModificationTest {

    @Override
    protected ModificationInfos buildModification() {
        return CreateVoltageLevelSectionInfos.builder()
                .stashed(false)
                .voltageLevelId("v1")
                .busbarSectionId("bbs1")
                .busbarCount(1)
                .sectionCount(1)
                .isAfterBusbarSectionId(true)
                .leftSwitchKind("BREAKER")
                .rightSwitchKind("DISCONNECTOR")
                .isAllBusbars(false)
                .isSwitchOpen(false)
                .build();
    }

    @Override
    public void checkModification() {
        Network network = getNetwork();
        CreateVoltageLevelSectionInfos voltageLevelSectionInfos = (CreateVoltageLevelSectionInfos) buildModification();
        AbstractModification modification = voltageLevelSectionInfos.toModification();

        assertEquals("CREATE_VOLTAGE_LEVEL_SECTION", modification.getName());
        String message = assertThrows(NetworkModificationException.class, () -> modification.check(network)).getMessage();
        assertEquals("BUSBAR_SECTION_NOT_FOUND", message);

        message = assertThrows(NetworkModificationException.class, () -> modification.check(network)).getMessage();
        assertEquals("BUSBAR_SECTION_NOT_FOUND", message);

        voltageLevelSectionInfos.setVoltageLevelId("notFoundVoltageLevel");
        voltageLevelSectionInfos.setBusbarSectionId("bbs1");
        voltageLevelSectionInfos.setBusbarCount(1);
        voltageLevelSectionInfos.setSectionCount(1);
        message = assertThrows(NetworkModificationException.class,
                () -> modification.check(network)).getMessage();
        assertEquals("CREATE_VOLTAGE_LEVEL_ERROR : Voltage level not found: notFoundVoltageLevel", message);

        voltageLevelSectionInfos.setVoltageLevelId("v1");
        voltageLevelSectionInfos.setBusbarSectionId("notFoundBusbar");
        message = assertThrows(NetworkModificationException.class,
                () -> modification.check(network)).getMessage();
        assertEquals("BUSBAR_SECTION_NOT_FOUND : notFoundBusbar", message);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        List<String> busBarIds = new ArrayList<>();
        getNetwork().getBusbarSections().forEach(busbarSection -> busBarIds.add(busbarSection.getId()));
        assertTrue(busBarIds.size() > 4);
        assertTrue(busBarIds.containsAll(List.of("bbs1", "bbs2", "bbs3", "bbs4")));
        Set<String> switchIds = getNetwork().getSwitchStream()
                .map(Switch::getId)
                .collect(Collectors.toSet());
        assertFalse(switchIds.isEmpty());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("CREATE_VOLTAGE_LEVEL_SECTION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1", updatedValues.get("voltageLevelId"));
    }

    @Test
    void testCreateSubReportNode() {
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("test")
                .build();

        CreateVoltageLevelSectionInfos modification = (CreateVoltageLevelSectionInfos) buildModification();

        modification.createSubReportNode(reportNode);
        assertLogMessage("Adding busbar section on v1", "network.modification.voltageLevel.section.created", reportNode);
    }
}
