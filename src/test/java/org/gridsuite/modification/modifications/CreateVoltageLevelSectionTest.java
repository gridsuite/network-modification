/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.BusbarSectionPositionAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.CreateVoltageLevelSectionModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.DummyNamingStrategy;
import org.gridsuite.modification.utils.NetworkWithTeePoint;
import org.junit.jupiter.api.Assertions;
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
    protected ModificationModel buildModification() {
        return CreateVoltageLevelSectionModel.builder()
                .voltageLevelId("v1")
                .busbarSectionId("bbs1")
                .busbarIndex(1)
                .isAfterBusbarSectionId(true)
                .leftSwitchKind("BREAKER")
                .rightSwitchKind("DISCONNECTOR")
                .isAllBusbars(false)
                .isSwitchOpen(true)
                .build();
    }

    @Override
    public void checkModification() {
        Network network = getNetwork();
        CreateVoltageLevelSectionModel voltageLevelSectionModel = (CreateVoltageLevelSectionModel) buildModification();
        AbstractModification modification = voltageLevelSectionModel.toModification();

        assertEquals("CREATE_VOLTAGE_LEVEL_SECTION", modification.getName());
        String message = assertThrows(NetworkModificationException.class, () -> modification.check(network)).getMessage();
        assertEquals("BUSBAR_SECTION_NOT_FOUND : 1 is not the busbar index of the busbar section bbs1 in voltage level v1", message);

        message = assertThrows(NetworkModificationException.class, () -> modification.check(network)).getMessage();
        assertEquals("BUSBAR_SECTION_NOT_FOUND : 1 is not the busbar index of the busbar section bbs1 in voltage level v1", message);

        voltageLevelSectionModel.setVoltageLevelId("notFoundVoltageLevel");
        voltageLevelSectionModel.setBusbarSectionId("bbs1");
        voltageLevelSectionModel.setBusbarIndex(1);
        message = assertThrows(NetworkModificationException.class,
                () -> modification.check(network)).getMessage();
        assertEquals("CREATE_VOLTAGE_LEVEL_ERROR : Voltage level not found: notFoundVoltageLevel", message);

        voltageLevelSectionModel.setVoltageLevelId("v1");
        voltageLevelSectionModel.setBusbarSectionId("notFoundBusbar");
        message = assertThrows(NetworkModificationException.class,
                () -> modification.check(network)).getMessage();
        assertEquals("BUSBAR_SECTION_NOT_FOUND : notFoundBusbar not found in voltage level v1", message);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        List<String> busBarIds = new ArrayList<>();
        getNetwork().getBusbarSections().forEach(busbarSection -> busBarIds.add(busbarSection.getId()));
        assertEquals(5, busBarIds.size());
        assertTrue(busBarIds.containsAll(List.of("bbs1", "bbs2", "bbs3", "bbs4", "v1_0_1")));
        Set<String> switchIds = getNetwork().getSwitchStream()
                .map(Switch::getId)
                .collect(Collectors.toSet());
        assertFalse(switchIds.isEmpty());
        assertTrue(switchIds.containsAll(List.of("v1_DISCONNECTOR_0_7", "v1_BREAKER_7_8", "v1_DISCONNECTOR_8_6")));
        Switch disconnector1 = getNetwork().getSwitch("v1_DISCONNECTOR_0_7");
        Switch disconnector2 = getNetwork().getSwitch("v1_DISCONNECTOR_8_6");
        Switch breaker1 = getNetwork().getSwitch("v1_BREAKER_7_8");
        assertTrue(disconnector1.isOpen());
        assertTrue(disconnector2.isOpen());
        assertTrue(breaker1.isOpen());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("CREATE_VOLTAGE_LEVEL_SECTION", modificationModel.getType().toString());
        Map<String, String> updatedValues = modificationModel.getMapMessageValues();
        assertEquals("v1", updatedValues.get("voltageLevelId"));
    }

    @Test
    void testCreateSubReportNode() {
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build();

        CreateVoltageLevelSectionModel modification = (CreateVoltageLevelSectionModel) buildModification();

        modification.createSubReportNode(reportNode);
        assertLogMessage("Adding busbar section on v1", "network.modification.voltageLevel.section.created", reportNode);
    }

    @Test
    void testCreateModificationWithAllBusbars() {
        Network network = getNetwork();
        VoltageLevel voltageLevel = network.getVoltageLevel("v1");
        var bbs = voltageLevel.getNodeBreakerView().newBusbarSection()
                .setId("bbs1_2")
                .setName("bbs1_2")
                .setNode(1)
                .add();
        bbs.newExtension(BusbarSectionPositionAdder.class).withBusbarIndex(1).withSectionIndex(0).add();
        CreateVoltageLevelSectionModel.builder()
                .voltageLevelId("v1")
                .busbarSectionId("bbs1_2")
                .busbarIndex(2)
                .isAfterBusbarSectionId(true)
                .leftSwitchKind("BREAKER")
                .rightSwitchKind("DISCONNECTOR")
                .isAllBusbars(true)
                .build().toModification().apply(network);
        List<String> busBarIds = new ArrayList<>();
        getNetwork().getBusbarSections().forEach(busbarSection -> busBarIds.add(busbarSection.getId()));
        assertEquals(7, busBarIds.size());
        assertTrue(busBarIds.containsAll(List.of("bbs1", "bbs2", "bbs3", "bbs4", "bbs1_2", "v1_0_1", "v1_1_1")));
    }

    @Test
    void testApplyWithNamingStrategy() {
        Network network = getNetwork();
        VoltageLevel voltageLevel = network.getVoltageLevel("v1");
        var bbs = voltageLevel.getNodeBreakerView().newBusbarSection()
                .setId("bbs1_2")
                .setName("bbs1_2")
                .setNode(1)
                .add();
        bbs.newExtension(BusbarSectionPositionAdder.class).withBusbarIndex(1).withSectionIndex(0).add();

        ReportNode report = ReportNode.newRootReportNode()
                .withMessageTemplate("test")
                .build();
        CreateVoltageLevelSectionModel.builder()
                .voltageLevelId("v1")
                .busbarSectionId("bbs1_2")
                .busbarIndex(2)
                .isAfterBusbarSectionId(true)
                .leftSwitchKind("BREAKER")
                .rightSwitchKind("DISCONNECTOR")
                .isAllBusbars(false)
                .build().toModification().apply(network, new DummyNamingStrategy(), report);
        Assertions.assertNotNull(network.getSwitch("DISCONNECTOR_1_7"));
    }

}
