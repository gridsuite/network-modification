/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.MoveFeederBayInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.MoveVoltageLevelFeederBaysInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.ModificationType.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS;
import static org.gridsuite.modification.utils.NetworkUtil.createBusBarSection;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
class MoveVoltageLevelFeederBaysTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        createBusBarSection(network.getVoltageLevel("v3"), "3B", "3B", 20);
        createBusBarSection(network.getVoltageLevel("v1"), "1.2", "1.2", 20);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        List<MoveFeederBayInfos> moveFeederBayInfos = new ArrayList<>();
        moveFeederBayInfos.add(MoveFeederBayInfos.builder().equipmentId("v3load")
            .busbarSectionId("3B")
            .connectionSide(null)
            .connectionName("v3loadrename")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        moveFeederBayInfos.add(MoveFeederBayInfos.builder()
            .equipmentId("line2")
            .busbarSectionId("3B")
            .connectionSide(ThreeSides.TWO.toString())
            .connectionName("line2NameV3")
            .connectionPosition(10)
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .build());
        return MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v3")
            .feederBays(moveFeederBayInfos)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        // v3load
        Load v3load = getNetwork().getLoad("v3load");
        assertNotNull(v3load);
        String busbarId = ModificationUtils.getInstance().getBusOrBusbarSection(v3load.getTerminal());
        assertEquals("3B", busbarId);
        ConnectablePosition connectablePosition = v3load.getExtension(ConnectablePosition.class);
        assertNotNull(connectablePosition);
        ConnectablePosition.Feeder feeder = connectablePosition.getFeeder();
        assertNotNull(feeder);
        assertTrue(feeder.getName().isPresent());
        assertEquals("v3loadrename", feeder.getName().get());
        assertTrue(feeder.getOrder().isPresent());
        assertEquals(4, feeder.getOrder().get());
        assertEquals(ConnectablePosition.Direction.TOP, feeder.getDirection());
        // line2
        Line line2 = getNetwork().getLine("line2");
        assertNotNull(line2);
        String line2BusbarId = ModificationUtils.getInstance().getBusOrBusbarSection(line2.getTerminal2());
        assertEquals("3B", line2BusbarId);
        ConnectablePosition lineConnectablePosition = line2.getExtension(ConnectablePosition.class);
        assertNotNull(lineConnectablePosition);
        ConnectablePosition.Feeder feeder2 = lineConnectablePosition.getFeeder2();
        assertNotNull(feeder2);
        assertTrue(feeder2.getName().isPresent());
        assertEquals("line2NameV3", feeder2.getName().get());
        assertTrue(feeder2.getOrder().isPresent());
        assertEquals(10, feeder2.getOrder().get());
        assertEquals(ConnectablePosition.Direction.BOTTOM, feeder2.getDirection());
    }

    @Override
    protected void checkModification() {
        Network network = getNetwork();
        testConnectableNotInjectionOrBranch(network);
        testConnectableNotFound(network);
        testVoltageLevelNotFound(network);
        testBusBarNotFound(network);
    }

    private void testVoltageLevelNotFound(Network network) {
        MoveVoltageLevelFeederBaysInfos moveVoltageLevelFeederBaysInfos = MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("notFound")
            .build();
        MoveVoltageLevelFeederBays moveVoltageLevelFeederBays = (MoveVoltageLevelFeederBays) moveVoltageLevelFeederBaysInfos.toModification();
        String message = assertThrows(NetworkModificationException.class, () -> moveVoltageLevelFeederBays.check(network)).getMessage();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR : Voltage level notFound is not found", message);
    }

    private void testBusBarNotFound(Network network) {
        List<MoveFeederBayInfos> moveFeederBayInfos = new ArrayList<>();
        moveFeederBayInfos.add(MoveFeederBayInfos.builder()
            .equipmentId("v3load")
            .busbarSectionId("notFound")
            .connectionSide("3B")
            .connectionName("v3loadrename")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        MoveVoltageLevelFeederBaysInfos moveVoltageLevelFeederBaysInfos = MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v3")
            .feederBays(moveFeederBayInfos)
            .build();
        MoveVoltageLevelFeederBays moveVoltageLevelFeederBays = (MoveVoltageLevelFeederBays) moveVoltageLevelFeederBaysInfos.toModification();
        String message = assertThrows(NetworkModificationException.class, () -> moveVoltageLevelFeederBays.check(network)).getMessage();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR : Bus or busbar section notFound where connectable v3load is supposed to be is not found in voltage level v3", message);
    }

    private void testConnectableNotFound(Network network) {
        List<MoveFeederBayInfos> moveFeederBayInfos = new ArrayList<>();
        moveFeederBayInfos.add(MoveFeederBayInfos.builder()
            .equipmentId("notFound")
            .busbarSectionId("3A")
            .connectionSide(null)
            .connectionName("notFoundMoved")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        MoveVoltageLevelFeederBaysInfos moveVoltageLevelFeederBaysInfos = MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v3")
            .feederBays(moveFeederBayInfos)
            .build();
        MoveVoltageLevelFeederBays moveVoltageLevelFeederBays = (MoveVoltageLevelFeederBays) moveVoltageLevelFeederBaysInfos.toModification();
        String message = assertThrows(NetworkModificationException.class, () -> moveVoltageLevelFeederBays.check(network)).getMessage();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR : Connectable notFound not found", message);
    }

    private void testConnectableNotInjectionOrBranch(Network network) {
        List<MoveFeederBayInfos> moveFeederBayInfos = new ArrayList<>();
        moveFeederBayInfos.add(MoveFeederBayInfos.builder()
            .equipmentId("trf6")
            .busbarSectionId("1A")
            .connectionSide("ONE")
            .connectionName("trf6Moved")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        MoveVoltageLevelFeederBaysInfos moveVoltageLevelFeederBaysInfos = MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v2")
            .feederBays(moveFeederBayInfos)
            .build();
        MoveVoltageLevelFeederBays moveVoltageLevelFeederBays = (MoveVoltageLevelFeederBays) moveVoltageLevelFeederBaysInfos.toModification();
        String message = assertThrows(NetworkModificationException.class, () -> moveVoltageLevelFeederBays.check(network)).getMessage();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR : ConnectablePositionModification is not implemented for class com.powsybl.network.store.iidm.impl.ThreeWindingsTransformerImpl", message);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v3", createdValues.get("voltageLevelId"));
    }

    @Test
    void testGetTerminal() {
        Network network = getNetwork();
        // is not an injection or a branch
        MoveFeederBayInfos transformerInfo = MoveFeederBayInfos.builder()
            .equipmentId("trf6")
            .busbarSectionId("1A")
            .connectionSide("ONE")
            .connectionName("trf6Moved")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        MoveVoltageLevelFeederBaysInfos moveVoltageLevelFeederBaysInfos = MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v2")
            .feederBays(List.of(transformerInfo))
            .build();
        MoveVoltageLevelFeederBays moveVoltageLevelFeederBays = (MoveVoltageLevelFeederBays) moveVoltageLevelFeederBaysInfos.toModification();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS", moveVoltageLevelFeederBays.getName());
        String message = assertThrows(NetworkModificationException.class, () -> moveVoltageLevelFeederBays.getTerminal(network, transformerInfo)).getMessage();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR : ConnectablePositionModification is not implemented for class com.powsybl.network.store.iidm.impl.ThreeWindingsTransformerImpl", message);
        MoveFeederBayInfos invalidSideInfo = MoveFeederBayInfos.builder()
            .equipmentId("line1")
            .busbarSectionId("random")
            .connectionSide("THREE")
            .connectionName("line1")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        message = assertThrows(NetworkModificationException.class, () -> moveVoltageLevelFeederBays.getTerminal(network, invalidSideInfo)).getMessage();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR : Invalid connection side: THREE for branch line1", message);
        MoveFeederBayInfos injectionInfo = MoveFeederBayInfos.builder()
            .equipmentId("v3Battery")
            .busbarSectionId("3A")
            .connectionSide(null)
            .connectionName("v3Battery")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        Terminal terminal = moveVoltageLevelFeederBays.getTerminal(network, injectionInfo);
        assertEquals("v3", terminal.getVoltageLevel().getId());
        assertEquals("v3Battery", terminal.getConnectable().getId());
        // branch side 2 with no error
        MoveFeederBayInfos branchSide2Info = MoveFeederBayInfos.builder()
            .equipmentId("line1")
            .busbarSectionId("1A")
            .connectionSide("TWO")
            .connectionName("line1")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        terminal = moveVoltageLevelFeederBays.getTerminal(network, branchSide2Info);
        assertEquals("v4", terminal.getVoltageLevel().getId());
        assertEquals("line1", terminal.getConnectable().getId());
        assertEquals(TwoSides.TWO, network.getLine("line1").getSide(terminal));
        // branch side 1 with no error
        MoveFeederBayInfos branchSide1Info = MoveFeederBayInfos.builder()
            .equipmentId("line2")
            .busbarSectionId("1.1")
            .connectionSide("ONE")
            .connectionName("line2")
            .connectionPosition(21)
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .build();
        terminal = moveVoltageLevelFeederBays.getTerminal(network, branchSide1Info);
        assertEquals("v1", terminal.getVoltageLevel().getId());
        assertEquals("line2", terminal.getConnectable().getId());
        assertEquals(TwoSides.ONE, network.getLine("line2").getSide(terminal));
    }

    @Test
    void testCreateSubReportNode() {
        ReportNode reportNode = ReportNode.newRootReportNode()
            .withAllResourceBundlesFromClasspath()
            .withMessageTemplate("test")
            .build();
        MoveVoltageLevelFeederBaysInfos modification = (MoveVoltageLevelFeederBaysInfos) buildModification();
        MoveVoltageLevelFeederBays moveVoltageLevelFeederBays = (MoveVoltageLevelFeederBays) modification.toModification();
        assertEquals(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS.toString(), moveVoltageLevelFeederBays.getName());
        modification.createSubReportNode(reportNode);
        assertLogMessage("Moving connections in v3", "network.modification.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS", reportNode);
    }

    @Test
    void testConnectablePositionModificationOnSide1() {
        Network network = getNetwork();
        MoveFeederBayInfos moveFeederBayInfos = MoveFeederBayInfos.builder()
            .equipmentId("line2")
            .busbarSectionId("1.1")
            .connectionSide("TWO")
            .connectionName("line2test")
            .connectionPosition(21)
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .build();
        MoveVoltageLevelFeederBays moveVoltageLevelFeederBays = new MoveVoltageLevelFeederBays(MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v1")
            .feederBays(List.of(moveFeederBayInfos))
            .build());
        moveVoltageLevelFeederBays.apply(network);
        Line line = network.getLine("line2");
        assertNotNull(line);
        String line2BusbarId = ModificationUtils.getInstance().getBusOrBusbarSection(line.getTerminal1());
        assertEquals("1.1", line2BusbarId);
        ConnectablePosition lineConnectablePosition = line.getExtension(ConnectablePosition.class);
        assertNotNull(lineConnectablePosition);
        ConnectablePosition.Feeder feeder1 = lineConnectablePosition.getFeeder1();
        assertNotNull(feeder1);
        assertTrue(feeder1.getName().isPresent());
        assertEquals("cn1line2", feeder1.getName().get());
        assertTrue(feeder1.getOrder().isPresent());
        assertEquals(2, feeder1.getOrder().get());
        assertEquals(ConnectablePosition.Direction.TOP, feeder1.getDirection());
    }
}
