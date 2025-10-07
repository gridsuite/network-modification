package org.gridsuite.modification.utils;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BusbarSectionPositionAdder;
import org.gridsuite.modification.modifications.BusbarSectionFinderTraverser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.test.context.SpringBootTest;

import static org.gridsuite.modification.utils.NetworkUtil.createSwitch;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SpringBootTest
@SpringBootConfiguration
class BusbarSectionFinderTraverserTest {

    private Network network;

    @BeforeEach
    void setUp() {
        network = NetworkFactory.findDefault().createNetwork("sim1", "test");
        Substation s1 = network.newSubstation()
                .setId("S1")
                .setCountry(Country.FR)
                .setTso("RTE")
                .setGeographicalTags("A")
                .add();
        VoltageLevel vl1 = s1.newVoltageLevel()
                .setId("VL1")
                .setNominalV(24.0)
                .setTopologyKind(TopologyKind.NODE_BREAKER)
                .add();
        vl1.getNodeBreakerView().newBusbarSection()
                .setId("BBS1")
                .setName("BBS1")
                .setNode(1)
                .add();

        VoltageLevel vl2 = s1.newVoltageLevel()
                .setId("VLGEN2")
                .setNominalV(24.0)
                .setTopologyKind(TopologyKind.NODE_BREAKER)
                .add();
        vl2.getNodeBreakerView().newBusbarSection()
                .setId("BBS1_1")
                .setName("BBS1_1")
                .setNode(1)
                .add();
        vl2.getNodeBreakerView().newBusbarSection()
                .setId("BBS2_1")
                .setName("BBS2_1")
                .setNode(2)
                .add();
        vl2.getNodeBreakerView().newBusbarSection()
                .setId("BBS1_2")
                .setName("BBS1_2")
                .setNode(3)
                .add();
        vl2.getNodeBreakerView().newBusbarSection()
                .setId("BBS2_2")
                .setName("BBS2_2")
                .setNode(4)
                .add();

        vl2.getNodeBreakerView()
                .getBusbarSection("BBS1_1")
                .newExtension(BusbarSectionPositionAdder.class)
                .withBusbarIndex(1)
                .withSectionIndex(1)
                .add();
        vl2.getNodeBreakerView()
                .getBusbarSection("BBS1_2")
                .newExtension(BusbarSectionPositionAdder.class)
                .withBusbarIndex(1)
                .withSectionIndex(2)
                .add();
        vl2.getNodeBreakerView()
                .getBusbarSection("BBS2_1")
                .newExtension(BusbarSectionPositionAdder.class)
                .withBusbarIndex(2)
                .withSectionIndex(1)
                .add();
        vl2.getNodeBreakerView()
                .getBusbarSection("BBS2_2")
                .newExtension(BusbarSectionPositionAdder.class)
                .withBusbarIndex(2)
                .withSectionIndex(2)
                .add();

        //Fork topology
        createSwitch(vl2, "DISC_BBS1.1_BBS1.2", "DISC_BBS1.1_BBS1.2", SwitchKind.DISCONNECTOR, false, false, false, 1, 3);
        createSwitch(vl2, "DISC_BBS2.1_BBS2.2", "DISC_BBS2.1_BBS2.2", SwitchKind.DISCONNECTOR, false, true, false, 2, 4);
        createSwitch(vl2, "DISC_BBS1_2", "DISC_BBS1_2", SwitchKind.DISCONNECTOR, false, true, false, 3, 5);
        createSwitch(vl2, "DISC_BBS2_2", "DISC_BBS2_2", SwitchKind.DISCONNECTOR, false, true, false, 4, 5);
        createSwitch(vl2, "BRK_FORK", "BRK_FORK", SwitchKind.BREAKER, false, true, false, 5, 6);
        createSwitch(vl2, "DISC_LINE_1_2", "DISC_LINE_1_2", SwitchKind.DISCONNECTOR, false, false, false, 6, 7);
        createSwitch(vl2, "DISC_LINE_2_2", "DISC_LINE_2_2", SwitchKind.DISCONNECTOR, false, false, false, 6, 8);
        createSwitch(vl2, "BRK_LINE_1_2", "BRK_LINE_1_2", SwitchKind.BREAKER, false, false, false, 7, 10);
        createSwitch(vl2, "BRK_LINE_2_2", "BRK_LINE_2_2", SwitchKind.BREAKER, false, false, false, 8, 9);
        network.newLine()
                .setId("LINE_1_2")
                .setName("LINE_1_2")
                .setVoltageLevel1("VLGEN2")
                .setNode1(10)
                .setVoltageLevel2("VL1")
                .setNode2(11)
                .setR(2.0)
                .setX(25.0)
                .setG1(0.0)
                .setB1(300E-6 / 2)
                .setG2(0.0)
                .setB2(300E-6 / 2)
                .add();

        network.newLine()
                .setId("LINE_2_2")
                .setName("LINE_2_2")
                .setVoltageLevel1("VLGEN2")
                .setNode1(9)
                .setVoltageLevel2("VL1")
                .setNode2(21)
                .setR(2.0)
                .setX(25.0)
                .setG1(0.0)
                .setB1(300E-6 / 2)
                .setG2(0.0)
                .setB2(300E-6 / 2)
                .add();

        // BYPASS topology
        createSwitch(vl2, "DISC_BBS1_1", "DISC_BBS1_1", SwitchKind.DISCONNECTOR, false, true, false, 1, 15);
        createSwitch(vl2, "DISC_BBS2_1", "DISC_BBS2_1", SwitchKind.DISCONNECTOR, false, true, false, 2, 18);
        createSwitch(vl2, "DISC_BYPASS", "DISC_BYPASS", SwitchKind.DISCONNECTOR, false, true, false, 15, 18);
        createSwitch(vl2, "DISC_LINE_1_1", "DISC_LINE_1_1", SwitchKind.DISCONNECTOR, false, true, false, 15, 11);
        createSwitch(vl2, "DISC_LINE_2_1", "DISC_LINE_2_1", SwitchKind.DISCONNECTOR, false, true, false, 18, 21);
        createSwitch(vl2, "BRK_LINE_1_1", "BRK_LINE_1_1", SwitchKind.BREAKER, false, true, false, 11, 12);
        createSwitch(vl2, "BRK_LINE_2_1", "BRK_LINE_2_1", SwitchKind.BREAKER, false, true, false, 21, 23);
        network.newLine()
                .setId("LINE_1_1")
                .setName("LINE_1_1")
                .setVoltageLevel1("VLGEN2")
                .setNode1(12)
                .setVoltageLevel2("VL1")
                .setNode2(3)
                .setR(2.0)
                .setX(25.0)
                .setG1(0.0)
                .setB1(300E-6 / 2)
                .setG2(0.0)
                .setB2(300E-6 / 2)
                .add();

        network.newLine()
                .setId("LINE_2_1")
                .setName("LINE_2_1")
                .setVoltageLevel1("VLGEN2")
                .setNode1(23)
                .setVoltageLevel2("VL1")
                .setNode2(41)
                .setR(2.0)
                .setX(25.0)
                .setG1(0.0)
                .setB1(300E-6 / 2)
                .setG2(0.0)
                .setB2(300E-6 / 2)
                .add();

        network.getVariantManager().setWorkingVariant(VariantManagerConstants.INITIAL_VARIANT_ID);
    }

    @Test
    void testWithOpenSwitch() {
        Line line12 = network.getLine("LINE_1_2");
        Line line22 = network.getLine("LINE_2_2");
        BusbarSectionFinderTraverser.BusbarSectionResult result12 = BusbarSectionFinderTraverser.getBusbarSectionResult(line12.getTerminal1());
        BusbarSectionFinderTraverser.BusbarSectionResult result22 = BusbarSectionFinderTraverser.getBusbarSectionResult(line22.getTerminal1());
        assertNotNull(result12);
        assertEquals("BBS1_2", result12.busbarSectionId());
        assertNotNull(result22);
        assertEquals("BBS1_2", result22.busbarSectionId());
        assertEquals(4, result12.depth());
        assertEquals(4, result22.depth());
        assertEquals("DISC_BBS1_2", result12.lastSwitch().id());
        assertEquals("DISC_BBS1_2", result22.lastSwitch().id());
        assertTrue(result12.lastSwitch().isOpen());
        assertTrue(result22.lastSwitch().isOpen());
        assertFalse(result12.allClosedSwitch());
        assertFalse(result22.allClosedSwitch());

    }

    @Test
    void testWithClosedLastSwitch() {
        network.getSwitch("DISC_BBS1_1").setOpen(false);
        network.getSwitch("DISC_BBS2_2").setOpen(false);
        Line line11 = network.getLine("LINE_1_1");
        Line line21 = network.getLine("LINE_2_1");
        Line line12 = network.getLine("LINE_1_2");
        BusbarSectionFinderTraverser.BusbarSectionResult result11 = BusbarSectionFinderTraverser.getBusbarSectionResult(line11.getTerminal1());
        BusbarSectionFinderTraverser.BusbarSectionResult result21 = BusbarSectionFinderTraverser.getBusbarSectionResult(line21.getTerminal1());
        BusbarSectionFinderTraverser.BusbarSectionResult result12 = BusbarSectionFinderTraverser.getBusbarSectionResult(line12.getTerminal1());
        assertNotNull(result11);
        assertEquals("BBS1_1", result11.busbarSectionId());
        assertEquals(3, result11.depth());
        assertEquals("DISC_BBS1_1", result11.lastSwitch().id());
        assertFalse(result11.lastSwitch().isOpen());
        assertFalse(result11.allClosedSwitch());
        assertNotNull(result21);
        assertEquals("BBS1_1", result21.busbarSectionId());
        assertEquals(4, result21.depth());
        assertEquals("DISC_BBS1_1", result21.lastSwitch().id());
        assertFalse(result21.lastSwitch().isOpen());
        assertFalse(result21.allClosedSwitch());
        assertNotNull(result12);
        assertEquals("BBS2_2", result12.busbarSectionId());
        assertEquals(4, result12.depth());
        assertEquals("DISC_BBS2_2", result12.lastSwitch().id());
        assertFalse(result12.lastSwitch().isOpen());
        assertFalse(result12.allClosedSwitch());
    }

    @Test
    void testWithAllClosedSwitch() {
        network.getSwitch("BRK_LINE_1_1").setOpen(false);
        network.getSwitch("DISC_LINE_1_1").setOpen(false);
        network.getSwitch("DISC_BBS1_1").setOpen(false);
        network.getSwitch("DISC_BBS2_1").setOpen(false);
        network.getSwitch("BRK_LINE_2_1").setOpen(false);
        network.getSwitch("DISC_LINE_2_1").setOpen(false);
        Line line11 = network.getLine("LINE_1_1");
        Line line21 = network.getLine("LINE_2_1");
        BusbarSectionFinderTraverser.BusbarSectionResult result11 = BusbarSectionFinderTraverser.getBusbarSectionResult(line11.getTerminal1());
        BusbarSectionFinderTraverser.BusbarSectionResult result21 = BusbarSectionFinderTraverser.getBusbarSectionResult(line21.getTerminal1());
        assertNotNull(result11);
        assertEquals("BBS1_1", result11.busbarSectionId());
        assertEquals(3, result11.depth());
        assertTrue(result11.allClosedSwitch());
        assertEquals("DISC_BBS1_1", result11.lastSwitch().id());
        assertNotNull(result21);
        assertEquals("BBS2_1", result21.busbarSectionId());
        assertEquals(3, result21.depth());
        assertEquals("DISC_BBS2_1", result21.lastSwitch().id());
        assertTrue(result21.allClosedSwitch());
    }
}

