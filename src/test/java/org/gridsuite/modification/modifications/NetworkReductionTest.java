/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.context.FilterLoader;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.NetworkReductionInfos;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.gridsuite.modification.utils.TestUtils.createFilterLoader;
import static org.junit.jupiter.api.Assertions.*;

class NetworkReductionTest extends AbstractNetworkModificationTest {
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return Network.read("fourSubstationsNb_country 2_N1.xiidm", getClass().getResourceAsStream("/fourSubstationsNb_country 2_N1.xiidm"));
    }

    @Override
    public FilterLoader getFilterLoader() {
        return createFilterLoader(EquipmentType.VOLTAGE_LEVEL, Map.of(
                FILTER_ID_1, Set.of("S3VL1"),
                FILTER_ID_2, Set.of("S3VL1", "S4VL1")));
    }

    @Override
    protected NetworkReductionInfos buildModification() {
        return NetworkReductionInfos.builder()
                .filters(List.of(new FilterInfos(FILTER_ID_1, "S3"), new FilterInfos(FILTER_ID_2, "S3 and S4")))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertThat(getNetwork().getVoltageLevelStream().map(VoltageLevel::getId))
                .containsExactlyInAnyOrder("S1VL1", "S1VL2", "S2VL1");
        assertThat(getNetwork().getSubstationStream().map(Substation::getId))
                .containsExactlyInAnyOrder("S1", "S2");
        assertNull(getNetwork().getGenerator("GTH2"));
        assertNull(getNetwork().getLoad("LD6"));
        assertNotNull(getNetwork().getGenerator("GH1"));
        assertNotNull(getNetwork().getLoad("LD1"));
        assertNotNull(getNetwork().getTwoWindingsTransformer("TWT"));
        assertNotNull(getNetwork().getLine("LINE_S2S3(1)"));
        assertEquals(1, getNetwork().getLineCount());
        assertNull(getNetwork().getLine("LINE_S2S3"));
        assertNull(getNetwork().getIdentifiable("LINE_S3S4"));
        assertEquals(0, getNetwork().getBoundaryLineCount());
        Load replacementLoad = getNetwork().getLoad("LINE_S2S3");
        assertNotNull(replacementLoad);
        assertEquals(LoadType.FICTITIOUS, replacementLoad.getLoadType());
        assertEquals("S2VL1", replacementLoad.getTerminal().getVoltageLevel().getId());
    }

    @Override
    protected void checkModification() {
        assertDoesNotThrow(() -> buildModification().toModification(context(getFilterLoader())).check(getNetwork()));
    }

    @Override
    @Test
    public void testApply() {
        double boundaryP = getNetwork().getLine("LINE_S2S3").getTerminal1().getP();
        double boundaryQ = getNetwork().getLine("LINE_S2S3").getTerminal1().getQ();

        apply(buildModification(), getFilterLoader());

        assertAfterNetworkModificationApplication();
        assertEquals(boundaryP, getNetwork().getLoad("LINE_S2S3").getP0(), 1e-6);
        assertEquals(boundaryQ, getNetwork().getLoad("LINE_S2S3").getQ0(), 1e-6);
    }

    @Test
    void testApplyWithOneVoltageLevelExcludedFromSubstation() {
        double transformerP = getNetwork().getTwoWindingsTransformer("TWT").getTerminal2().getP();
        double transformerQ = getNetwork().getTwoWindingsTransformer("TWT").getTerminal2().getQ();
        NetworkReductionInfos infos = NetworkReductionInfos.builder()
                .filters(List.of(new FilterInfos(FILTER_ID_1, "S1VL1")))
                .build();

        apply(infos, createFilterLoader(EquipmentType.VOLTAGE_LEVEL, Map.of(FILTER_ID_1, Set.of("S1VL1"))));

        assertThat(getNetwork().getVoltageLevelStream().map(VoltageLevel::getId))
                .containsExactlyInAnyOrder("S1VL2", "S2VL1", "S3VL1", "S4VL1");
        assertEquals(4, getNetwork().getSubstationCount());
        assertNull(getNetwork().getTwoWindingsTransformer("TWT"));
        Load replacementLoad = getNetwork().getLoad("TWT");
        assertNotNull(replacementLoad);
        assertEquals("S1VL2", replacementLoad.getTerminal().getVoltageLevel().getId());
        assertEquals(LoadType.FICTITIOUS, replacementLoad.getLoadType());
        assertEquals(transformerP, replacementLoad.getP0(), 1e-6);
        assertEquals(transformerQ, replacementLoad.getQ0(), 1e-6);
        assertNull(getNetwork().getLoad("LD1"));
        assertNotNull(getNetwork().getGenerator("GH1"));
        assertNotNull(getNetwork().getGenerator("GTH2"));
        assertEquals(3, getNetwork().getLineCount());
        assertEquals(0, getNetwork().getBoundaryLineCount());
    }

    @Test
    void testReductionReports() {
        NetworkReductionInfos infos = NetworkReductionInfos.builder()
                .filters(List.of(new FilterInfos(FILTER_ID_1, "S1VL1 and S3VL1")))
                .build();
        FilterLoader filterLoader = createFilterLoader(EquipmentType.VOLTAGE_LEVEL,
                Map.of(FILTER_ID_1, Set.of("S1VL1", "S3VL1")));
        ReportNode report = infos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());

        infos.toModification(context(filterLoader)).apply(getNetwork(), report);

        ReportNode reductionReport = report.getChildren().stream()
                .filter(node -> node.getMessageKey().equals("network.modification.networkReduction.applied"))
                .findFirst().orElseThrow();
        assertLogMessageWithoutRank("Voltage level S1VL1 removed",
                "network.modification.networkReduction.voltageLevelRemoved", reductionReport);
        assertLogMessageWithoutRank("Line LINE_S2S3 replaced by LOAD LINE_S2S3",
                "network.modification.networkReduction.lineReplaced", reductionReport);
        assertLogMessageWithoutRank("Line LINE_S2S3 removed",
                "network.modification.networkReduction.lineRemoved", reductionReport);
        assertLogMessageWithoutRank("Substation S3 removed",
                "network.modification.networkReduction.substationRemoved", reductionReport);
        assertLogMessageWithoutRank("Two-winding transformer TWT replaced by LOAD TWT",
                "network.modification.networkReduction.twoWindingsTransformerReplaced", reductionReport);
        assertLogMessageWithoutRank("Two-winding transformer TWT removed",
                "network.modification.networkReduction.twoWindingsTransformerRemoved", reductionReport);
        assertLogMessageWithoutRank("HVDC line HVDC2 replaced by LOAD HVDC2",
                "network.modification.networkReduction.hvdcLineReplaced", reductionReport);
        assertLogMessageWithoutRank("HVDC line HVDC2 removed",
                "network.modification.networkReduction.hvdcLineRemoved", reductionReport);
        assertThat(reductionReport.getChildren()).extracting(ReportNode::getMessageKey)
                .contains("network.modification.networkReduction.voltageLevelRemoved",
                        "network.modification.networkReduction.lineReplaced");
    }

    @Test
    void testReportsWithAllVoltageLevelsExcluded() {
        ReportNode report = applyWithReport(getNetwork(), Set.of("S1VL1", "S1VL2", "S2VL1", "S3VL1", "S4VL1"));

        assertEmptyNetwork();
        assertLogMessageWithoutRank("Line LINE_S3S4 removed",
                "network.modification.networkReduction.lineRemoved", report);
        assertLogMessageWithoutRank("Two-winding transformer TWT removed",
                "network.modification.networkReduction.twoWindingsTransformerRemoved", report);
        assertLogMessageWithoutRank("HVDC line HVDC2 removed",
                "network.modification.networkReduction.hvdcLineRemoved", report);
        assertThat(report.getChildren()).extracting(ReportNode::getMessageKey)
                .doesNotContain("network.modification.networkReduction.lineReplaced",
                        "network.modification.networkReduction.twoWindingsTransformerReplaced",
                        "network.modification.networkReduction.hvdcLineReplaced");
    }

    @Test
    void testThreeWindingsTransformerReplacementAndTieLineReports() {
        Network network = createThreeWindingsTransformerNetwork();
        network.getThreeWindingsTransformer("3WT").getLeg1().getTerminal().setP(10).setQ(5);
        network.getVoltageLevel("VL_132").newBoundaryLine()
                .setId("BL1").setBus("BUS_132").setR(1).setX(1).setG(0).setB(0).setP0(0).setQ0(0).add();
        network.getVoltageLevel("VL_33").newBoundaryLine()
                .setId("BL2").setBus("BUS_33").setR(1).setX(1).setG(0).setB(0).setP0(0).setQ0(0).add();
        network.newTieLine().setId("TL").setBoundaryLine1("BL1").setBoundaryLine2("BL2").add();

        ReportNode report = applyWithReport(network, Set.of("VL_33", "VL_11"));

        assertNotNull(network.getLoad("3WT"));
        assertNull(network.getTieLine("TL"));
        assertLogMessageWithoutRank("Three-winding transformer 3WT replaced by LOAD 3WT",
                "network.modification.networkReduction.threeWindingsTransformerReplaced", report);
        assertLogMessageWithoutRank("Three-winding transformer 3WT removed",
                "network.modification.networkReduction.threeWindingsTransformerRemoved", report);
        assertLogMessageWithoutRank("Tie line TL removed",
                "network.modification.networkReduction.tieLineRemoved", report);
    }

    @Test
    void testThreeWindingsTransformerRemovalReports() {
        Network network = createThreeWindingsTransformerNetwork();

        ReportNode report = applyWithReport(network, Set.of("VL_132", "VL_33", "VL_11"));

        assertEquals(0, network.getThreeWindingsTransformerCount());
        assertLogMessageWithoutRank("Three-winding transformer 3WT removed",
                "network.modification.networkReduction.threeWindingsTransformerRemoved", report);
        assertThat(report.getChildren()).extracting(ReportNode::getMessageKey)
                .doesNotContain("network.modification.networkReduction.threeWindingsTransformerReplaced");
    }

    @Test
    void testHvdcGeneratorReplacementReports() {
        Network network = createVscNetwork();

        ReportNode report = applyWithReport(network, Set.of("VL2"));

        assertNotNull(network.getGenerator("L"));
        assertLogMessageWithoutRank("HVDC line L replaced by GENERATOR L",
                "network.modification.networkReduction.hvdcLineReplaced", report);
    }

    @Test
    void testApplyWithNoFilters() {
        apply(NetworkReductionInfos.builder().filters(List.of()).build(), getFilterLoader());

        assertUnreducedNetwork();
    }

    @Test
    void testApplyWithNoMatchingVoltageLevels() {
        NetworkReductionInfos infos = NetworkReductionInfos.builder()
                .filters(List.of(new FilterInfos(FILTER_ID_1, "Unknown voltage level")))
                .build();

        apply(infos, createFilterLoader(EquipmentType.VOLTAGE_LEVEL, Map.of(FILTER_ID_1, Set.of("UNKNOWN"))));

        assertUnreducedNetwork();
    }

    @Test
    void testFilterWithWrongIds() {
        UUID wrongFilterId = UUID.randomUUID();
        FilterLoader customFilterLoader = createFilterLoader(EquipmentType.VOLTAGE_LEVEL,
                Map.of(wrongFilterId, Set.of()));
        NetworkReductionInfos infos = NetworkReductionInfos.builder()
                .filters(List.of(new FilterInfos(wrongFilterId, "filter")))
                .build();
        NetworkReduction networkReduction = (NetworkReduction) infos.toModification(context(customFilterLoader));
        ReportNode report = infos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());

        networkReduction.apply(getNetwork(), report);

        assertLogMessage("No equipment will be removed",
                "network.modification.byFilterDeletion.noEquipmentToRemove", report);
        assertUnreducedNetwork();
    }

    @Test
    void testApplyWithAllVoltageLevelsExcluded() {
        NetworkReductionInfos infos = NetworkReductionInfos.builder()
                .filters(List.of(new FilterInfos(FILTER_ID_1, "All voltage levels")))
                .build();

        apply(infos, createFilterLoader(EquipmentType.VOLTAGE_LEVEL,
                Map.of(FILTER_ID_1, Set.of("S1VL1", "S1VL2", "S2VL1", "S3VL1", "S4VL1"))));

        assertEmptyNetwork();
    }

    @Test
    void testGetName() {
        assertEquals(ModificationType.NETWORK_REDUCTION.name(),
                buildModification().toModification(context(getFilterLoader())).getName());
    }

    private static ModificationContext context(FilterLoader filterLoader) {
        return ModificationContext.builder().filterLoader(filterLoader).build();
    }

    private void apply(NetworkReductionInfos infos, FilterLoader filterLoader) {
        infos.toModification(context(filterLoader)).apply(getNetwork(), new DefaultNamingStrategy(), ReportNode.NO_OP);
    }

    private ReportNode applyWithReport(Network network, Set<String> excludedVoltageLevelIds) {
        NetworkReductionInfos infos = NetworkReductionInfos.builder()
                .filters(List.of(new FilterInfos(FILTER_ID_1, "Excluded voltage levels")))
                .build();
        FilterLoader filterLoader = createFilterLoader(EquipmentType.VOLTAGE_LEVEL,
                Map.of(FILTER_ID_1, excludedVoltageLevelIds));
        ReportNode report = infos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());

        infos.toModification(context(filterLoader)).apply(network, report);

        return report.getChildren().stream()
                .filter(node -> node.getMessageKey().equals("network.modification.networkReduction.applied"))
                .findFirst().orElseThrow();
    }

    private static Network createThreeWindingsTransformerNetwork() {
        Network network = Network.create("three-windings-transformer", "test");
        Substation substation = network.newSubstation().setId("S").add();
        for (int nominalV : List.of(132, 33, 11)) {
            substation.newVoltageLevel().setId("VL_" + nominalV).setNominalV(nominalV)
                    .setTopologyKind(TopologyKind.BUS_BREAKER).add()
                    .getBusBreakerView().newBus().setId("BUS_" + nominalV).add();
        }
        substation.newThreeWindingsTransformer().setId("3WT").setRatedU0(132)
                .newLeg1().setR(1).setX(1).setG(0).setB(0).setRatedU(132)
                .setVoltageLevel("VL_132").setBus("BUS_132").add()
                .newLeg2().setR(1).setX(1).setG(0).setB(0).setRatedU(33)
                .setVoltageLevel("VL_33").setBus("BUS_33").add()
                .newLeg3().setR(1).setX(1).setG(0).setB(0).setRatedU(11)
                .setVoltageLevel("VL_11").setBus("BUS_11").add()
                .add();
        return network;
    }

    private static Network createVscNetwork() {
        Network network = Network.create("vsc", "test");
        for (int side : List.of(1, 2)) {
            VoltageLevel voltageLevel = network.newSubstation().setId("S" + side).add()
                    .newVoltageLevel().setId("VL" + side).setNominalV(400)
                    .setTopologyKind(TopologyKind.BUS_BREAKER).add();
            voltageLevel.getBusBreakerView().newBus().setId("B" + side).add();
            voltageLevel.newVscConverterStation().setId("C" + side).setBus("B" + side)
                    .setLossFactor(1).setVoltageSetpoint(400).setVoltageRegulatorOn(true).add()
                    .getTerminal().setP(100).setQ(50);
        }
        network.newHvdcLine().setId("L").setConverterStationId1("C1").setConverterStationId2("C2")
                .setR(1).setNominalV(400).setMaxP(300).setActivePowerSetpoint(100)
                .setConvertersMode(HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER).add();
        return network;
    }

    private void assertEmptyNetwork() {
        assertEquals(0, getNetwork().getSubstationCount());
        assertEquals(0, getNetwork().getVoltageLevelCount());
        assertEquals(0, getNetwork().getLineCount());
        assertEquals(0, getNetwork().getBoundaryLineCount());
        assertEquals(0, getNetwork().getGeneratorCount());
        assertEquals(0, getNetwork().getLoadCount());
    }

    private void assertUnreducedNetwork() {
        assertThat(getNetwork().getVoltageLevelStream().map(VoltageLevel::getId))
                .containsExactlyInAnyOrder("S1VL1", "S1VL2", "S2VL1", "S3VL1", "S4VL1");
        assertEquals(4, getNetwork().getSubstationCount());
        assertEquals(3, getNetwork().getLineCount());
        assertEquals(0, getNetwork().getBoundaryLineCount());
        assertEquals(5, getNetwork().getGeneratorCount());
        assertEquals(6, getNetwork().getLoadCount());
        assertNotNull(getNetwork().getTwoWindingsTransformer("TWT"));
        assertNotNull(getNetwork().getHvdcLine("HVDC2"));
    }
}
