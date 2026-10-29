/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.context.FilterLoader;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.NetworkReductionInfos;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
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
