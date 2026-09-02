/*
 * Copyright (c) 2024-2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.modifications.saling;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.impl.NetworkFactoryImpl;
import lombok.Getter;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.FilterLoader;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.LoadScalingInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ScalingVariationInfos;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.modifications.scaling.LoadScaling;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.gridsuite.modification.utils.TestUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.file.Paths;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Stream;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LoadScalingTest extends AbstractNetworkModificationTest {
    private static final UUID LOAD_SCALING_ID = UUID.randomUUID();
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
    private static final UUID FILTER_ID_ALL_LOADS = UUID.randomUUID();
    private static final UUID FILTER_NO_DK = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_2 = UUID.randomUUID();
    private static final String LOAD_ID_1 = "load1";
    private static final String LOAD_ID_2 = "load2";
    private static final String LOAD_ID_3 = "load3";
    private static final String LOAD_ID_4 = "load4";
    private static final String LOAD_ID_5 = "load5";
    private static final String LOAD_ID_6 = "load6";
    private static final String LOAD_ID_7 = "load7";
    private static final String LOAD_ID_8 = "load8";
    private static final String LOAD_ID_9 = "load9";
    private static final String LOAD_ID_10 = "load10";
    private static final String LOAD_WRONG_ID_1 = "wrongId1";

    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(LOAD_ID_1, LOAD_ID_2),
            FILTER_ID_2, Set.of(LOAD_ID_3, LOAD_ID_4),
            FILTER_ID_3, Set.of(LOAD_ID_5, LOAD_ID_6),
            FILTER_ID_4, Set.of(LOAD_ID_7, LOAD_ID_8),
            FILTER_ID_5, Set.of(LOAD_ID_9, LOAD_ID_10));

    private static final Map<String, Double> DISTRIBUTION_KEYS_MAPPING = Map.of(
            LOAD_ID_1, 1.0, LOAD_ID_2, 2.0,
            LOAD_ID_3, 2.0, LOAD_ID_4, 5.0,
            LOAD_ID_5, 6.0, LOAD_ID_6, 7.0,
            LOAD_ID_7, 3.0, LOAD_ID_8, 8.0,
            LOAD_ID_9, 0.0, LOAD_ID_10, 9.0
    );

    @Getter
    private final FilterLoader filterLoader = TestUtils.createFilterLoader(EquipmentType.LOAD, FILTER_MAPPING, DISTRIBUTION_KEYS_MAPPING);

    @BeforeEach
    void specificSetUp() {
        //createLoads
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        getNetwork().getLoad(LOAD_ID_1).setP0(100).setQ0(10);
        getNetwork().getLoad(LOAD_ID_2).setP0(200).setQ0(20);
        getNetwork().getLoad(LOAD_ID_3).setP0(200).setQ0(20);
        getNetwork().getLoad(LOAD_ID_4).setP0(100).setQ0(1.0);
        getNetwork().getLoad(LOAD_ID_5).setP0(200).setQ0(2.0);
        getNetwork().getLoad(LOAD_ID_6).setP0(120).setQ0(4.0);
        getNetwork().getLoad(LOAD_ID_7).setP0(200).setQ0(1.0);
        getNetwork().getLoad(LOAD_ID_8).setP0(130).setQ0(3.0);
        getNetwork().getLoad(LOAD_ID_9).setP0(200).setQ0(1.0);
        getNetwork().getLoad(LOAD_ID_10).setP0(100).setQ0(1.0);
    }

    @Test
    @Override
    public void testApply() throws Exception {
        LoadScalingInfos modificationInfo = (LoadScalingInfos) buildModification();
        LoadScaling loadScaling = (LoadScaling) modificationInfo.toModification(filterLoader);
        loadScaling.apply(getNetwork());
        assertAfterNetworkModificationApplication();
    }

    @Test
    void testVentilationModeWithoutDistributionKey() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_NO_DK, Set.of(LOAD_ID_2, LOAD_ID_3));
        FilterLoader customFilterLoader = TestUtils.createFilterLoader(EquipmentType.LOAD, filterMappings, Collections.emptyMap());

        FilterInfos filter = FilterInfos.builder()
                .id(FILTER_NO_DK)
                .name("filter")
                .build();

        ScalingVariationInfos variation1 = ScalingVariationInfos.builder()
                .variationValue(100D)
                .variationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .filters(List.of(filter))
                .build();

        ModificationInfos modificationToCreate = LoadScalingInfos.builder()
                .stashed(false)
                .uuid(LOAD_SCALING_ID)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1))
                .build();

        LoadScaling loadScaling = (LoadScaling) modificationToCreate.toModification(customFilterLoader);
        loadScaling.apply(getNetwork());

        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
    }

    @Test
    void testFilterWithWrongIds() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_WRONG_ID_1, Collections.emptySet());
        FilterLoader customFilterLoader = TestUtils.createFilterLoader(EquipmentType.LOAD, filterMappings, Collections.emptyMap());

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_WRONG_ID_1)
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();

        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(customFilterLoader);
        ReportNode report = loadScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        loadScaling.apply(getNetwork(), report);
        assertLogMessage("No equipment will be scaled",
                "network.modification.scaling.noEquipmentToScale", report);
    }

    @Test
    void testScalingCreationWithWarning() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_5, Set.of(LOAD_ID_9, LOAD_ID_10),
                FILTER_WRONG_ID_2, Set.of(LOAD_WRONG_ID_1));
        Map<String, Double> distributionKeysMapping = Map.of(LOAD_ID_9, 0.0, LOAD_ID_10, 9.0, LOAD_WRONG_ID_1, 2.0);
        FilterLoader customFilterLoader = TestUtils.createFilterLoader(EquipmentType.LOAD, filterMappings, distributionKeysMapping);

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_WRONG_ID_2)
                .build();

        FilterInfos filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_ID_5)
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(900D)
                .filters(List.of(filter, filter2))
                .build();

        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(customFilterLoader);
        loadScaling.apply(getNetwork());
        assertEquals(600, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(300, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
    }

    @Test
    void testFilteredDuplicatedEquipmentsRemoved() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_4, Set.of(LOAD_ID_9, LOAD_ID_10),
                FILTER_ID_5, Set.of(LOAD_ID_9));
        FilterLoader customFilterLoader = TestUtils.createFilterLoader(EquipmentType.LOAD, filterMappings, Collections.emptyMap());

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_4)
                .build();

        FilterInfos filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_ID_5)
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(900D)
                .filters(List.of(filter, filter2))
                .build();
        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(customFilterLoader);
        ReportNode report = loadScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        loadScaling.apply(getNetwork(), report);

        assertEquals(600, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(300, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
        assertLogMessage("Equipment load9 already seen in previous filter evaluation, skipping it",
                "network.modification.filterEvaluation.equipmentAlreadySeen", report);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createLoadNetwork(networkUuid, new NetworkFactoryImpl());
    }

    @Override
    protected ModificationInfos buildModification() {
        FilterInfos filter1 = FilterInfos.builder()
            .id(FILTER_ID_1)
            .name("filter1")
            .build();

        FilterInfos filter2 = FilterInfos.builder()
            .id(FILTER_ID_2)
            .name("filter2")
            .build();

        FilterInfos filter3 = FilterInfos.builder()
            .id(FILTER_ID_3)
            .name("filter3")
            .build();

        FilterInfos filter4 = FilterInfos.builder()
            .id(FILTER_ID_4)
            .name("filter4")
            .build();

        FilterInfos filter5 = FilterInfos.builder()
            .id(FILTER_ID_5)
            .name("filter5")
            .build();

        ScalingVariationInfos variation1 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.REGULAR_DISTRIBUTION)
            .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
            .variationValue(50D)
            .filters(List.of(filter2))
            .build();

        ScalingVariationInfos variation2 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.VENTILATION)
            .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
            .variationValue(50D)
            .filters(List.of(filter4))
            .build();

        ScalingVariationInfos variation3 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.PROPORTIONAL)
            .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
            .variationValue(50D)
            .filters(List.of(filter1, filter5))
            .build();

        ScalingVariationInfos variation4 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.PROPORTIONAL)
            .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
            .variationValue(100D)
            .filters(List.of(filter3))
            .build();

        ScalingVariationInfos variation5 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.REGULAR_DISTRIBUTION)
            .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
            .variationValue(50D)
            .filters(List.of(filter3))
            .build();

        return LoadScalingInfos.builder()
            .stashed(false)
            .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
            .variationType(VariationType.DELTA_P)
            .variations(List.of(variation1, variation2, variation3, variation4, variation5))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(108.33, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(216.66, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(225.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
        assertEquals(125.0, getNetwork().getLoad(LOAD_ID_4).getP0(), 0.01D);
        assertEquals(287.5, getNetwork().getLoad(LOAD_ID_5).getP0(), 0.01D);
        assertEquals(182.5, getNetwork().getLoad(LOAD_ID_6).getP0(), 0.01D);
        assertEquals(213.63, getNetwork().getLoad(LOAD_ID_7).getP0(), 0.01D);
        assertEquals(166.36, getNetwork().getLoad(LOAD_ID_8).getP0(), 0.01D);
        assertEquals(216.66, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(108.33, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
    }

    @Test
    void testProportionalAllConnected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.PROPORTIONAL, List.of());
    }

    @Test
    void testProportionalAndVentilationLD1Disconnected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.PROPORTIONAL, List.of("LD1"));
        testVariationWithSomeDisconnections(VariationMode.VENTILATION, List.of("LD1"));
    }

    @Test
    void testProportionalOnlyLD6Connected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.PROPORTIONAL, List.of("LD1", "LD2", "LD3", "LD4", "LD5"));
    }

    private void testVariationWithSomeDisconnections(VariationMode variationMode, List<String> loadsToDisconnect) throws Exception {
        // use a dedicated network where we can easily disconnect loads
        setNetwork(Network.read(Paths.get(Objects.requireNonNull(this.getClass().getClassLoader().getResource("fourSubstations_testsOpenReac.xiidm")).toURI())));

        // disconnect some loads (must not be taken into account by the variation modification)
        loadsToDisconnect.forEach(l -> getNetwork().getLoad(l).getTerminal().disconnect());
        List<String> modifiedLoads = Stream.of("LD1", "LD2", "LD3", "LD4", "LD5", "LD6")
                .filter(l -> !loadsToDisconnect.contains(l))
                .toList();

        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_ALL_LOADS, Set.of("LD1", "LD2", "LD3", "LD4", "LD5", "LD6"));
        Map<String, Double> distributionKeysMapping = Map.of("LD1", 0.0, "LD2", 100.0, "LD3", 100.0,
                "LD4", 100.0, "LD5", 100.0, "LD6", 100.0);
        FilterLoader customFilterLoader = TestUtils.createFilterLoader(EquipmentType.LOAD, filterMappings, distributionKeysMapping);

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_ALL_LOADS)
                .build();
        final double variationValue = 100D;
        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(variationMode)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(variationValue)
                .filters(List.of(filter))
                .build();
        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
                .stashed(false)
                .uuid(LOAD_SCALING_ID)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(customFilterLoader);
        loadScaling.apply(getNetwork());

        // If we sum the P0 for all expected modified loads, we should have the requested variation value
        double connectedLoadsConstantP = modifiedLoads
                .stream()
                .map(g -> getNetwork().getLoad(g).getP0())
                .reduce(0D, Double::sum);
        assertEquals(variationValue, connectedLoadsConstantP, 0.001D);
    }

    @Override
    protected void checkModification() {
    }
}
