/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.LoadScalingInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ScalingVariationInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Stream;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

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

    @Mock
    private IFilterService filterService;

    @BeforeEach
    public void specificSetUp() {
        MockitoAnnotations.openMocks(this);
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

    private static Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(new IdentifiableAttributes(LOAD_ID_1, IdentifiableType.LOAD, 1.0),
            new IdentifiableAttributes(LOAD_ID_2, IdentifiableType.LOAD, 2.0))).build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(new IdentifiableAttributes(LOAD_ID_3, IdentifiableType.LOAD, 2.0),
            new IdentifiableAttributes(LOAD_ID_4, IdentifiableType.LOAD, 5.0))).build();

        FilterEquipments filter3 = FilterEquipments.builder().filterId(FILTER_ID_3).identifiableAttributes(List.of(new IdentifiableAttributes(LOAD_ID_5, IdentifiableType.LOAD, 6.0),
            new IdentifiableAttributes(LOAD_ID_6, IdentifiableType.LOAD, 7.0))).build();

        FilterEquipments filter4 = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(new IdentifiableAttributes(LOAD_ID_7, IdentifiableType.LOAD, 3.0),
            new IdentifiableAttributes(LOAD_ID_8, IdentifiableType.LOAD, 8.0))).build();

        FilterEquipments filter5 = FilterEquipments.builder().filterId(FILTER_ID_5).identifiableAttributes(List.of(new IdentifiableAttributes(LOAD_ID_9, IdentifiableType.LOAD, 0.0),
            new IdentifiableAttributes(LOAD_ID_10, IdentifiableType.LOAD, 9.0))).build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2, FILTER_ID_3, filter3, FILTER_ID_4, filter4, FILTER_ID_5, filter5);
    }

    @Test
    @Override
    public void testApply() throws Exception {
        LoadScalingInfos modificationInfo = (LoadScalingInfos) buildModification();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(getTestFilters());
        LoadScaling loadScaling = (LoadScaling) modificationInfo.toModification();
        loadScaling.initApplicationContext(filterService);
        loadScaling.apply(getNetwork());
        assertAfterNetworkModificationApplication();
    }

    @Test
    void testVentilationModeWithoutDistributionKey() throws Exception {
        List<IdentifiableAttributes> identifiableAttributes = List.of(new IdentifiableAttributes(LOAD_ID_2, IdentifiableType.LOAD, null),
            new IdentifiableAttributes(LOAD_ID_3, IdentifiableType.LOAD, null));
        Map<UUID, FilterEquipments> filters = Map.of(FILTER_NO_DK, FilterEquipments.builder().filterId(FILTER_NO_DK).identifiableAttributes(identifiableAttributes).build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);
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

        LoadScaling loadScaling = (LoadScaling) modificationToCreate.toModification();
        loadScaling.initApplicationContext(filterService);
        loadScaling.apply(getNetwork());

        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
    }

    @Test
    void testFilterWithWrongIds() throws Exception {
        Map<UUID, FilterEquipments> filters = Map.of(FILTER_WRONG_ID_1, FilterEquipments.builder().filterId(FILTER_WRONG_ID_1).identifiableAttributes(List.of()).build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);

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
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification();
        loadScaling.initApplicationContext(filterService);
        ReportNode report = loadScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles("i18n.reports")
                .withMessageTemplate("test").build());
        loadScaling.apply(getNetwork(), report);
        assertLogMessage(loadScalingInfo.getErrorType().name() + ": There is no valid equipment ID among the provided filter(s)",
                "invalidFilters", report);
    }

    @Test
    void testScalingCreationWithWarning() throws Exception {
        Map<UUID, FilterEquipments> filters = Map.of(FILTER_ID_5, FilterEquipments.builder()
                .filterId(FILTER_ID_5)
                .identifiableAttributes(List.of(new IdentifiableAttributes(LOAD_ID_9, IdentifiableType.LOAD, 0.0),
                        new IdentifiableAttributes(LOAD_ID_10, IdentifiableType.LOAD, 9.0)))
                .build(),
                FILTER_WRONG_ID_2, FilterEquipments.builder()
                        .filterId(FILTER_WRONG_ID_2)
                        .identifiableAttributes(
                                List.of(new IdentifiableAttributes(LOAD_WRONG_ID_1, IdentifiableType.LOAD, 2.0)))
                        .build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);

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

        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification();
        loadScaling.initApplicationContext(filterService);
        loadScaling.apply(getNetwork());
        assertEquals(600, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(300, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
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

        List<IdentifiableAttributes> identifiableAttributes = List.of(
            new IdentifiableAttributes("LD1", IdentifiableType.LOAD, 0.0),
            new IdentifiableAttributes("LD2", IdentifiableType.LOAD, 100.0),
            new IdentifiableAttributes("LD3", IdentifiableType.LOAD, 100.0),
            new IdentifiableAttributes("LD4", IdentifiableType.LOAD, 100.0),
            new IdentifiableAttributes("LD5", IdentifiableType.LOAD, 100.0),
            new IdentifiableAttributes("LD6", IdentifiableType.LOAD, 100.0));
        Map<UUID, FilterEquipments> filters = Map.of(FILTER_ID_ALL_LOADS, FilterEquipments.builder().filterId(FILTER_ID_ALL_LOADS).identifiableAttributes(identifiableAttributes).build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);

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

        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification();
        loadScaling.initApplicationContext(filterService);
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
