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
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.GeneratorScalingInfos;
import org.gridsuite.modification.dto.IdentifiableAttributes;
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
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class GeneratorScalingTest extends AbstractNetworkModificationTest {
    private static final UUID GENERATOR_SCALING_ID = UUID.randomUUID();
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
    private static final UUID FILTER_ID_ALL_GEN = UUID.randomUUID();
    private static final UUID FILTER_NO_DK = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_2 = UUID.randomUUID();
    private static final String GENERATOR_ID_1 = "gen1";
    private static final String GENERATOR_ID_2 = "gen2";
    private static final String GENERATOR_ID_3 = "gen3";
    private static final String GENERATOR_ID_4 = "gen4";
    private static final String GENERATOR_ID_5 = "gen5";
    private static final String GENERATOR_ID_6 = "gen6";
    private static final String GENERATOR_ID_7 = "gen7";
    private static final String GENERATOR_ID_8 = "gen8";
    private static final String GENERATOR_ID_9 = "gen9";
    private static final String GENERATOR_ID_10 = "gen10";
    private static final String GENERATOR_WRONG_ID_1 = "wrongId1";

    @Mock
    private IFilterService filterService;

    @BeforeEach
    void specificSetUp() {
        MockitoAnnotations.openMocks(this);
        //createGenerators
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        getNetwork().getGenerator(GENERATOR_ID_1).setTargetP(100).setMaxP(500);
        getNetwork().getGenerator(GENERATOR_ID_2).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_3).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_4).setTargetP(100).setMaxP(500);
        getNetwork().getGenerator(GENERATOR_ID_5).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_6).setTargetP(100).setMaxP(500);
        getNetwork().getGenerator(GENERATOR_ID_7).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_8).setTargetP(100).setMaxP(500);
        getNetwork().getGenerator(GENERATOR_ID_9).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_10).setTargetP(100).setMaxP(500);
    }

    private static Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(new IdentifiableAttributes(GENERATOR_ID_1, IdentifiableType.LOAD, 1.0),
            new IdentifiableAttributes(GENERATOR_ID_2, IdentifiableType.LOAD, 2.0))).build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(new IdentifiableAttributes(GENERATOR_ID_3, IdentifiableType.LOAD, 2.0),
            new IdentifiableAttributes(GENERATOR_ID_4, IdentifiableType.LOAD, 5.0))).build();

        FilterEquipments filter3 = FilterEquipments.builder().filterId(FILTER_ID_3).identifiableAttributes(List.of(new IdentifiableAttributes(GENERATOR_ID_5, IdentifiableType.LOAD, 6.0),
            new IdentifiableAttributes(GENERATOR_ID_6, IdentifiableType.LOAD, 7.0))).build();

        FilterEquipments filter4 = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(new IdentifiableAttributes(GENERATOR_ID_7, IdentifiableType.LOAD, 3.0),
            new IdentifiableAttributes(GENERATOR_ID_8, IdentifiableType.LOAD, 8.0))).build();

        FilterEquipments filter5 = FilterEquipments.builder().filterId(FILTER_ID_5).identifiableAttributes(List.of(new IdentifiableAttributes(GENERATOR_ID_9, IdentifiableType.LOAD, 0.0),
            new IdentifiableAttributes(GENERATOR_ID_10, IdentifiableType.LOAD, 9.0))).build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2, FILTER_ID_3, filter3, FILTER_ID_4, filter4, FILTER_ID_5, filter5);
    }

    @Test
    @Override
    public void testApply() throws Exception {
        GeneratorScalingInfos modificationInfo = (GeneratorScalingInfos) buildModification();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(getTestFilters());
        GeneratorScaling loadScaling = (GeneratorScaling) modificationInfo.toModification();
        loadScaling.initApplicationContext(filterService);
        loadScaling.apply(getNetwork());
        assertAfterNetworkModificationApplication();
    }

    @Test
    void testVentilationModeWithoutDistributionKey() throws Exception {
        List<IdentifiableAttributes> identifiableAttributes = List.of(new IdentifiableAttributes(GENERATOR_ID_2, IdentifiableType.GENERATOR, null),
            new IdentifiableAttributes(GENERATOR_ID_3, IdentifiableType.GENERATOR, null));
        Map<UUID, FilterEquipments> filters = Map.of(FILTER_NO_DK, FilterEquipments.builder().filterId(FILTER_NO_DK).identifiableAttributes(identifiableAttributes).build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);

        var filter = FilterInfos.builder()
                .id(FILTER_NO_DK)
                .name("filter")
                .build();

        var variation1 = ScalingVariationInfos.builder()
                .variationValue(100D)
                .variationMode(VariationMode.VENTILATION)
                .filters(List.of(filter))
                .build();

        ModificationInfos modificationToCreate = GeneratorScalingInfos.builder()
                .stashed(false)
                .uuid(GENERATOR_SCALING_ID)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1))
                .build();

        GeneratorScaling generatorScaling = (GeneratorScaling) modificationToCreate.toModification();
        generatorScaling.initApplicationContext(filterService);
        generatorScaling.apply(getNetwork());

        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0.01D);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 0.01D);
    }

    @Test
    void testFilterWithWrongIds() throws Exception {
        Map<UUID, FilterEquipments> filters = Map.of(FILTER_WRONG_ID_1, FilterEquipments.builder().filterId(FILTER_WRONG_ID_1).identifiableAttributes(List.of()).build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);

        var filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_WRONG_ID_1)
                .build();
        var variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();
        var generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification();
        generatorScaling.initApplicationContext(filterService);
        ReportNode report = generatorScalingInfo.createSubReportNode(ReportNode.newRootReportNode().withMessageTemplate("", "").build());
        generatorScaling.apply(getNetwork(), report);
        assertLogMessage(generatorScalingInfo.getErrorType().name() + ": There is no valid equipment ID among the provided filter(s)",
                "invalidFilters", report);
    }

    @Test
    void testScalingCreationWithWarning() throws Exception {
        Map<UUID, FilterEquipments> filters = Map.of(FILTER_ID_5, FilterEquipments.builder()
                .filterId(FILTER_ID_5)
                .identifiableAttributes(List.of(new IdentifiableAttributes(GENERATOR_ID_9, IdentifiableType.GENERATOR, 0.0),
                        new IdentifiableAttributes(GENERATOR_ID_10, IdentifiableType.LOAD, 9.0)))
                .build(),
                FILTER_WRONG_ID_2, FilterEquipments.builder()
                        .filterId(FILTER_WRONG_ID_2)
                        .identifiableAttributes(
                                List.of(new IdentifiableAttributes(GENERATOR_WRONG_ID_1, IdentifiableType.GENERATOR, 2.0)))
                        .build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);

        var filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_WRONG_ID_2)
                .build();

        var filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_ID_5)
                .build();

        var variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(900D)
                .filters(List.of(filter, filter2))
                .build();
        var generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification();
        generatorScaling.initApplicationContext(filterService);
        generatorScaling.apply(getNetwork());

        assertEquals(600, getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 0.01D);
        assertEquals(300, getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 0.01D);
    }

    @Test
    void testScalingCreationWithDupeGenerator() {
        // GENERATOR 10 is in both filter
        // the filter with wrong ids will be taken for valid ids
        // it will be counted only once even if it is twice
        Map<UUID, FilterEquipments> filters = Map.of(FILTER_ID_5, FilterEquipments.builder()
                .filterId(FILTER_ID_5)
                .identifiableAttributes(List.of(new IdentifiableAttributes(GENERATOR_ID_9, IdentifiableType.GENERATOR, 0.0),
                    new IdentifiableAttributes(GENERATOR_ID_10, IdentifiableType.LOAD, 9.0)))
                .build(),
            FILTER_WRONG_ID_2, FilterEquipments.builder()
                .filterId(FILTER_WRONG_ID_2)
                .identifiableAttributes(
                    List.of(new IdentifiableAttributes(GENERATOR_WRONG_ID_1, IdentifiableType.GENERATOR, 2.0),
                        new IdentifiableAttributes(GENERATOR_ID_10, IdentifiableType.LOAD, 9.0)))
                .build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);

        var filter = FilterInfos.builder()
            .name("filter")
            .id(FILTER_WRONG_ID_2)
            .build();

        var filter2 = FilterInfos.builder()
            .name("filter2")
            .id(FILTER_ID_5)
            .build();

        var variation = ScalingVariationInfos.builder()
            .variationMode(VariationMode.PROPORTIONAL)
            .variationValue(900D)
            .filters(List.of(filter, filter2))
            .build();
        var generatorScalingInfo = GeneratorScalingInfos.builder()
            .stashed(false)
            .variationType(VariationType.TARGET_P)
            .variations(List.of(variation))
            .build();

        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification();
        generatorScaling.initApplicationContext(filterService);
        generatorScaling.apply(getNetwork());
        assertEquals(600, getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 0.01D);
        assertEquals(300, getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 0.01D);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createGeneratorsNetwork(networkUuid, new NetworkFactoryImpl());
    }

    @Override
    protected ModificationInfos buildModification() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter2")
                .build();

        var filter3 = FilterInfos.builder()
                .id(FILTER_ID_3)
                .name("filter3")
                .build();

        var filter4 = FilterInfos.builder()
                .id(FILTER_ID_4)
                .name("filter4")
                .build();

        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter5")
                .build();

        var variation1 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL_TO_PMAX)
                .variationValue(50D)
                .filters(List.of(filter1))
                .build();

        var variation2 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .variationValue(50D)
                .filters(List.of(filter2))
                .build();

        var variation3 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.STACKING_UP)
                .variationValue(50D)
                .filters(List.of(filter3))
                .build();

        var variation4 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .variationValue(50D)
                .filters(List.of(filter4))
                .build();

        var variation5 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(50D)
                .filters(List.of(filter1, filter5))
                .build();

        return GeneratorScalingInfos.builder()
                .stashed(false)
                //.date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1, variation2, variation3, variation4, variation5))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(118.46, getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 0.01D);
        assertEquals(258.46, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0.01D);
        assertEquals(225, getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 0.01D);
        assertEquals(125, getNetwork().getGenerator(GENERATOR_ID_4).getTargetP(), 0.01D);
        assertEquals(250, getNetwork().getGenerator(GENERATOR_ID_5).getTargetP(), 0.01D);
        assertEquals(100, getNetwork().getGenerator(GENERATOR_ID_6).getTargetP(), 0.01D);
        assertEquals(213.63, getNetwork().getGenerator(GENERATOR_ID_7).getTargetP(), 0.01D);
        assertEquals(136.36, getNetwork().getGenerator(GENERATOR_ID_8).getTargetP(), 0.01D);
        assertEquals(215.38, getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 0.01D);
        assertEquals(107.69, getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 0.01D);
    }

    @Test
    void testRegularDistributionAllConnected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.REGULAR_DISTRIBUTION, List.of());
    }

    @Test
    void testRegularDistributionOnlyGTH2Connected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.REGULAR_DISTRIBUTION, List.of("GH1", "GH2", "GH3", "GTH1", "GTH3"));
    }

    @Test
    void testAllModesGH1Disconnected() throws Exception {
        for (VariationMode mode : VariationMode.values()) {
            testVariationWithSomeDisconnections(mode, List.of("GH1"));
        }
    }

    private void testVariationWithSomeDisconnections(VariationMode variationMode, List<String> generatorsToDisconnect) throws Exception {
        // use a dedicated network where we can easily disconnect generators
        setNetwork(Network.read(Paths.get(Objects.requireNonNull(this.getClass().getClassLoader().getResource("fourSubstations_testsOpenReac.xiidm")).toURI())));

        // disconnect some generators (must not be taken into account by the variation modification)
        generatorsToDisconnect.forEach(g -> getNetwork().getGenerator(g).getTerminal().disconnect());
        List<String> modifiedGenerators = Stream.of("GH1", "GH2", "GH3", "GTH1", "GTH2", "GTH3")
                .filter(g -> !generatorsToDisconnect.contains(g))
                .toList();

        List<IdentifiableAttributes> identifiableAttributes = List.of(
                    new IdentifiableAttributes("GH1", IdentifiableType.GENERATOR, 0.0),
                    new IdentifiableAttributes("GH2", IdentifiableType.GENERATOR, 100.0),
                    new IdentifiableAttributes("GH3", IdentifiableType.GENERATOR, 100.0),
                    new IdentifiableAttributes("GTH1", IdentifiableType.GENERATOR, 100.0),
                    new IdentifiableAttributes("GTH2", IdentifiableType.GENERATOR, 100.0),
                    new IdentifiableAttributes("GTH3", IdentifiableType.GENERATOR, 100.0));
        Map<UUID, FilterEquipments> filters = Map.of(FILTER_ID_ALL_GEN, FilterEquipments.builder().filterId(FILTER_ID_ALL_GEN).identifiableAttributes(identifiableAttributes).build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);

        var filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_ALL_GEN)
                .build();
        final double variationValue = 100D;
        var variation = ScalingVariationInfos.builder()
                .variationMode(variationMode)
                .variationValue(variationValue)
                .filters(List.of(filter))
                .build();
        var generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification();
        generatorScaling.initApplicationContext(filterService);
        generatorScaling.apply(getNetwork());

        // If we sum the targetP for all expected modified generators, we should have the requested variation value
        double connectedGeneratorsTargetP = modifiedGenerators
                .stream()
                .map(g -> getNetwork().getGenerator(g).getTargetP())
                .reduce(0D, Double::sum);
        assertEquals(variationValue, connectedGeneratorsTargetP, 0.001D);
    }

    @Override
    protected void checkModification() {
    }
}
