/*
 * Copyright (c) 2024-2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.modifications.scaling;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.impl.NetworkFactoryImpl;
import lombok.Getter;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.context.loaders.FilterWithDistributionKeysLoader;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.scaling.GeneratorScalingInfos;
import org.gridsuite.modification.dto.scaling.ScalingVariationInfos;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
    private static final String DUPLICATED_KEY_MESSAGE = "Ventilation mode could not be applied: multiple distribution keys "
            + "exist for the same equipment across filters";

    private static final Map<UUID, Set<String>> FILTER_MAPPINGS = Map.of(
            FILTER_ID_1, Set.of(GENERATOR_ID_1, GENERATOR_ID_2),
            FILTER_ID_2, Set.of(GENERATOR_ID_3, GENERATOR_ID_4),
            FILTER_ID_3, Set.of(GENERATOR_ID_5, GENERATOR_ID_6),
            FILTER_ID_4, Set.of(GENERATOR_ID_7, GENERATOR_ID_8),
            FILTER_ID_5, Set.of(GENERATOR_ID_9, GENERATOR_ID_10));

    private static final Map<String, Double> DISTRIBUTION_KEY_MAPPINGS = Map.of(
            GENERATOR_ID_1, 1.0, GENERATOR_ID_2, 2.0,
            GENERATOR_ID_3, 2.0, GENERATOR_ID_4, 5.0,
            GENERATOR_ID_5, 6.0, GENERATOR_ID_6, 7.0,
            GENERATOR_ID_7, 3.0, GENERATOR_ID_8, 8.0,
            GENERATOR_ID_9, 0.0, GENERATOR_ID_10, 9.0
    );

    @Getter
    private final FilterWithDistributionKeysLoader filterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.GENERATOR,
            FILTER_MAPPINGS, DISTRIBUTION_KEY_MAPPINGS);

    @BeforeEach
    void specificSetUp() {
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

    @Test
    @Override
    public void testApply() throws Exception {
        GeneratorScalingInfos modificationInfo = (GeneratorScalingInfos) buildModification();
        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) modificationInfo.toModification(modificationContext);
        assertFalse(generatorScaling.getScalingVariations().get(3).getFilters().isEmpty(),
                "toModification resolves the filters of every variation");
        assertTrue(generatorScaling.getScalingVariations().stream()
                        .flatMap(variation -> variation.getFilters().stream())
                        .allMatch(filter -> filter.filter() != null && !filter.distributionKeys().isEmpty()),
                "every filter of every variation is resolved, with the distribution keys it was loaded with");
        generatorScaling.apply(getNetwork());
        assertAfterNetworkModificationApplication();
    }

    @Test
    void testVentilationModeWithoutDistributionKey() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_NO_DK, Set.of(GENERATOR_ID_2, GENERATOR_ID_3));
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.GENERATOR, filterMappings, Collections.emptyMap());

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

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) modificationToCreate.toModification(modificationContext);
        generatorScaling.apply(getNetwork());

        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0.01D);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 0.01D);
    }

    @Test
    void testFilterWithWrongIds() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_WRONG_ID_1, Collections.emptySet());
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.GENERATOR, filterMappings, Collections.emptyMap());

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_WRONG_ID_1)
                .build();
        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();
        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification(modificationContext);
        ReportNode report = generatorScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        generatorScaling.apply(getNetwork(), report);
        assertLogMessage("No equipment evaluated by filters",
                "network.modification.filterEvaluationResult.noResult", report);
    }

    @Test
    void testScalingCreationWithWarning() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_5, Set.of(GENERATOR_ID_9, GENERATOR_ID_10),
                FILTER_WRONG_ID_2, Set.of(GENERATOR_WRONG_ID_1));
        Map<String, Double> distributionKeysMapping = Map.of(GENERATOR_ID_9, 0.0, GENERATOR_ID_10, 9.0, GENERATOR_WRONG_ID_1, 2.0);
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.GENERATOR, filterMappings, distributionKeysMapping);

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
                .variationValue(900D)
                .filters(List.of(filter, filter2))
                .build();
        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification(modificationContext);
        generatorScaling.apply(getNetwork());

        assertEquals(600, getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 0.01D);
        assertEquals(300, getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 0.01D);
    }

    @Test
    void testVentilationWithWrongIdScalesFoundEquipmentsOnly() {
        // wrongId1 is not in the network: its distribution key must not be part of the ventilation
        // sum, otherwise the percentages do not add up to 100 and the whole variation fails.
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_1, Set.of(GENERATOR_ID_1, GENERATOR_ID_2),
                FILTER_WRONG_ID_2, Set.of(GENERATOR_WRONG_ID_1));
        Map<String, Double> distributionKeysMapping = Map.of(GENERATOR_ID_1, 1.0, GENERATOR_ID_2, 2.0, GENERATOR_WRONG_ID_1, 2.0);
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.GENERATOR, filterMappings, distributionKeysMapping);

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_1)
                .build();

        FilterInfos filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_WRONG_ID_2)
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .variationValue(600D)
                .filters(List.of(filter, filter2))
                .build();

        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification(modificationContext);
        generatorScaling.apply(getNetwork());

        // gen1 and gen2 are at 100 and 200, so 300 MW are to be added over a key sum of 1.0 + 2.0,
        // which splits it 1:2 between gen1 and gen2
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 0.01D);
        assertEquals(400, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0.01D);
    }

    @Test
    void testVentilationWithDuplicatedEquipmentReportsAnError() {
        // gen2 is selected by both filters: ventilation cannot pick one of the two distribution keys
        // for it, so the variation must be reported as an error and nothing must be scaled.
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_1, Set.of(GENERATOR_ID_1, GENERATOR_ID_2),
                FILTER_ID_2, Set.of(GENERATOR_ID_2, GENERATOR_ID_3));
        Map<String, Double> distributionKeysMapping = Map.of(GENERATOR_ID_1, 1.0, GENERATOR_ID_2, 2.0, GENERATOR_ID_3, 3.0);
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.GENERATOR, filterMappings, distributionKeysMapping);

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_1)
                .build();

        FilterInfos filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_ID_2)
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .variationValue(600D)
                .filters(List.of(filter, filter2))
                .build();

        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification(modificationContext);
        ReportNode report = generatorScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        generatorScaling.apply(getNetwork(), report);

        assertLogMessage(DUPLICATED_KEY_MESSAGE, "network.modification.distributionKeys.duplicatedKey", report);
        assertEquals(100, getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 0.01D);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0.01D);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 0.01D);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createGeneratorsNetwork(networkUuid, new NetworkFactoryImpl());
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
                .variationMode(VariationMode.PROPORTIONAL_TO_PMAX)
                .variationValue(50D)
                .filters(List.of(filter1))
                .build();

        ScalingVariationInfos variation2 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .variationValue(50D)
                .filters(List.of(filter2))
                .build();

        ScalingVariationInfos variation3 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.STACKING_UP)
                .variationValue(50D)
                .filters(List.of(filter3))
                .build();

        ScalingVariationInfos variation4 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .variationValue(50D)
                .filters(List.of(filter4))
                .build();

        ScalingVariationInfos variation5 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(50D)
                .filters(List.of(filter1, filter5))
                .build();

        return GeneratorScalingInfos.builder()
                .stashed(false)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1, variation2, variation3, variation4, variation5))
                .build();
    }

    @Test
    void testFilteredDuplicatedEquipmentsRemoved() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_4, Set.of(GENERATOR_ID_9, GENERATOR_ID_10),
                FILTER_ID_5, Set.of(GENERATOR_ID_9));
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.GENERATOR, filterMappings, Collections.emptyMap());

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_4)
                .build();

        FilterInfos filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_ID_5)
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(900D)
                .filters(List.of(filter, filter2))
                .build();
        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification(modificationContext);
        ReportNode report = generatorScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        generatorScaling.apply(getNetwork(), report);

        assertEquals(600, getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 0.01D);
        assertEquals(300, getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 0.01D);
        assertLogMessage("Equipment gen9 already seen in previous filter evaluation, skipping it",
                "network.modification.filterEvaluation.equipmentAlreadySeen", report);
    }

    @Test
    void filterReportUsesTheFilterNameWhenAvailable() {
        ReportNode report = applyScalingOverASingleFilter(FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("myFilter")
                .build());

        assertLogMessage("Evaluate filter myFilter", "network.modification.filterEvaluation", report);
    }

    @Test
    void filterReportFallsBackToAOneBasedIndexWhenTheFilterIsUnnamed() {
        ReportNode report = applyScalingOverASingleFilter(FilterInfos.builder()
                .id(FILTER_ID_1)
                .build());

        assertLogMessage("Evaluate filter 1", "network.modification.filterEvaluation", report);
    }

    private ReportNode applyScalingOverASingleFilter(FilterInfos filter) {
        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();
        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification(modificationContext);
        ReportNode report = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build();
        generatorScaling.apply(getNetwork(), report);
        return report;
    }

    @Test
    void testEqualVariationsReportDistinctIndexes() {
        FilterInfos filter = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();
        // Two distinct but equal variations: what a duplicated JSON payload or UI row produces
        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();
        ScalingVariationInfos duplicate = ScalingVariationInfos.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();
        assertEquals(variation, duplicate);

        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .uuid(GENERATOR_SCALING_ID)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation, duplicate))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification(modificationContext);
        ReportNode report = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build();
        generatorScaling.apply(getNetwork(), report);

        assertEquals(List.of("Variation 1/2 : 100.0MW in REGULAR_DISTRIBUTION mode",
                        "Variation 2/2 : 100.0MW in REGULAR_DISTRIBUTION mode"),
                TestUtils.getAllMessages(report).stream()
                        .filter(message -> message.startsWith("Variation "))
                        .toList());
        // The duplicate is really applied twice: 100 MW of DELTA_P is shared equally, so 50 MW on each generator and variation
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 0.01D);
        assertEquals(300, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0.01D);
    }

    @Test
    void nullVariationModeIsRejectedWhenTheModificationIsBuilt() {
        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(null)
                .variationValue(50D)
                .filters(List.of(FilterInfos.builder().id(FILTER_ID_1).name("filter1").build()))
                .build();

        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .uuid(GENERATOR_SCALING_ID)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> generatorScalingInfo.toModification(modificationContext),
                "A missing variation mode must be reported as an invalid modification, not as an NPE");
        assertEquals("Invalid modification : Attribute 'variationMode' is missing from modification", exception.getMessage());
    }

    @Test
    void nullVariationModeIsRejectedEvenWhenNoFilterMatches() {
        // The attributes are checked before the filters are resolved, so a missing mode must be
        // rejected the same way whether or not the filters would have selected an equipment.
        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(null)
                .variationValue(50D)
                .filters(List.of(FilterInfos.builder().id(FILTER_WRONG_ID_1).name("filter").build()))
                .build();

        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .uuid(GENERATOR_SCALING_ID)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder()
                .filterWithDistributionKeysLoader(TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.GENERATOR,
                        Map.of(FILTER_WRONG_ID_1, Set.of(GENERATOR_WRONG_ID_1)), Map.of(GENERATOR_WRONG_ID_1, 1.0)))
                .build();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> generatorScalingInfo.toModification(modificationContext),
                "A missing variation mode is an error whether or not the filters select an equipment");
        assertEquals("Invalid modification : Attribute 'variationMode' is missing from modification", exception.getMessage());
    }

    @Test
    void nullVariationValueIsRejected() {
        // getAsked returns the variation value as a primitive, so a null one used to be unboxed blindly
        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(null)
                .filters(List.of(FilterInfos.builder().id(FILTER_ID_1).name("filter1").build()))
                .build();

        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .uuid(GENERATOR_SCALING_ID)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> generatorScalingInfo.toModification(modificationContext),
                "A missing variation value must be reported, not unboxed into a NullPointerException");
        assertEquals("Invalid modification : Attribute 'variationValue' is missing from modification", exception.getMessage());
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

        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_ALL_GEN, Set.of("GH1", "GH2", "GH3", "GTH1", "GTH2", "GTH3"));
        Map<String, Double> distributionKeysMapping = Map.of("GH1", 0.0, "GH2", 100.0, "GH3", 100.0, "GTH1", 100.0, "GTH2", 100.0, "GTH3", 100.0);
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.GENERATOR, filterMappings, distributionKeysMapping);

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_ALL_GEN)
                .build();
        final double variationValue = 100D;
        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(variationMode)
                .variationValue(variationValue)
                .filters(List.of(filter))
                .build();
        GeneratorScalingInfos generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        GeneratorScaling generatorScaling = (GeneratorScaling) generatorScalingInfo.toModification(modificationContext);
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
