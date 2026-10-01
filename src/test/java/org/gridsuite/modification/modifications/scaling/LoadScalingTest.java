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
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.context.loaders.FilterWithDistributionKeysLoader;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.scaling.LoadScalingInfos;
import org.gridsuite.modification.dto.scaling.ScalingVariationInfos;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.error.NetworkModificationExceptionType;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.modifications.data.ScalingVariationData;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.gridsuite.modification.utils.TestUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.nio.file.Paths;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;

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
    private static final String DISTRIBUTION_KEYS_ISSUE_MESSAGE = "Ventilation mode could not be applied: the total of the distribution keys "
            + "of the selected equipment is zero, so no key could weight the variation. A distribution key is taken into account only "
            + "if every filter of the variation was found, carries at least one key, and no equipment is selected by two filters.";

    private static final Map<UUID, Set<String>> FILTER_MAPPINGS = Map.of(
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
    private final FilterWithDistributionKeysLoader filterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, FILTER_MAPPINGS, DISTRIBUTION_KEYS_MAPPING);

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
        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();
        LoadScaling loadScaling = (LoadScaling) modificationInfo.toModification(modificationContext);
        loadScaling.apply(getNetwork());
        assertAfterNetworkModificationApplication();
    }

    @Test
    void loadIsCalledOnceForAllVariationsSharingAFilters() {
        List<List<UUID>> calls = new ArrayList<>();
        FilterWithDistributionKeysLoader countingLoader = filterUuids -> {
            calls.add(filterUuids);
            return this.filterWithDistributionKeysLoader.load(filterUuids);
        };
        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(countingLoader).build();

        ((LoadScalingInfos) buildModification()).toModification(modificationContext);

        assertThat(calls).as("The filters of all the variations are resolved in a single call").hasSize(1);
        assertThat(calls.getFirst()).as("FILTER_ID_3 is shared by two variations, it is requested once")
                .containsExactlyInAnyOrder(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5);
    }

    @Test
    void eachVariationKeepsOnlyItsOwnFilters() {
        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();

        LoadScaling loadScaling = (LoadScaling) ((LoadScalingInfos) buildModification()).toModification(modificationContext);
        List<ScalingVariationData> scalingVariations = loadScaling.getScalingVariations();

        // the filters are resolved once for the whole scaling: no variation may see the filters of another one
        assertEquals(equipmentIdsOf(scalingVariations.get(0).getFilters()), Set.of(LOAD_ID_3, LOAD_ID_4));
        assertEquals(equipmentIdsOf(scalingVariations.get(1).getFilters()), Set.of(LOAD_ID_7, LOAD_ID_8));
        assertEquals(equipmentIdsOf(scalingVariations.get(2).getFilters()), Set.of(LOAD_ID_1, LOAD_ID_2, LOAD_ID_9, LOAD_ID_10));
        assertEquals(equipmentIdsOf(scalingVariations.get(3).getFilters()), Set.of(LOAD_ID_5, LOAD_ID_6));
        assertEquals(equipmentIdsOf(scalingVariations.get(4).getFilters()), Set.of(LOAD_ID_5, LOAD_ID_6));
    }

    @Test
    void aSharedFilterDoesNotInvalidateTheDistributionKeysOfEitherVariation() {
        // load1 is selected by two different filters: each variation alone is valid, ventilation of the
        // first one must not be invalidated by the filter belonging to the second one
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_1, Set.of(LOAD_ID_1),
                FILTER_ID_2, Set.of(LOAD_ID_1));
        FilterWithDistributionKeysLoader loader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, Map.of(LOAD_ID_1, 1.0));
        LoadScalingInfos loadScalingInfo = loadScalingInfosOver(FilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                FilterInfos.builder().id(FILTER_ID_2).name("filter2").build());

        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(ModificationContext.builder().filterWithDistributionKeysLoader(loader).build());
        ReportNode report = loadScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        loadScaling.apply(getNetwork(), report);

        assertThat(TestUtils.getAllMessages(report)).as("No variation reports a distribution keys issue")
                .noneMatch(message -> message.contains(DISTRIBUTION_KEYS_ISSUE_MESSAGE));
        assertEquals(200, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D, "load1 is scaled once per variation");
    }

    @Test
    void aFilterSharedByTwoVariationsKeepsItsDistributionKeysInBoth() {
        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();
        FilterInfos filter = FilterInfos.builder().id(FILTER_ID_3).name("filter3").build();

        LoadScaling loadScaling = (LoadScaling) loadScalingInfosOver(filter, filter).toModification(modificationContext);
        loadScaling.apply(getNetwork());

        Map<String, Double> expectedDistributionKeys = Map.of(LOAD_ID_5, 6.0, LOAD_ID_6, 7.0);
        loadScaling.getScalingVariations().forEach(scalingVariation -> assertEquals(expectedDistributionKeys, scalingVariation.getDistributionKeys(),
                "The deduplicated resolution of the shared filter must keep its distribution keys"));
        // 50 MW of key-weighted ventilation, i.e. 6/13 and 7/13 of it, applied twice
        assertEquals(246.15, getNetwork().getLoad(LOAD_ID_5).getP0(), 0.01D);
        assertEquals(173.85, getNetwork().getLoad(LOAD_ID_6).getP0(), 0.01D);
    }

    private ScalingVariationInfos ventilationVariation(FilterInfos filter) {
        return ScalingVariationInfos.builder()
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationMode(VariationMode.VENTILATION)
                .variationValue(50D)
                .filters(List.of(filter))
                .build();
    }

    private LoadScalingInfos loadScalingInfosOver(FilterInfos... filters) {
        return LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.DELTA_P)
                .variations(Arrays.stream(filters).map(this::ventilationVariation).toList())
                .build();
    }

    private static Set<String> equipmentIdsOf(List<Filter> filters) {
        return filters.stream()
                .map(IdentifierListFilter.class::cast)
                .map(IdentifierListFilter::getEquipmentIds)
                .flatMap(Set::stream)
                .collect(Collectors.toSet());
    }

    @Test
    void testVentilationModeWithoutDistributionKey() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_NO_DK, Set.of(LOAD_ID_2, LOAD_ID_3));
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, Collections.emptyMap());

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

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        LoadScaling loadScaling = (LoadScaling) modificationToCreate.toModification(modificationContext);
        loadScaling.apply(getNetwork());

        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
    }

    @Test
    void testFilterWithWrongIds() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_WRONG_ID_1, Collections.emptySet());
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, Collections.emptyMap());

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

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(modificationContext);
        ReportNode report = loadScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        loadScaling.apply(getNetwork(), report);
        assertLogMessage("Preparing 1 scaling variations for equipments of type=LOAD",
                "network.modification.scaling.preparingScalingVariations", report);
        assertLogMessage("No equipment evaluated by filters",
                "network.modification.filterEvaluationResult.noResult", report);
    }

    @Test
    void testScalingCreationWithWarning() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_5, Set.of(LOAD_ID_9, LOAD_ID_10),
                FILTER_WRONG_ID_2, Set.of(LOAD_WRONG_ID_1));
        Map<String, Double> distributionKeysMapping = Map.of(LOAD_ID_9, 0.0, LOAD_ID_10, 9.0, LOAD_WRONG_ID_1, 2.0);
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, distributionKeysMapping);

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

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(modificationContext);
        loadScaling.apply(getNetwork());
        assertEquals(600, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(300, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
    }

    @Test
    void testVentilationWithWrongIdScalesFoundLoadsOnly() {
        // wrongId1 is not in the network: its distribution key must not be part of the ventilation
        // sum, otherwise the percentages do not add up to 100 and the whole variation fails.
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_1, Set.of(LOAD_ID_1, LOAD_ID_2),
                FILTER_WRONG_ID_2, Set.of(LOAD_WRONG_ID_1));
        Map<String, Double> distributionKeysMapping = Map.of(LOAD_ID_1, 1.0, LOAD_ID_2, 2.0, LOAD_WRONG_ID_1, 2.0);
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, distributionKeysMapping);

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_1)
                .build();

        FilterInfos filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_WRONG_ID_2)
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationMode(VariationMode.VENTILATION)
                .variationValue(600D)
                .filters(List.of(filter, filter2))
                .build();

        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(modificationContext);
        loadScaling.apply(getNetwork());

        // load1 and load2 are at 100 and 200, so 300 MW are to be added over a key sum of 1.0 + 2.0,
        // which splits it 1:2 between load1 and load2
        assertEquals(200, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(400, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
    }

    @Test
    void testVentilationWithDuplicatedEquipmentReportsAnError() {
        // load2 is selected by both filters: ventilation cannot pick one of the two distribution keys
        // for it, so the variation must be reported as an error and nothing must be scaled.
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_1, Set.of(LOAD_ID_1, LOAD_ID_2),
                FILTER_ID_2, Set.of(LOAD_ID_2, LOAD_ID_3));
        Map<String, Double> distributionKeysMapping = Map.of(LOAD_ID_1, 1.0, LOAD_ID_2, 2.0, LOAD_ID_3, 3.0);
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, distributionKeysMapping);

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_1)
                .build();

        FilterInfos filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_ID_2)
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationMode(VariationMode.VENTILATION)
                .variationValue(600D)
                .filters(List.of(filter, filter2))
                .build();

        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(modificationContext);
        ReportNode report = loadScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        loadScaling.apply(getNetwork(), report);

        assertLogMessage(DISTRIBUTION_KEYS_ISSUE_MESSAGE, "network.modification.distributionKeysIssue", report);
        assertEquals(100, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
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
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();
        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(modificationContext);
        ReportNode report = loadScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        loadScaling.apply(getNetwork(), report);
        return report;
    }

    @Test
    void testFilteredDuplicatedEquipmentsRemoved() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_4, Set.of(LOAD_ID_9, LOAD_ID_10),
                FILTER_ID_5, Set.of(LOAD_ID_9));
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, Collections.emptyMap());

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

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(modificationContext);
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

    @ParameterizedTest
    @EnumSource(value = VariationMode.class, names = {"STACKING_UP", "PROPORTIONAL_TO_PMAX"})
    void testUnsupportedVariations(VariationMode variationMode) {
        FilterInfos filter = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(variationMode)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(50D)
                .filters(List.of(filter))
                .build();

        LoadScalingInfos loadScalingInfos = LoadScalingInfos.builder()
                .stashed(false)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfos.toModification(modificationContext);

        NetworkModificationException networkModificationException = assertThrows(NetworkModificationException.class, () -> loadScaling.apply(getNetwork()));
        assertTrue(networkModificationException.getMessage().contains("This variation mode is not supported"));
    }

    @Test
    void nullVariationModeIsRejectedAsAnUnsupportedVariation() {
        // Nothing validates variationMode on the DTO, so a client may omit it. The unsupported mode
        // error must be raised rather than a NullPointerException, which is what currently happens:
        // the preparingScalingVariation report node calls getVariationMode().name() before the mode is
        // ever dispatched. The default branch of applyVariation cannot help either, since a switch on a
        // null enum throws while building its jump table.
        FilterInfos filter = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(null)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(50D)
                .filters(List.of(filter))
                .build();

        LoadScalingInfos loadScalingInfos = LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation))
                .build();

        LoadScaling loadScaling = (LoadScaling) loadScalingInfos.toModification(
                ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build());

        // the null mode survives toModification untouched, so the failure is an apply time one
        assertNull(loadScaling.getScalingVariations().get(0).getVariationMode());

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> loadScaling.apply(getNetwork()),
                "A missing variation mode must be reported as an unsupported variation, not as an NPE");
        assertTrue(exception.getMessage().contains("This variation mode is not supported"));
        assertTrue(exception.getMessage().startsWith(NetworkModificationExceptionType.LOAD_SCALING_ERROR.getMessage()),
                "The exception must carry the type hardcoded by the constructor");
    }

    @Test
    void nullVariationModeIsRejectedEvenWhenNoFilterMatches() {
        // Before the migration, filters were resolved at apply time and a variation matching nothing was
        // reported without ever reading the variation mode. The mode is now read while building the
        // report node, before the filters are evaluated, so a missing mode must fail the same way
        // whether the filters select something or not.
        FilterInfos missingFilter = FilterInfos.builder()
                .id(FILTER_WRONG_ID_1)
                .name("filter")
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(null)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(50D)
                .filters(List.of(missingFilter))
                .build();

        LoadScalingInfos loadScalingInfos = LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation))
                .build();

        ModificationContext modificationContext = ModificationContext.builder()
                .filterWithDistributionKeysLoader(TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD,
                        Map.of(FILTER_WRONG_ID_1, Set.of(LOAD_WRONG_ID_1)), Map.of(LOAD_WRONG_ID_1, 1.0)))
                .build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfos.toModification(modificationContext);

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> loadScaling.apply(getNetwork()),
                "A missing variation mode is an error whether or not the filters select an equipment");
        assertTrue(exception.getMessage().contains("This variation mode is not supported"));
    }

    @Test
    void nullVariationValueIsRejected() {
        // getAsked returns the variation value as a primitive, so a null one is unboxed blindly
        FilterInfos filter = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(null)
                .filters(List.of(filter))
                .build();

        LoadScalingInfos loadScalingInfos = LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation))
                .build();

        LoadScaling loadScaling = (LoadScaling) loadScalingInfos.toModification(
                ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build());

        assertThrows(NetworkModificationException.class, () -> loadScaling.apply(getNetwork()),
                "A missing variation value must be reported, not unboxed into a NullPointerException");
    }

    @Test
    void nullReactiveVariationModeIsRejected() {
        // provideScalingParameters switches on the reactive variation mode, which throws on a null one
        FilterInfos filter = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(50D)
                .filters(List.of(filter))
                .build();

        LoadScalingInfos loadScalingInfos = LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation))
                .build();

        LoadScaling loadScaling = (LoadScaling) loadScalingInfos.toModification(
                ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build());

        assertThrows(NetworkModificationException.class, () -> loadScaling.apply(getNetwork()),
                "A missing reactive variation mode must be reported, not switched on blindly");
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
        FilterWithDistributionKeysLoader customFilterWithDistributionKeysLoader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, distributionKeysMapping);

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

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(customFilterWithDistributionKeysLoader).build();
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(modificationContext);
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
