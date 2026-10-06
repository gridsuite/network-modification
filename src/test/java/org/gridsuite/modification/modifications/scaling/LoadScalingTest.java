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
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.gridsuite.modification.context.loaders.FilterWithDistributionKeysLoader;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.scaling.LoadScalingInfos;
import org.gridsuite.modification.dto.scaling.ScalingVariationInfos;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.modifications.data.scaling.ScalingVariationData;
import org.gridsuite.modification.modifications.data.scaling.VariationFilterData;
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
import java.util.function.Function;
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
    /** Reported when an equipment is given a key by more than one filter of the variation. */
    private static final String DUPLICATED_KEY_MESSAGE = "Ventilation mode could not be applied: multiple distribution keys "
            + "exist for the same equipment across filters";
    /** Every error a variation reports when its distribution keys are unusable shares this prefix. */
    private static final String DISTRIBUTION_KEYS_ERROR_PREFIX = "Ventilation mode could not be applied: ";

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

        assertThat(TestUtils.getAllMessages(report)).as("No variation reports a distribution keys error")
                .noneMatch(message -> message.contains(DISTRIBUTION_KEYS_ERROR_PREFIX));
        assertEquals(200, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D, "load1 is scaled once per variation");
    }

    @Test
    void aFilterSharedByTwoVariationsKeepsItsDistributionKeysInBoth() {
        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();
        FilterInfos filter = FilterInfos.builder().id(FILTER_ID_3).name("filter3").build();

        LoadScaling loadScaling = (LoadScaling) loadScalingInfosOver(filter, filter).toModification(modificationContext);
        loadScaling.apply(getNetwork());

        Map<String, Double> expectedDistributionKeys = Map.of(LOAD_ID_5, 6.0, LOAD_ID_6, 7.0);
        loadScaling.getScalingVariations().forEach(scalingVariation -> {
            List<VariationFilterData> filters = scalingVariation.getFilters();
            assertEquals(1, filters.size(), "The reference is resolved once, and stays a single filter");
            assertEquals(expectedDistributionKeys, filters.getFirst().distributionKeys(),
                    "A filter shared by two variations keeps its distribution keys in both of them");
        });
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

    private static Set<String> equipmentIdsOf(List<VariationFilterData> filters) {
        return filters.stream()
                .map(VariationFilterData::filter)
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

        assertLogMessage(DUPLICATED_KEY_MESSAGE, "network.modification.distributionKeys.duplicatedKey", report);
        assertEquals(100, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
    }

    @Test
    void aFilterThatCannotBeResolvedIsReportedInsteadOfBeingDropped() {
        // the loader cannot resolve the second filter, so the variation must say so rather than scale the
        // equipments of the one it did resolve with a value sized for both
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_1, Set.of(LOAD_ID_1, LOAD_ID_2));
        Map<String, Double> distributionKeys = Map.of(LOAD_ID_1, 1.0, LOAD_ID_2, 2.0);
        FilterWithDistributionKeysLoader loader = omittingUnknownFilters(
                TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, distributionKeys));
        LoadScalingInfos loadScalingInfo = ventilationOver(FilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                FilterInfos.builder().id(UUID.randomUUID()).name("deletedFilter").build());

        ReportNode report = applyAndReport(loader, loadScalingInfo);

        assertLogMessage("Ventilation mode could not be applied: one of the filters is missing",
                "network.modification.distributionKeys.missingFilter", report);
        assertEquals(100, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D, "nothing is scaled when a filter is missing");
        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
    }

    @Test
    void aFilterKeyingOnlyPartOfWhatItSelectsIsReportedInsteadOfThrowing() {
        // load2 is selected but has no key: it cannot be weighted, so the variation reports it rather than
        // dividing every share by a null key
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_1, Set.of(LOAD_ID_1, LOAD_ID_2));
        Map<String, Double> partialKeys = new HashMap<>();
        partialKeys.put(LOAD_ID_1, 1.0);
        FilterWithDistributionKeysLoader loader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, partialKeys);
        LoadScalingInfos loadScalingInfo = ventilationOver(FilterInfos.builder().id(FILTER_ID_1).name("filter1").build());

        ReportNode report = applyAndReport(loader, loadScalingInfo);

        assertLogMessage("Ventilation mode could not be applied: at least one equipment is missing a distribution key",
                "network.modification.distributionKeys.missingEquipmentKey", report);
        assertEquals(100, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
    }

    @Test
    void distributionKeysThatAddUpToZeroAreReported() {
        Map<UUID, Set<String>> filterMappings = Map.of(FILTER_ID_1, Set.of(LOAD_ID_1, LOAD_ID_2));
        Map<String, Double> zeroKeys = Map.of(LOAD_ID_1, 0.0, LOAD_ID_2, 0.0);
        FilterWithDistributionKeysLoader loader = TestUtils.createFilterWithDistributionKeysLoader(EquipmentType.LOAD, filterMappings, zeroKeys);
        LoadScalingInfos loadScalingInfo = ventilationOver(FilterInfos.builder().id(FILTER_ID_1).name("filter1").build());

        ReportNode report = applyAndReport(loader, loadScalingInfo);

        assertLogMessage("Ventilation mode could not be applied: the total of the distribution keys of the selected equipment is zero",
                "network.modification.distributionKeys.unexpectedSum", report);
        assertEquals(100, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
    }

    /** One ventilation variation over all the given filters, as opposed to one variation per filter. */
    private LoadScalingInfos ventilationOver(FilterInfos... filters) {
        return LoadScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(ScalingVariationInfos.builder()
                        .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                        .variationMode(VariationMode.VENTILATION)
                        .variationValue(50D)
                        .filters(Arrays.asList(filters))
                        .build()))
                .build();
    }

    /** {@link TestUtils} loaders resolve every identifier they are given; a real one may not. */
    private static FilterWithDistributionKeysLoader omittingUnknownFilters(FilterWithDistributionKeysLoader delegate) {
        Map<UUID, FilterWithDistributionKeys> known = delegate.load(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5));
        return filterUuids -> filterUuids.stream()
                .filter(known::containsKey)
                .collect(Collectors.toMap(Function.identity(), known::get));
    }

    private ReportNode applyAndReport(FilterWithDistributionKeysLoader loader, LoadScalingInfos loadScalingInfo) {
        LoadScaling loadScaling = (LoadScaling) loadScalingInfo.toModification(ModificationContext.builder().filterWithDistributionKeysLoader(loader).build());
        ReportNode report = loadScalingInfo.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        loadScaling.apply(getNetwork(), report);
        return report;
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
        Network network = getNetwork();
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

        NetworkModificationException networkModificationException = assertThrows(NetworkModificationException.class, () -> loadScaling.apply(network));
        assertTrue(networkModificationException.getMessage().contains("This variation mode is not supported"));
    }

    @Test
    void nullVariationModeIsRejectedWhenTheModificationIsBuilt() {
        // A client may omit variationMode. toModification calls check() before anything is built, so
        // the modification is never created and no Network can be left half applied. This used to be
        // an apply time failure, once the preparingScalingVariation report node had already called
        // getVariationMode().name() and the switch of applyVariation was building its jump table.
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

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> loadScalingInfos.toModification(modificationContext),
                "A missing variation mode must be reported as an invalid modification, not as an NPE");
        assertEquals("Invalid modification : Attribute 'variationMode' is missing from modification", exception.getMessage());
    }

    @Test
    void nullVariationModeIsRejectedEvenWhenNoFilterMatches() {
        // The attributes are checked before the filters are resolved, so a missing mode must be
        // rejected the same way whether or not the filters would have selected an equipment. Before
        // the migration, filters were resolved at apply time and a variation matching nothing was
        // reported without ever reading the variation mode.
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

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> loadScalingInfos.toModification(modificationContext),
                "A missing variation mode is an error whether or not the filters select an equipment");
        assertEquals("Invalid modification : Attribute 'variationMode' is missing from modification", exception.getMessage());
    }

    @Test
    void nullVariationValueIsRejected() {
        // getAsked returns the variation value as a primitive, so a null one used to be unboxed blindly
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

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> loadScalingInfos.toModification(modificationContext),
                "A missing variation value must be reported, not unboxed into a NullPointerException");
        assertEquals("Invalid modification : Attribute 'variationValue' is missing from modification", exception.getMessage());
    }

    @Test
    void nullReactiveVariationModeIsRejected() {
        // provideScalingParameters switches on the reactive variation mode, which used to throw on a
        // null one. Only LoadScalingInfos requires it, so this check is specific to load scaling.
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

        ModificationContext modificationContext = ModificationContext.builder().filterWithDistributionKeysLoader(this::loadFiltersWithDistributionKeys).build();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> loadScalingInfos.toModification(modificationContext),
                "A missing reactive variation mode must be reported, not switched on blindly");
        assertEquals("Invalid modification : Attribute 'reactiveVariationMode' is missing from modification", exception.getMessage());
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
