/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.modifications.scaling;

import com.powsybl.commons.report.ReportNode;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.modifications.data.scaling.VariationFilterData;
import org.gridsuite.modification.utils.TestUtils;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
class DistributionKeysTest {

    private static final String GEN_1 = "gen1";
    private static final String GEN_2 = "gen2";
    private static final String GEN_3 = "gen3";
    private static final String GEN_4 = "gen4";
    private static final String NOT_IN_THE_NETWORK = "notInTheNetwork";
    private static final String REPORT_KEY_FILTER_HAS_NO_KEYS = "network.modification.distributionKeys.filterHasNoKeys";
    private static final String REPORT_KEY_MISSING_EQUIPMENT_KEY = "network.modification.distributionKeys.missingEquipmentKey";
    private static final String REPORT_KEY_DUPLICATED_EQUIPMENT_KEY = "network.modification.distributionKeys.duplicatedKey";
    private static final String REPORT_KEY_UNEXPECTED_SUM = "network.modification.distributionKeys.unexpectedSum";
    private static final String REPORT_KEY_VALID_KEYS = "network.modification.distributionKeys.valid";
    private static final List<String> REPORT_KEY_REASONS = List.of(REPORT_KEY_FILTER_HAS_NO_KEYS, REPORT_KEY_MISSING_EQUIPMENT_KEY,
            REPORT_KEY_DUPLICATED_EQUIPMENT_KEY, REPORT_KEY_UNEXPECTED_SUM, REPORT_KEY_VALID_KEYS);

    /** (equipmentId, key) couples, so that a null key can be built, which Map.of does not allow. */
    private static Map<String, Double> keys(Object... equipmentIdAndKey) {
        Map<String, Double> keys = new HashMap<>();
        for (int i = 0; i < equipmentIdAndKey.length; i += 2) {
            keys.put((String) equipmentIdAndKey[i], (Double) equipmentIdAndKey[i + 1]);
        }
        return keys;
    }

    private static VariationFilterData aFilter(Map<String, Double> distributionKeys, String... selectedEquipmentIds) {
        IdentifierListFilter filter = IdentifierListFilter.builder()
                .equipmentType(EquipmentType.GENERATOR)
                .equipmentIds(Set.of(selectedEquipmentIds))
                .build();
        return new VariationFilterData(filter, distributionKeys);
    }

    private static ReportNode aReport() {
        return ReportNode.newRootReportNode().withMessageTemplate("test").build();
    }

    private static DistributionKeys resolve(ReportNode reportNode, List<VariationFilterData> filters, String... selectedEquipmentIds) {
        return DistributionKeys.resolve(filters, List.of(selectedEquipmentIds), reportNode);
    }

    /** The reason the variation refused to be ventilated, read back from the report it was given. */
    private static Optional<String> reportedReason(ReportNode reportNode) {
        List<String> messages = TestUtils.getAllMessages(reportNode);
        return REPORT_KEY_REASONS.stream()
                .filter(reason -> messages.stream().anyMatch(message -> message.contains(reason)))
                .findFirst();
    }

    @Test
    void theKeysOfEveryFilterAreGatheredAndWeightedByTheirTotal() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report,
                List.of(aFilter(keys(GEN_1, 1.0, GEN_2, 2.0), GEN_1, GEN_2),
                        aFilter(keys(GEN_3, 3.0, GEN_4, 0.5), GEN_3, GEN_4)),
                GEN_1, GEN_2, GEN_3, GEN_4);

        assertNotNull(distributionKeys);
        assertEquals(keys(GEN_1, 1.0, GEN_2, 2.0, GEN_3, 3.0, GEN_4, 0.5), distributionKeys.keys());
        assertEquals(6.5, distributionKeys.total());
        assertEquals(100 * 1.0 / 6.5, distributionKeys.percentageOf(GEN_1), 1e-9);
        assertEquals(100 * 0.5 / 6.5, distributionKeys.percentageOf(GEN_4), 1e-9);
        assertEquals(Optional.of(REPORT_KEY_VALID_KEYS), reportedReason(report));
    }

    @Test
    void aFilterWithoutAnyDistributionKeyInvalidatesTheWholeSet() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report,
                List.of(aFilter(keys(GEN_1, 1.0), GEN_1), aFilter(Map.of(), GEN_2)),
                GEN_1, GEN_2);

        assertNull(distributionKeys);
        assertEquals(Optional.of(REPORT_KEY_FILTER_HAS_NO_KEYS), reportedReason(report),
                "A filter whose equipments have no distribution key at all is not usable");
    }

    @Test
    void anEquipmentWithANullDistributionKeyInvalidatesTheWholeSet() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report, List.of(aFilter(keys(GEN_1, 1.0, GEN_2, null), GEN_1, GEN_2)), GEN_1, GEN_2);

        assertNull(distributionKeys);
        assertEquals(Optional.of(REPORT_KEY_MISSING_EQUIPMENT_KEY), reportedReason(report),
                "A null distribution key is not a valid distribution key");
    }

    @Test
    void anEquipmentKeyedByTwoFiltersInvalidatesTheWholeSet() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report,
                List.of(aFilter(keys(GEN_1, 1.0, GEN_2, 2.0), GEN_1, GEN_2),
                        aFilter(keys(GEN_2, 5.0, GEN_3, 3.0), GEN_2, GEN_3)),
                GEN_1, GEN_2, GEN_3);

        assertNull(distributionKeys, "An equipment selected twice cannot be weighted once, so nothing is weighted at all");
        assertEquals(Optional.of(REPORT_KEY_DUPLICATED_EQUIPMENT_KEY), reportedReason(report));
    }

    @Test
    void anEquipmentKeyedTwiceByNullKeysIsADuplicationNotAMissingKey() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report,
                List.of(aFilter(keys(GEN_1, null), GEN_1), aFilter(keys(GEN_1, null), GEN_1)),
                GEN_1);

        assertNull(distributionKeys);
        assertEquals(Optional.of(REPORT_KEY_DUPLICATED_EQUIPMENT_KEY), reportedReason(report),
                "Two filters keying the same equipment is a duplication, null keys or not");
    }

    @Test
    void aFilterResolvedOnlyOnceDoesNotKeyItsEquipmentsTwice() {
        ReportNode report = aReport();
        VariationFilterData filter = aFilter(keys(GEN_1, 1.0), GEN_1);

        DistributionKeys distributionKeys = resolve(report, List.of(filter), GEN_1);

        assertNotNull(distributionKeys, "A filter listed twice is one filter: it does not key its equipments twice");
        assertEquals(1.0, distributionKeys.total());
    }

    @Test
    void aZeroDistributionKeyIsAValidDistributionKey() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report, List.of(aFilter(keys(GEN_1, 0.0, GEN_2, 2.0), GEN_1, GEN_2)), GEN_1, GEN_2);

        assertNotNull(distributionKeys, "Only a null key is rejected, not a zero one");
        assertEquals(2.0, distributionKeys.total());
        assertEquals(0, distributionKeys.percentageOf(GEN_1), 1e-9, "a zero key is given no share of the variation");
        assertEquals(100, distributionKeys.percentageOf(GEN_2), 1e-9);
    }

    @Test
    void keysThatAddUpToZeroAreRejected() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report, List.of(aFilter(keys(GEN_1, 0.0, GEN_2, 0.0), GEN_1, GEN_2)), GEN_1, GEN_2);

        assertNull(distributionKeys, "A zero total would make every percentage infinite");
        assertEquals(Optional.of(REPORT_KEY_UNEXPECTED_SUM), reportedReason(report));
    }

    @Test
    void twoEquipmentsMayShareTheSameKeyValue() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report, List.of(aFilter(keys(GEN_1, 2.0, GEN_2, 2.0), GEN_1, GEN_2)), GEN_1, GEN_2);

        assertNotNull(distributionKeys, "Unique means one key per equipment, not one value per key");
        assertEquals(50, distributionKeys.percentageOf(GEN_1), 1e-9);
        assertEquals(50, distributionKeys.percentageOf(GEN_2), 1e-9);
    }

    @Test
    void aSelectedEquipmentWithoutAnyKeyIsRejectedInsteadOfBeingUnboxed() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report, List.of(aFilter(keys(GEN_1, 1.0), GEN_1, GEN_2)), GEN_1, GEN_2);

        assertNull(distributionKeys, "GEN_2 is selected but keyed by nobody: it cannot be given a share of the variation");
        assertEquals(Optional.of(REPORT_KEY_MISSING_EQUIPMENT_KEY), reportedReason(report));
    }

    @Test
    void aKeyOfAnEquipmentThatIsNotInTheNetworkDoesNotDiluteTheTotal() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report,
                List.of(aFilter(keys(GEN_1, 1.0, GEN_2, 2.0, NOT_IN_THE_NETWORK, 100.0), GEN_1, GEN_2, NOT_IN_THE_NETWORK)),
                GEN_1, GEN_2);

        assertNotNull(distributionKeys);
        assertEquals(3.0, distributionKeys.total(), "Only the keys of the equipments that are actually scaled weight the variation");
        assertEquals(100 * 2.0 / 3.0, distributionKeys.percentageOf(GEN_2), 1e-9);
    }

    @Test
    void aKeyOfAnEquipmentThatIsNotInTheNetworkMayBeNullWithoutFailing() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report,
                List.of(aFilter(keys(GEN_1, 1.0, NOT_IN_THE_NETWORK, null), GEN_1, NOT_IN_THE_NETWORK)),
                GEN_1);

        assertNotNull(distributionKeys);
        assertEquals(1.0, distributionKeys.total());
    }

    @Test
    void theFirstUnusableFilterDictatesTheReportedReason() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report,
                List.of(aFilter(keys(GEN_1, 1.0), GEN_1), aFilter(Map.of(), GEN_2), aFilter(keys(GEN_1, 5.0), GEN_1)),
                GEN_1);

        assertNull(distributionKeys, "The check stops at the first problem met, in reference order");
        assertEquals(Optional.of(REPORT_KEY_FILTER_HAS_NO_KEYS), reportedReason(report));
    }

    @Test
    void anInvalidFilterDiscardsTheKeysGatheredFromTheValidFiltersBeforeIt() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report,
                List.of(aFilter(keys(GEN_1, 1.0, GEN_2, 2.0), GEN_1, GEN_2), aFilter(keys(GEN_3, null), GEN_3)),
                GEN_1, GEN_2, GEN_3);

        assertNull(distributionKeys, "All or nothing: a partially gathered set must never leak out");
    }

    @Test
    void noEquipmentAtAllLeavesNothingToWeight() {
        ReportNode report = aReport();

        DistributionKeys distributionKeys = resolve(report, List.of(aFilter(keys(GEN_1, 1.0), GEN_1)));

        assertNull(distributionKeys, "An empty variation is never ventilated");
        assertEquals(Optional.of(REPORT_KEY_UNEXPECTED_SUM), reportedReason(report));
    }

    @Test
    void aNullKeyMapIsNormalisedToAnEmptyOne() {
        VariationFilterData filter = new VariationFilterData(aFilter(Map.of(), GEN_1).filter(), null);

        assertEquals(Map.of(), filter.distributionKeys(), "A null key map cannot even be built, whatever the loader answered");
    }
}
