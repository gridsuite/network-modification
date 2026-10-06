/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.modifications.scaling;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import org.gridsuite.modification.modifications.data.scaling.VariationFilterData;

import java.util.*;

/**
 * The distribution keys of the equipments selected by a variation, and their total.
 *
 * <p>Built by {@link #resolve}, which guarantees one non-null key per selected equipment and a non-zero
 * total, so {@link #percentageOf(String)} never sees a missing key.
 *
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
public record DistributionKeys(Map<String, Double> keys, double total) {

    public double percentageOf(String equipmentId) {
        return keys.get(equipmentId) / total * 100;
    }

    /**
     * Checks the keys a variation carries against the equipments its filters selected, and reports the
     * first reason they cannot be used.
     *
     * <p>The keys are usable only if every filter was resolved and carries a non-empty key map, if no
     * equipment is keyed by more than one filter, if every selected equipment has a non-null key, and if
     * the total is not zero. All or nothing.
     *
     * @param filters the filters of the variation, with their keys
     * @param selectedEquipmentIds the ids those filters actually selected
     * @param reportNode where the outcome is reported
     * @return the usable keys and their total, or {@code null} if they cannot be used, in which case the
     * reason has already been reported as an ERROR
     */
    public static DistributionKeys resolve(List<VariationFilterData> filters, Collection<String> selectedEquipmentIds, ReportNode reportNode) {
        Map<String, Double> keys = new LinkedHashMap<>();
        for (VariationFilterData filter : filters) {
            if (!filter.isResolved()) {
                return reportError(DistributionKeyStatus.MISSING_FILTER, reportNode);
            }
            if (filter.distributionKeys().isEmpty()) {
                return reportError(DistributionKeyStatus.FILTER_HAS_NO_KEYS, reportNode);
            }
            for (Map.Entry<String, Double> key : filter.distributionKeys().entrySet()) {
                if (keys.containsKey(key.getKey())) {
                    return reportError(DistributionKeyStatus.DUPLICATED_EQUIPMENT_KEY, reportNode);
                }
                keys.put(key.getKey(), key.getValue());
            }
        }

        Map<String, Double> selectedKeys = new LinkedHashMap<>();
        double total = 0;
        for (String equipmentId : selectedEquipmentIds) {
            Double key = keys.get(equipmentId);
            if (key == null) {
                return reportError(DistributionKeyStatus.MISSING_EQUIPMENT_KEY, reportNode);
            }
            selectedKeys.put(equipmentId, key);
            total += key;
        }
        if (total == 0) {
            return reportError(DistributionKeyStatus.UNEXPECTED_SUM, reportNode);
        }

        return report(DistributionKeyStatus.VALID_KEYS, reportNode, TypedValue.INFO_SEVERITY, selectedKeys, total);
    }

    private static DistributionKeys reportError(DistributionKeyStatus status, ReportNode reportNode) {
        return report(status, reportNode, TypedValue.ERROR_SEVERITY, Map.of(), 0);
    }

    private static DistributionKeys report(DistributionKeyStatus status, ReportNode reportNode, TypedValue severity,
                                           Map<String, Double> keys, double total) {
        reportNode.newReportNode()
                .withMessageTemplate(status.getReportKey())
                .withSeverity(severity)
                .add();
        return DistributionKeyStatus.VALID_KEYS.equals(status) ? new DistributionKeys(keys, total) : null;
    }
}
