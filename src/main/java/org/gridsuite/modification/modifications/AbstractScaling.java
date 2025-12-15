/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Network;

import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.CollectionUtils;

import static org.gridsuite.modification.utils.ModificationUtils.createReport;
import static org.gridsuite.modification.utils.ModificationUtils.distinctByKey;

import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public abstract class AbstractScaling extends AbstractModification {
    protected final ScalingInfos scalingInfos;

    protected IFilterService filterService;

    protected AbstractScaling(ScalingInfos scalingInfos) {
        this.scalingInfos = scalingInfos;
    }

    @Override
    public void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService) {
        this.filterService = filterService;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // collect all filters from all variations
        var filters = scalingInfos.getVariations().stream()
                .flatMap(v -> v.getFilters().stream())
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        Map<UUID, FilterEquipments> exportFilters = ModificationUtils.getUuidFilterEquipmentsMap(filterService, network, subReportNode, filters);
        if (exportFilters != null) {
            ModificationUtils.logWrongEquipmentsIdsFilters(subReportNode, exportFilters, filters);

            // apply variations
            scalingInfos.getVariations().forEach(variation -> {
                Set<IdentifiableAttributes> identifiableAttributes = ModificationUtils.getIdentifiableAttributes(exportFilters, variation.getFilters(), subReportNode);

                if (CollectionUtils.isEmpty(identifiableAttributes)) {
                    String filterNames = variation.getFilters().stream().map(FilterInfos::getName).collect(Collectors.joining(", "));
                    createReport(subReportNode, "network.modification.allFiltersWrong.variation", Map.of("filterNames", filterNames), TypedValue.WARN_SEVERITY);
                } else {
                    applyVariation(network, subReportNode, identifiableAttributes, variation);
                }
            });
            createReport(subReportNode, "network.modification.scalingCreated", Map.of(), TypedValue.INFO_SEVERITY);
        }
    }

    private void applyVariation(Network network,
                                ReportNode subReportNode,
                                Set<IdentifiableAttributes> identifiableAttributes,
                                ScalingVariationInfos variation) {
        switch (variation.getVariationMode()) {
            case PROPORTIONAL:
                applyProportionalVariation(network, subReportNode, identifiableAttributes, variation);
                break;
            case PROPORTIONAL_TO_PMAX:
                applyProportionalToPmaxVariation(network, subReportNode, identifiableAttributes, variation);
                break;
            case REGULAR_DISTRIBUTION:
                applyRegularDistributionVariation(network, subReportNode, identifiableAttributes, variation);
                break;
            case VENTILATION:
                applyVentilationVariation(network, subReportNode, identifiableAttributes, variation, getDistributionKeys(identifiableAttributes, subReportNode));
                break;
            case STACKING_UP:
                applyStackingUpVariation(network, subReportNode, identifiableAttributes, variation);
                break;
            default:
                throw new NetworkModificationRunException(String.format("This variation mode is not supported : %s", variation.getVariationMode().name()));
        }
    }

    private Double getDistributionKeys(Set<IdentifiableAttributes> identifiableAttributes, ReportNode subReportNode) {
        var distributionKeys = identifiableAttributes.stream()
                .filter(equipment -> equipment.getDistributionKey() != null)
                .mapToDouble(IdentifiableAttributes::getDistributionKey)
                .sum();
        if (distributionKeys == 0) {
            createReport(subReportNode, "network.modification.distributionKeysNotFound", Map.of(), TypedValue.WARN_SEVERITY);
            return null;
        }
        return distributionKeys;
    }

    protected abstract void applyStackingUpVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos);

    protected abstract void applyVentilationVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos, Double distributionKeys);

    protected abstract void applyRegularDistributionVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos);

    protected abstract void applyProportionalToPmaxVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos);

    protected abstract void applyProportionalVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos);

    protected abstract double getAsked(ScalingVariationInfos variationInfos, AtomicReference<Double> sum);

    protected abstract Scalable getScalable(String id);

}
