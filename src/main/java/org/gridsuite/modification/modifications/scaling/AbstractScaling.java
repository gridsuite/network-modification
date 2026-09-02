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
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import lombok.*;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.error.NetworkModificationExceptionType;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.data.ScalingVariationData;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Setter
@Getter
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public abstract class AbstractScaling extends AbstractModification {

    private static final String REPORT_KEY_PREPARING_SCALING_VARIATIONS = "network.modification.scaling.preparingScalingVariations";
    private static final String REPORT_KEY_PREPARING_SCALING_VARIATION = "network.modification.scaling.preparingScalingVariation";
    private static final String REPORT_KEY_FILTER_DUPLICATED_EQUIPMENT = "network.modification.filterEvaluation.equipmentAlreadySeen";
    private static final String REPORT_KEY_SCALING_APPLIED = "network.modification.scaling.scalingApplied";
    private static final String REPORT_KEY_FILTER_EVALUATION = "network.modification.scaling.filterEvaluation";
    private static final String REPORT_KEY_FILTER_EVALUATION_RESULT = "network.modification.scaling.filterEvaluationResult";
    private static final String REPORT_KEY_NO_EQUIPMENT_TO_SCALE = "network.modification.scaling.noEquipmentToScale";
    private static final String REPORT_KEY_EQUIPMENTS_TO_SCALE = "network.modification.scaling.equipmentsToScale";
    private static final String REPORT_KEY_DISTRIBUTION_KEYS_NOT_FOUND = "network.modification.distributionKeysNotFound";

    protected List<ScalingVariationData> scalingVariations;
    protected VariationType variationType;
    protected NetworkModificationExceptionType exceptionType;

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ReportNode subReporter = subReportNode.newReportNode()
                .withMessageTemplate(REPORT_KEY_PREPARING_SCALING_VARIATIONS)
                .withUntypedValue("scalingVariationsCount", scalingVariations.size())
                .withUntypedValue("equipmentType", getEquipmentType())
                .add();
        for (ScalingVariationData scalingVariation : scalingVariations) {
            ReportNode scalingVariationContainer = subReporter.newReportNode()
                    .withMessageTemplate(REPORT_KEY_PREPARING_SCALING_VARIATION)
                    .withUntypedValue("scalingVariationIndex", scalingVariations.indexOf(scalingVariation) + 1)
                    .withUntypedValue("scalingVariationsCount", scalingVariations.size())
                    .withUntypedValue("scalingVariationType", scalingVariation.getVariationMode().name())
                    .add();
            List<Identifiable<?>> equipments = evaluateFilters(network, scalingVariation, scalingVariationContainer);

            if (CollectionUtils.isEmpty(equipments)) {
                scalingVariationContainer.newReportNode()
                        .withMessageTemplate(REPORT_KEY_NO_EQUIPMENT_TO_SCALE)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else {
                scalingVariationContainer.newReportNode()
                        .withMessageTemplate(REPORT_KEY_EQUIPMENTS_TO_SCALE)
                        .withUntypedValue("equipmentCount", equipments.size())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
                applyVariation(network, scalingVariationContainer, equipments, scalingVariation);
            }
        }
    }

    protected void scale(Network network, ReportNode subReportNode, ScalingVariationData scalingVariation, AtomicReference<Double> sum, Scalable scalable, ScalingParameters scalingParameters) {
        double asked = getAsked(scalingVariation, sum);
        double done = scalable.scale(network, asked, scalingParameters);
        subReportNode.newReportNode()
                .withMessageTemplate(REPORT_KEY_SCALING_APPLIED)
                .withUntypedValue("variationMode", scalingVariation.getVariationMode().name())
                .withUntypedValue("askedValue", asked)
                .withUntypedValue("actualValue", done)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    protected abstract void applyStackingUpVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariations);

    protected abstract void applyRegularDistributionVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation);

    protected abstract void applyProportionalToPmaxVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation);

    protected abstract void applyProportionalVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation);

    protected abstract void applyVentilationVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation, Double distributionKeysSum);

    protected abstract String getEquipmentType();

    private List<Identifiable<?>> evaluateFilters(Network network, ScalingVariationData scalingVariation, ReportNode scalingVariationContainer) {
        Set<String> alreadySeenEquipments = new HashSet<>();
        List<Identifiable<?>> equipments = new ArrayList<>();
        for (int i = 0; i < scalingVariation.getFilters().size(); i++) {
            Filter filter = scalingVariation.getFilters().get(i);
            ReportNode filterReportNode = scalingVariationContainer.newReportNode()
                    .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION)
                    .withUntypedValue("filterCount", i)
                    .add();

            List<Identifiable<?>> filteredEquipments = filter.evaluate(network, filterReportNode);
            for (Identifiable<?> equipment : filteredEquipments) {
                if (alreadySeenEquipments.add(equipment.getId())) {
                    equipments.add(equipment);
                } else {
                    scalingVariationContainer.newReportNode()
                            .withMessageTemplate(REPORT_KEY_FILTER_DUPLICATED_EQUIPMENT)
                            .withUntypedValue("equipmentId", equipment.getId())
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .add();
                }
            }
        }
        scalingVariationContainer.newReportNode()
                .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION_RESULT)
                .withUntypedValue("equipmentCount", equipments.size())
                .add();
        return equipments;
    }

    private void applyVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation) {
        switch (scalingVariation.getVariationMode()) {
            case PROPORTIONAL -> applyProportionalVariation(network, subReportNode, equipments, scalingVariation);
            case PROPORTIONAL_TO_PMAX -> applyProportionalToPmaxVariation(network, subReportNode, equipments, scalingVariation);
            case REGULAR_DISTRIBUTION -> applyRegularDistributionVariation(network, subReportNode, equipments, scalingVariation);
            case VENTILATION -> applyVentilationVariation(network, subReportNode, equipments, scalingVariation, getDistributionKeysSum(scalingVariation, subReportNode));
            case STACKING_UP -> applyStackingUpVariation(network, subReportNode, equipments, scalingVariation);
            default -> throw new NetworkModificationException(exceptionType, String.format("This variation mode is not supported : %s", scalingVariation.getVariationMode().name()));
        }
    }

    private Double getDistributionKeysSum(ScalingVariationData scalingVariation, ReportNode subReportNode) {
        double distributionKeysSum = scalingVariation.getDistributionKeysPerEquipmentId().values().stream()
                .filter(Objects::nonNull)
                .mapToDouble(Double::doubleValue)
                .sum();

        if (distributionKeysSum == 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_DISTRIBUTION_KEYS_NOT_FOUND)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .add();
            return null;
        }
        return distributionKeysSum;
    }

    private double getAsked(ScalingVariationData scalingVariation, AtomicReference<Double> sum) {
        return VariationType.DELTA_P.equals(variationType)
                ? scalingVariation.getVariationValue()
                : scalingVariation.getVariationValue() - sum.get();
    }
}
