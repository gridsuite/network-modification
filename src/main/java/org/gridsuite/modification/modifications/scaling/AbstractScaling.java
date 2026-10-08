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
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import lombok.*;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.error.NetworkModificationExceptionType;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.data.scaling.ScalingVariationData;
import org.gridsuite.modification.modifications.data.scaling.VariationFilterData;

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

    private static final String REPORT_KEY_PREPARING_SCALING_VARIATION = "network.modification.scaling.preparingScalingVariation";
    private static final String REPORT_KEY_FILTER_DUPLICATED_EQUIPMENT = "network.modification.filterEvaluation.equipmentAlreadySeen";
    private static final String REPORT_KEY_SCALING_APPLIED = "network.modification.scaling.scalingApplied";
    private static final String REPORT_KEY_FILTER_EVALUATION = "network.modification.filterEvaluation";
    private static final String REPORT_KEY_FILTER_EVALUATION_RESULT = "network.modification.filterEvaluationResult";
    private static final String REPORT_KEY_FILTER_EVALUATION_WITH_NO_RESULT = "network.modification.filterEvaluationResult.noResult";
    private static final String REPORT_KEY_APPLY_ASSIGNMENT = "network.modification.applyAssignment";
    private static final String VALUE_KEY_ACTUAL_VALUE = "actualValue";
    private static final String VALUE_KEY_ASKED_VALUE = "askedValue";
    private static final String VALUE_KEY_EQUIPMENT_COUNT = "equipmentCount";
    private static final String VALUE_KEY_EQUIPMENT_ID = "equipmentId";
    public static final String REPORT_KEY_FILTERS_EVALUATION = "network.modification.filtersEvaluation";
    private static final String VALUE_KEY_FILTER_IDENTIFIER = "filterIdentifier";
    private static final String VALUE_KEY_SCALING_VARIATIONS_COUNT = "scalingVariationsCount";
    private static final String VALUE_KEY_SCALING_VARIATION_INDEX = "scalingVariationIndex";
    private static final String VALUE_KEY_SCALING_VARIATION_VALUE = "scalingVariationValue";
    private static final String VALUE_KEY_SCALING_VARIATION_TYPE = "scalingVariationType";
    private static final String VALUE_KEY_VARIATION_MODE = "variationMode";

    protected static final String UNSUPPORTED_VARIATION_MODE_TEMPLATE = "This variation mode is not supported : %s";

    protected List<ScalingVariationData> scalingVariations;
    protected VariationType variationType;
    protected NetworkModificationExceptionType exceptionType;

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        for (int i = 0; i < scalingVariations.size(); i++) {
            ScalingVariationData scalingVariation = scalingVariations.get(i);
            ReportNode scalingVariationContainer = subReportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_PREPARING_SCALING_VARIATION)
                    .withUntypedValue(VALUE_KEY_SCALING_VARIATION_INDEX, i + 1)
                    .withUntypedValue(VALUE_KEY_SCALING_VARIATIONS_COUNT, scalingVariations.size())
                    .withUntypedValue(VALUE_KEY_SCALING_VARIATION_VALUE, scalingVariation.getVariationValue())
                    .withUntypedValue(VALUE_KEY_SCALING_VARIATION_TYPE, scalingVariation.getVariationMode().name())
                    .add();
            List<Identifiable<?>> equipments = evaluateFilters(network, scalingVariation, scalingVariationContainer);

            if (CollectionUtils.isEmpty(equipments)) {
                scalingVariationContainer.newReportNode()
                        .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION_WITH_NO_RESULT)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else {
                scalingVariationContainer.newReportNode()
                        .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION_RESULT)
                        .withUntypedValue(VALUE_KEY_EQUIPMENT_COUNT, equipments.size())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
                ReportNode applyAssignmentContainer = scalingVariationContainer.newReportNode()
                        .withMessageTemplate(REPORT_KEY_APPLY_ASSIGNMENT)
                        .add();
                applyVariation(network, applyAssignmentContainer, equipments, scalingVariation);
            }
        }
    }

    protected void scale(Network network, ReportNode subReportNode, ScalingVariationData scalingVariation, AtomicReference<Double> sum, Scalable scalable, ScalingParameters scalingParameters) {
        double asked = getAsked(scalingVariation, sum);
        double done = scalable.scale(network, asked, scalingParameters);
        subReportNode.newReportNode()
                .withMessageTemplate(REPORT_KEY_SCALING_APPLIED)
                .withUntypedValue(VALUE_KEY_VARIATION_MODE, scalingVariation.getVariationMode().name())
                .withUntypedValue(VALUE_KEY_ASKED_VALUE, asked)
                .withUntypedValue(VALUE_KEY_ACTUAL_VALUE, done)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    protected abstract void applyStackingUpVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariations);

    protected abstract void applyRegularDistributionVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation);

    protected abstract void applyProportionalToPmaxVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation);

    protected abstract void applyProportionalVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation);

    /**
     * @param distributionKeys the keys of the selected equipments and their total, or {@code null} if they
     *                         cannot be used, in which case the reason has already been reported
     */
    protected abstract void applyVentilationVariation(Network network, ReportNode subReportNode,
                                                      List<Identifiable<?>> equipments,
                                                      ScalingVariationData scalingVariation,
                                                      DistributionKeys distributionKeys);

    protected abstract IdentifiableType getEquipmentType();

    // TODO Today, a filter that could not be resolved is skipped without notifying the user, and the
    //  modification continues with a subset of the equipments its filters were meant to select.
    //  Agreed fix: add an error node to the report indicating the number of unresolved filters and, if
    //  possible, their names/location
    private List<Identifiable<?>> evaluateFilters(Network network, ScalingVariationData scalingVariation, ReportNode scalingVariationContainer) {
        Set<String> alreadySeenEquipments = new HashSet<>();
        List<Identifiable<?>> equipments = new ArrayList<>();
        ReportNode filtersContainer = scalingVariationContainer.newReportNode()
                .withMessageTemplate(REPORT_KEY_FILTERS_EVALUATION)
                .add();
        List<VariationFilterData> filters = scalingVariation.getFilters();
        for (int i = 0; i < filters.size(); i++) {
            Filter filter = filters.get(i).filter();
            if (filter == null) {
                // unresolved filter
                continue;
            }
            String filterIdentifier = filter.getName() == null ? Integer.toString(i + 1) : filter.getName();
            ReportNode filterReportNode = filtersContainer.newReportNode()
                    .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION)
                    .withUntypedValue(VALUE_KEY_FILTER_IDENTIFIER, filterIdentifier)
                    .add();

            List<Identifiable<?>> filteredEquipments = filter.evaluate(network, filterReportNode);
            for (Identifiable<?> equipment : filteredEquipments) {
                if (alreadySeenEquipments.add(equipment.getId())) {
                    equipments.add(equipment);
                } else {
                    filterReportNode.newReportNode()
                            .withMessageTemplate(REPORT_KEY_FILTER_DUPLICATED_EQUIPMENT)
                            .withUntypedValue(VALUE_KEY_EQUIPMENT_ID, equipment.getId())
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .add();
                }
            }
        }
        return equipments;
    }

    private void applyVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation) {
        switch (scalingVariation.getVariationMode()) {
            case PROPORTIONAL -> applyProportionalVariation(network, subReportNode, equipments, scalingVariation);
            case PROPORTIONAL_TO_PMAX -> applyProportionalToPmaxVariation(network, subReportNode, equipments, scalingVariation);
            case REGULAR_DISTRIBUTION -> applyRegularDistributionVariation(network, subReportNode, equipments, scalingVariation);
            case VENTILATION -> applyVentilationVariation(network, subReportNode, equipments, scalingVariation,
                    DistributionKeys.resolve(scalingVariation.getFilters(), equipmentIdsOf(equipments), subReportNode));
            case STACKING_UP -> applyStackingUpVariation(network, subReportNode, equipments, scalingVariation);
            default -> throw new NetworkModificationException(exceptionType, String.format(UNSUPPORTED_VARIATION_MODE_TEMPLATE, scalingVariation.getVariationMode().name()));
        }
    }

    private static List<String> equipmentIdsOf(List<Identifiable<?>> equipments) {
        return equipments.stream().map(Identifiable::getId).toList();
    }

    private double getAsked(ScalingVariationData scalingVariation, AtomicReference<Double> sum) {
        return VariationType.DELTA_P.equals(variationType)
                ? scalingVariation.getVariationValue()
                : scalingVariation.getVariationValue() - sum.get();
    }
}
