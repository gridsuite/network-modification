/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.modifications.scaling;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import lombok.*;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.error.NetworkModificationExceptionType;
import org.gridsuite.modification.modifications.data.ScalingVariationData;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class LoadScaling extends AbstractScaling {

    @Builder
    public LoadScaling(List<ScalingVariationData> scalingVariations, VariationType variationType) {
        super(scalingVariations, variationType, NetworkModificationExceptionType.LOAD_SCALING_ERROR);
    }

    @Override
    public String getName() {
        return ModificationType.LOAD_SCALING.name();
    }

    @Override
    protected void applyStackingUpVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariations) {
        // no implementation for load scaling
        throw new NetworkModificationException(exceptionType, String.format("This variation mode is not supported : %s", scalingVariations.getVariationMode().name()));
    }

    @Override
    protected void applyProportionalToPmaxVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation) {
        // no implementation for load scaling
        throw new NetworkModificationException(exceptionType, String.format("This variation mode is not supported : %s", scalingVariation.getVariationMode().name()));
    }

    @Override
    protected void applyRegularDistributionVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation) {
        List<Load> loads = equipments.stream()
                .map(Load.class::cast)
                .filter(ModificationUtils::isInjectionConnected)
                .toList();

        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Scalable> scalables = loads.stream()
                .map(load -> {
                    sum.set(sum.get() + load.getP0());
                    return getScalable(load.getId());
                }).toList();

        List<Double> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), 100.0 / scalables.size()));
        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        scale(network, subReportNode, scalingVariation, sum, regularDistributionScalable, provideScalingParameters(scalingVariation.getReactiveVariationMode()));

    }

    @Override
    protected void applyProportionalVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation) {
        List<Load> loads = equipments.stream()
                .map(Load.class::cast)
                .filter(ModificationUtils::isInjectionConnected)
                .toList();
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Double> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        loads.forEach(load -> {
            targetPMap.put(load.getId(), load.getP0());
            sum.set(sum.get() + load.getP0());
        });
        targetPMap.forEach((id, p) -> {
            percentages.add((p / sum.get()) * 100);
            scalables.add(getScalable(id));
        });

        Scalable proportionalScalable = Scalable.proportional(percentages, scalables);
        scale(network, subReportNode, scalingVariation, sum, proportionalScalable, provideScalingParameters(scalingVariation.getReactiveVariationMode()));
    }

    @Override
    protected void applyVentilationVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation, Double distributionKeysSum) {
        if (distributionKeysSum != null) {
            AtomicReference<Double> sum = new AtomicReference<>(0D);
            List<Double> percentages = new ArrayList<>();
            List<Scalable> scalables = new ArrayList<>();

            equipments.forEach(equipment -> {
                Load load = (Load) equipment;
                Double distributionKey = scalingVariation.getDistributionKeysPerEquipmentId().get(equipment.getId());
                if (ModificationUtils.isInjectionConnected(load)) {
                    sum.set(load.getP0() + sum.get());
                    scalables.add(getScalable(equipment.getId()));
                    percentages.add((distributionKey / distributionKeysSum) * 100);
                }
            });
            Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
            scale(network, subReportNode, scalingVariation, sum, ventilationScalable, provideScalingParameters(scalingVariation.getReactiveVariationMode()));
        }
    }

    @Override
    protected String getEquipmentType() {
        return "load";
    }

    private Scalable getScalable(String id) {
        return Scalable.onLoad(id, -Double.MAX_VALUE, Double.MAX_VALUE);
    }

    private ScalingParameters provideScalingParameters(ReactiveVariationMode reactiveVariationMode) {
        return switch (reactiveVariationMode) {
            case CONSTANT_Q -> new ScalingParameters().setScalingConvention(Scalable.ScalingConvention.LOAD);
            case TAN_PHI_FIXED ->
                new ScalingParameters().setScalingConvention(Scalable.ScalingConvention.LOAD).setConstantPowerFactor(true);
        };
    }
}
