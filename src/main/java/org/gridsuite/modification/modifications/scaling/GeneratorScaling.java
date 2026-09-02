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
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import lombok.*;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.modifications.data.ScalingVariationData;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;

import static com.powsybl.iidm.modification.scalable.ScalingParameters.Priority.RESPECT_OF_VOLUME_ASKED;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.GENERATOR_SCALING_ERROR;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GeneratorScaling extends AbstractScaling {

    @Builder
    public GeneratorScaling(List<ScalingVariationData> scalingVariations, VariationType variationType) {
        super(scalingVariations, variationType, GENERATOR_SCALING_ERROR);
    }

    @Override
    public String getName() {
        return ModificationType.GENERATOR_SCALING.name();
    }

    @Override
    protected void applyStackingUpVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariations) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Scalable stackingUpScalable = Scalable.stack(equipments.stream()
                .map(Generator.class::cast)
                .filter(ModificationUtils::isInjectionConnected)
                .map(g -> {
                    sum.set(g.getTargetP() + sum.get());
                    return getScalable(g.getId());
                }).toArray(Scalable[]::new));
        scale(network, subReportNode, scalingVariations, sum, stackingUpScalable, new ScalingParameters());
    }

    @Override
    protected void applyRegularDistributionVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation) {
        List<Generator> generators = equipments.stream()
                .map(Generator.class::cast)
                .filter(ModificationUtils::isInjectionConnected)
                .toList();

        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Scalable> scalables = generators.stream()
                .map(generator -> {
                    sum.set(sum.get() + generator.getTargetP());
                    return getScalable(generator.getId());
                }).toList();

        List<Double> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), 100.0 / scalables.size()));
        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        scale(network, subReportNode, scalingVariation, sum, regularDistributionScalable, new ScalingParameters().setPriority(RESPECT_OF_VOLUME_ASKED));
    }

    @Override
    protected void applyProportionalToPmaxVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation) {
        AtomicReference<Double> maxPSum = new AtomicReference<>(0D);
        AtomicReference<Double> targetPSum = new AtomicReference<>(0D);
        List<Generator> generators = equipments.stream()
                .map(Generator.class::cast)
                .filter(ModificationUtils::isInjectionConnected)
                .toList();
        Map<String, Double> maxPMap = new HashMap<>();
        List<Double> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();

        // we retrieve max P and the sum of max P of each generator to calculate the percentage.
        // we calculate the sum of target P to calculate variation value if variation type is Target_P
        generators.forEach(generator -> {
            maxPMap.put(generator.getId(), generator.getMaxP());
            maxPSum.set(maxPSum.get() + generator.getMaxP());
            targetPSum.set(targetPSum.get() + generator.getTargetP());
        });

        setScalablePercentage(maxPSum, maxPMap, percentages, scalables);
        Scalable proportionalToPmaxScalable = Scalable.proportional(percentages, scalables);
        scale(network, subReportNode, scalingVariation, targetPSum, proportionalToPmaxScalable, new ScalingParameters().setPriority(RESPECT_OF_VOLUME_ASKED));

    }

    @Override
    protected void applyProportionalVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Generator> generators = equipments.stream()
                .map(Generator.class::cast)
                .filter(ModificationUtils::isInjectionConnected)
                .toList();
        List<Double> percentages = new ArrayList<>();
        Map<String, Double> targetPMap = new HashMap<>();
        List<Scalable> scalables = new ArrayList<>();

        // we retrieve the target P for every generator and calculate their sum
        generators.forEach(generator -> {
            targetPMap.put(generator.getId(), generator.getTargetP());
            sum.set(sum.get() + generator.getTargetP());
        });

        // we calculate percentage of each target P value relative to the sum of target P
        setScalablePercentage(sum, targetPMap, percentages, scalables);
        Scalable proportionalScalable = Scalable.proportional(percentages, scalables);
        scale(network, subReportNode, scalingVariation, sum, proportionalScalable, new ScalingParameters().setPriority(RESPECT_OF_VOLUME_ASKED));

    }

    @Override
    protected void applyVentilationVariation(Network network, ReportNode subReportNode, List<Identifiable<?>> equipments, ScalingVariationData scalingVariation, Double distributionKeysSum) {
        if (distributionKeysSum != null) {
            AtomicReference<Double> sum = new AtomicReference<>(0D);
            List<Double> percentages = new ArrayList<>();
            List<Scalable> scalables = new ArrayList<>();

            equipments.forEach(equipment -> {
                Generator generator = (Generator) equipment;
                Double distributionKey = scalingVariation.getDistributionKeysPerEquipmentId().get(equipment.getId());
                if (ModificationUtils.isInjectionConnected(generator)) {
                    sum.set(generator.getTargetP() + sum.get());
                    scalables.add(getScalable(equipment.getId()));
                    percentages.add((distributionKey / distributionKeysSum) * 100);
                }
            });
            Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
            scale(network, subReportNode, scalingVariation, sum, ventilationScalable, new ScalingParameters().setPriority(RESPECT_OF_VOLUME_ASKED));
        }
    }

    @Override
    protected String getEquipmentType() {
        return "generator";
    }

    private Scalable getScalable(String id) {
        return Scalable.onGenerator(id);
    }

    private void setScalablePercentage(AtomicReference<Double> sum,
                                       Map<String, Double> targetPMap,
                                       List<Double> percentages,
                                       List<Scalable> scalables) {
        targetPMap.forEach((id, p) -> {
            percentages.add((p / sum.get()) * 100);
            scalables.add(getScalable(id));
        });
    }
}
