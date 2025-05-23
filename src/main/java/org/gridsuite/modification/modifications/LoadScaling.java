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
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.LoadScalingInfos;
import org.gridsuite.modification.dto.ScalingVariationInfos;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class LoadScaling extends AbstractScaling {

    public LoadScaling(LoadScalingInfos loadScalableInfos) {
        super(loadScalableInfos);
    }

    @Override
    protected void applyVentilationVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos, Double distributionKeys) {
        if (distributionKeys != null) {
            AtomicReference<Double> sum = new AtomicReference<>(0D);
            List<Double> percentages = new ArrayList<>();
            List<Scalable> scalables = new ArrayList<>();

            identifiableAttributes.forEach(equipment -> {
                Load load = network.getLoad(equipment.getId());
                if (ModificationUtils.isInjectionConnected(load)) {
                    sum.set(load.getP0() + sum.get());
                    scalables.add(getScalable(equipment.getId()));
                    percentages.add((equipment.getDistributionKey() / distributionKeys) * 100);
                }
            });
            Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
            var asked = getAsked(scalingVariationInfos, sum);
            var done = scale(network, scalingVariationInfos, asked, ventilationScalable);
            reportScaling(subReportNode, scalingVariationInfos.getVariationMode(), asked, done);
        }
    }

    @Override
    protected void applyRegularDistributionVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        List<Load> loads = identifiableAttributes
                .stream()
                .map(attribute -> network.getLoad(attribute.getId()))
                .filter(ModificationUtils::isInjectionConnected)
                .toList();

        AtomicReference<Double> sum = new AtomicReference<>(0D);

        List<Scalable> scalables = loads.stream()
                .map(load -> {
                    sum.set(sum.get() + load.getP0());
                    return getScalable(load.getId());
                }).collect(Collectors.toList());

        List<Double> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), 100.0 / scalables.size()));
        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        var asked = getAsked(scalingVariationInfos, sum);
        var done = scale(network, scalingVariationInfos, asked, regularDistributionScalable);
        reportScaling(subReportNode, scalingVariationInfos.getVariationMode(), asked, done);
    }

    @Override
    protected void applyProportionalVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        List<Load> loads = identifiableAttributes.stream()
                .map(attribute -> network.getLoad(attribute.getId()))
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
        var asked = getAsked(scalingVariationInfos, sum);
        var done = scale(network, scalingVariationInfos, asked, proportionalScalable);
        reportScaling(subReportNode, scalingVariationInfos.getVariationMode(), asked, done);
    }

    @Override
    protected void applyProportionalToPmaxVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        // no implementation for load scaling
        throw new NetworkModificationException(scalingInfos.getErrorType(), String.format("This variation mode is not supported : %s", scalingVariationInfos.getVariationMode().name()));
    }

    @Override
    protected void applyStackingUpVariation(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        // no implementation for load scaling
        throw new NetworkModificationException(scalingInfos.getErrorType(), String.format("This variation mode is not supported : %s", scalingVariationInfos.getVariationMode().name()));
    }

    private double scale(Network network, ScalingVariationInfos scalingVariationInfos, double asked, Scalable proportionalScalable) {
        return switch (scalingVariationInfos.getReactiveVariationMode()) {
            case CONSTANT_Q ->
                    proportionalScalable.scale(network, asked, new ScalingParameters().setScalingConvention(Scalable.ScalingConvention.LOAD));
            case TAN_PHI_FIXED ->
                    proportionalScalable.scale(network, asked, new ScalingParameters().setScalingConvention(Scalable.ScalingConvention.LOAD).setConstantPowerFactor(true));
        };
    }

    @Override
    public double getAsked(ScalingVariationInfos scalingVariationInfos, AtomicReference<Double> sum) {
        return scalingInfos.getVariationType() == VariationType.DELTA_P
                ? scalingVariationInfos.getVariationValue()
                : scalingVariationInfos.getVariationValue() - sum.get();
    }

    @Override
    protected Scalable getScalable(String id) {
        return Scalable.onLoad(id, -Double.MAX_VALUE, Double.MAX_VALUE);
    }

    @Override
    public String getName() {
        return "LoadScaling";
    }

    private void reportScaling(ReportNode subReportNode, VariationMode variationMode, double askedValue, double actualValue) {
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.scalingApplied")
                .withUntypedValue("variationMode", variationMode.name())
                .withUntypedValue("askedValue", askedValue)
                .withUntypedValue("actualValue", actualValue)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }
}
