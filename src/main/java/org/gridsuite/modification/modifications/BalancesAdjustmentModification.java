/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.balances_adjustment.balance_computation.*;
import com.powsybl.balances_adjustment.util.CountryAreaFactory;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.computation.ComputationManager;
import com.powsybl.iidm.modification.scalable.ProportionalScalable;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.*;
import com.powsybl.loadflow.LoadFlow;
import com.powsybl.openloadflow.OpenLoadFlowParameters;
import org.gridsuite.modification.dto.ShiftEquipmentType;
import org.gridsuite.modification.dto.ShiftType;
import org.gridsuite.modification.dto.BalancesAdjustmentAreaInfos;
import org.gridsuite.modification.dto.BalancesAdjustmentModificationInfos;

import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public class BalancesAdjustmentModification extends AbstractModification {
    private final BalancesAdjustmentModificationInfos balancesAdjustmentModificationInfos;

    public BalancesAdjustmentModification(BalancesAdjustmentModificationInfos balancesAdjustmentModificationInfos) {
        this.balancesAdjustmentModificationInfos = balancesAdjustmentModificationInfos;
    }

    @Override
    public String getName() {
        return "BalancesAdjustmentModification";
    }

    @Override
    public void apply(Network network,
                      NamingStrategy namingStrategy,
                      boolean throwException,
                      ComputationManager computationManager,
                      ReportNode reportNode) {
        final boolean isStatic = !balancesAdjustmentModificationInfos.isWithLoadFlow();

        List<BalanceComputationArea> areas = balancesAdjustmentModificationInfos
            .getAreas()
            .stream()
            .map(areaInfos ->
                new BalanceComputationArea(
                    areaInfos.getName(),
                    new CountryAreaFactory(isStatic, areaInfos.getCountries().toArray(Country[]::new)),
                    createScalable(areaInfos, network, reportNode),
                    areaInfos.getNetPosition()
                )
            )
            .toList();

        BalanceComputation balanceComputation = new BalanceComputationFactoryImpl(isStatic)
            .create(areas, LoadFlow.find(), computationManager);

        BalanceComputationParameters parameters = BalanceComputationParameters.load();
        parameters.setMaxNumberIterations(balancesAdjustmentModificationInfos.getMaxNumberIterations());
        parameters.setThresholdNetPosition(balancesAdjustmentModificationInfos.getThresholdNetPosition());
        parameters.setMismatchMode(BalanceComputationParameters.MismatchMode.MAX);
        parameters.setSubtractLoadFlowBalancing(balancesAdjustmentModificationInfos.isSubtractLoadFlowBalancing());
        parameters.getScalingParameters().setPriority(ScalingParameters.Priority.RESPECT_OF_VOLUME_ASKED);
        parameters.getLoadFlowParameters().setCountriesToBalance(new HashSet<>(balancesAdjustmentModificationInfos.getCountriesToBalance()));
        parameters.getLoadFlowParameters().setBalanceType(balancesAdjustmentModificationInfos.getBalanceType());
        parameters.getLoadFlowParameters().getExtension(OpenLoadFlowParameters.class).setSlackDistributionFailureBehavior(OpenLoadFlowParameters.SlackDistributionFailureBehavior.FAIL);

        balanceComputation
            .run(network, network.getVariantManager().getWorkingVariantId(), parameters, reportNode)
            .join();
    }

    private Scalable createScalable(BalancesAdjustmentAreaInfos balancesAdjustmentAreaInfos, Network network, ReportNode reportNode) {
        return createScalable(
            balancesAdjustmentAreaInfos.getShiftEquipmentType(),
            balancesAdjustmentAreaInfos.getShiftType(),
            balancesAdjustmentAreaInfos.getCountries(),
            network,
            reportNode
        );
    }

    private Scalable createScalable(ShiftEquipmentType shiftEquipmentType, ShiftType shiftType, List<Country> countries, Network network, ReportNode reportNode) {
        return switch (shiftEquipmentType) {
            case GENERATOR -> switch (shiftType) {
                case PROPORTIONAL ->
                    Scalable.proportional(getCountriesGenerators(network, countries, reportNode), ProportionalScalable.DistributionMode.PROPORTIONAL_TO_PMAX);
                case BALANCED ->
                    Scalable.proportional(getCountriesGenerators(network, countries, reportNode), ProportionalScalable.DistributionMode.UNIFORM_DISTRIBUTION);
            };
            case LOAD -> switch (shiftType) {
                case PROPORTIONAL ->
                    Scalable.proportional(getCountriesLoads(network, countries, reportNode), ProportionalScalable.DistributionMode.PROPORTIONAL_TO_P0);
                case BALANCED ->
                    Scalable.proportional(getCountriesLoads(network, countries, reportNode), ProportionalScalable.DistributionMode.UNIFORM_DISTRIBUTION);
            };
        };
    }

    private List<Generator> getCountriesGenerators(Network network, List<Country> countries, ReportNode reportNode) {
        var generators = countries.stream().flatMap(country -> getCountryGenerators(network, country)).toList();
        reportNode.newReportNode().withMessageTemplate("network.modification.balancesAdjustment.addingGenerators")
            .withUntypedValue("count", generators.size())
            .withUntypedValue("countries", countries.stream().map(Enum::name).collect(Collectors.joining(",")))
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
        return generators;
    }

    private List<Load> getCountriesLoads(Network network, List<Country> countries, ReportNode reportNode) {
        var loads = countries.stream().flatMap(country -> getCountryLoads(network, country)).toList();
        reportNode.newReportNode().withMessageTemplate("network.modification.balancesAdjustment.addingLoads")
            .withUntypedValue("count", loads.size())
            .withUntypedValue("countries", countries.stream().map(Enum::name).collect(Collectors.joining(",")))
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
        return loads;
    }

    private Stream<Generator> getCountryGenerators(Network network, Country country) {
        return network.getGeneratorStream()
            .filter(generator -> generator.getTerminal().getVoltageLevel().getSubstation().flatMap(Substation::getCountry).filter(c -> c == country).isPresent());
    }

    private Stream<Load> getCountryLoads(Network network, Country country) {
        return network.getLoadStream()
            .filter(load -> load.getTerminal().getVoltageLevel().getSubstation().flatMap(Substation::getCountry).filter(c -> c == country).isPresent());
    }
}
