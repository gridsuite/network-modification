/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.balances_adjustment.balance_computation.*;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.computation.local.LocalComputationManager;
import com.powsybl.iidm.modification.scalable.ProportionalScalable;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.*;
import com.powsybl.loadflow.LoadFlow;
import com.powsybl.loadflow.LoadFlowParameters;
import com.powsybl.networkarea.CountryAreaFactory;
import com.powsybl.openloadflow.OpenLoadFlowParameters;
import lombok.*;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.utils.LoadFlowParametersUtils.mapLoadFlowPrameters;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class BalancesAdjustmentModification extends AbstractModification {
    private static final Logger LOGGER = LoggerFactory.getLogger(BalancesAdjustmentModification.class);
    private static final String OPEN_LOAD_FLOW_PROVIDER = "OpenLoadFlow";

    private List<BalancesAdjustmentAreaInfos> areas;
    private int maxNumberIterations;
    private double thresholdNetPosition;
    private List<Country> countriesToBalance;
    private LoadFlowParameters.BalanceType balanceType;
    private boolean withLoadFlow;
    /** Load flow parameters resolved when the modification was built, {@code null} to use the default ones. */
    private LoadFlowParametersInfos loadFlowParameters;
    /** Why the default load flow parameters are used, reported when {@link #loadFlowParameters} is {@code null}. */
    private String defaultLoadFlowParametersReason;
    private boolean withRatioTapChangers;
    private boolean subtractLoadFlowBalancing;

    @Builder
    public BalancesAdjustmentModification(List<BalancesAdjustmentAreaInfos> areas,
                                          int maxNumberIterations,
                                          double thresholdNetPosition,
                                          List<Country> countriesToBalance,
                                          LoadFlowParameters.BalanceType balanceType,
                                          boolean withLoadFlow,
                                          LoadFlowParametersInfos loadFlowParameters,
                                          String defaultLoadFlowParametersReason,
                                          boolean withRatioTapChangers,
                                          boolean subtractLoadFlowBalancing) {
        this.areas = areas;
        this.maxNumberIterations = maxNumberIterations;
        this.thresholdNetPosition = thresholdNetPosition;
        this.countriesToBalance = countriesToBalance;
        this.balanceType = balanceType;
        this.withLoadFlow = withLoadFlow;
        this.loadFlowParameters = loadFlowParameters;
        this.defaultLoadFlowParametersReason = defaultLoadFlowParametersReason;
        this.withRatioTapChangers = withRatioTapChangers;
        this.subtractLoadFlowBalancing = subtractLoadFlowBalancing;
    }

    @Override
    public String getName() {
        return ModificationType.BALANCES_ADJUSTMENT_MODIFICATION.name();
    }

    private BalanceComputationParameters createBalanceComputationParameters(ReportNode reportNode) {
        BalanceComputationParameters parameters = BalanceComputationParameters.load();
        parameters.getScalingParameters().setPriority(ScalingParameters.Priority.RESPECT_OF_VOLUME_ASKED);
        parameters.setWithLoadFlow(withLoadFlow);

        if (!withLoadFlow) {
            return parameters;
        }

        if (loadFlowParameters == null) {
            reportUsingDefaultParameters(reportNode, defaultLoadFlowParametersReason);
        } else if (OPEN_LOAD_FLOW_PROVIDER.equals(loadFlowParameters.getProvider())) {
            parameters.setLoadFlowParameters(mapLoadFlowPrameters(loadFlowParameters));
        }

        overrideBalanceComputationParameters(parameters);
        return parameters;
    }

    private void reportUsingDefaultParameters(ReportNode reportNode, String reason) {
        reportNode.newReportNode()
                .withMessageTemplate("network.modification.balancesAdjustment.usingDefaultLoadFlowParameters")
                .withUntypedValue("reason", reason)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        LOGGER.info("Using default load flow parameters: {}", reason);
    }

    private void overrideBalanceComputationParameters(BalanceComputationParameters parameters) {
        parameters.setMaxNumberIterations(maxNumberIterations);
        parameters.setThresholdNetPosition(thresholdNetPosition);
        parameters.setMismatchMode(BalanceComputationParameters.MismatchMode.MAX);
        parameters.setSubtractLoadFlowBalancing(subtractLoadFlowBalancing);
        parameters.getLoadFlowParameters().setCountriesToBalance(
                new HashSet<>(countriesToBalance)
        );
        parameters.getLoadFlowParameters().setBalanceType(balanceType);
        parameters.getLoadFlowParameters().setTransformerVoltageControlOn(withRatioTapChangers);

        parameters.getLoadFlowParameters().getExtension(OpenLoadFlowParameters.class)
                .setSlackDistributionFailureBehavior(OpenLoadFlowParameters.SlackDistributionFailureBehavior.FAIL);
    }

    @SneakyThrows
    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode reportNode) {

        BalanceComputationParameters parameters = createBalanceComputationParameters(reportNode);

        List<BalanceComputationArea> balanceComputationAreas = createBalanceComputationAreas(network, reportNode);

        BalanceComputation balanceComputation = new BalanceComputationFactoryImpl()
            .create(balanceComputationAreas, LoadFlow.find(), new LocalComputationManager(Runnable::run));

        balanceComputation
            .run(network, network.getVariantManager().getWorkingVariantId(), parameters, reportNode)
            .join();
    }

    private List<BalanceComputationArea> createBalanceComputationAreas(Network network, ReportNode reportNode) {
        return areas
                .stream()
                .map(areaInfos ->
                        new BalanceComputationArea(
                                areaInfos.getName(),
                                new CountryAreaFactory(areaInfos.getCountries().toArray(Country[]::new)),
                                createScalable(areaInfos, network, reportNode),
                                areaInfos.getNetPosition()
                        )
                )
                .toList();
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
