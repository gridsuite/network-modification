/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.balances_adjustment.balance_computation.*;
import com.powsybl.balances_adjustment.util.CountryAreaFactory;
import com.powsybl.balances_adjustment.util.Reports;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.computation.ComputationManager;
import com.powsybl.iidm.modification.scalable.ProportionalScalable;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.*;
import com.powsybl.loadflow.LoadFlow;
import com.powsybl.loadflow.LoadFlowParameters;
import com.powsybl.openloadflow.OpenLoadFlowParameters;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
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
public class BalancesAdjustmentModification extends AbstractModification {
    private static final Logger LOGGER = LoggerFactory.getLogger(BalancesAdjustmentModification.class);

    private final BalancesAdjustmentModificationInfos balancesAdjustmentModificationInfos;

    protected ILoadFlowService loadFlowService;

    public BalancesAdjustmentModification(BalancesAdjustmentModificationInfos balancesAdjustmentModificationInfos) {
        this.balancesAdjustmentModificationInfos = balancesAdjustmentModificationInfos;
    }

    @Override
    public void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService) {
        this.loadFlowService = loadFlowService;
    }

    @Override
    public String getName() {
        return "BalancesAdjustmentModification";
    }

    private BalanceComputationParameters createBalanceComputationParameters(ReportNode reportNode) {
        BalanceComputationParameters parameters = BalanceComputationParameters.load();
        parameters.getScalingParameters().setPriority(ScalingParameters.Priority.RESPECT_OF_VOLUME_ASKED);

        if (!balancesAdjustmentModificationInfos.isWithLoadFlow()) {
            return parameters;
        }

        LoadFlowParametersInfos loadFlowParametersInfos = getLoadFlowParametersInfos(reportNode);

        if (loadFlowParametersInfos != null &&
                loadFlowParametersInfos.getProvider().equals("OpenLoadFlow")) {

            LoadFlowParameters loadFlowParameters = mapLoadFlowPrameters(loadFlowParametersInfos);
            parameters.setLoadFlowParameters(loadFlowParameters);
        }

        overrideBalanceComputationParameters(parameters);
        return parameters;
    }

    private LoadFlowParametersInfos getLoadFlowParametersInfos(ReportNode reportNode) {
        if (balancesAdjustmentModificationInfos.getLoadFlowParametersId() == null) {
            reportUsingDefaultParameters(reportNode, "Load flow parameters ID is null");
            return null;
        }

        LoadFlowParametersInfos loadFlowParametersInfos = loadFlowService.getLoadFlowParametersInfos(
                balancesAdjustmentModificationInfos.getLoadFlowParametersId()
        );

        if (loadFlowParametersInfos == null) {
            reportUsingDefaultParameters(reportNode,
                    "Load flow parameters with id " + balancesAdjustmentModificationInfos.getLoadFlowParametersId() + " not found");
            return null;
        }

        if (loadFlowParametersInfos.getProvider() == null) {
            reportUsingDefaultParameters(reportNode,
                    "Load flow provider is null in parameters with id " + balancesAdjustmentModificationInfos.getLoadFlowParametersId());
            return null;
        }

        return loadFlowParametersInfos;
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
        parameters.setMaxNumberIterations(balancesAdjustmentModificationInfos.getMaxNumberIterations());
        parameters.setThresholdNetPosition(balancesAdjustmentModificationInfos.getThresholdNetPosition());
        parameters.setMismatchMode(BalanceComputationParameters.MismatchMode.MAX);
        parameters.setSubtractLoadFlowBalancing(balancesAdjustmentModificationInfos.isSubtractLoadFlowBalancing());
        parameters.getLoadFlowParameters().setCountriesToBalance(
                new HashSet<>(balancesAdjustmentModificationInfos.getCountriesToBalance())
        );
        parameters.getLoadFlowParameters().setBalanceType(balancesAdjustmentModificationInfos.getBalanceType());
        parameters.getLoadFlowParameters().setTransformerVoltageControlOn(balancesAdjustmentModificationInfos.isWithRatioTapChangers());

        parameters.getLoadFlowParameters().getExtension(OpenLoadFlowParameters.class)
                .setSlackDistributionFailureBehavior(OpenLoadFlowParameters.SlackDistributionFailureBehavior.FAIL);
    }

    @Override
    public void apply(Network network,
                      NamingStrategy namingStrategy,
                      boolean throwException,
                      ComputationManager computationManager,
                      ReportNode reportNode) {

        BalanceComputationParameters parameters = createBalanceComputationParameters(reportNode);

        if (balancesAdjustmentModificationInfos.isWithLoadFlow()) {
            applyWithLoadFlow(network, computationManager, reportNode, parameters);
        } else {
            applyWithoutLoadFlow(network, reportNode, parameters);
        }
    }

    private void applyWithLoadFlow(Network network, ComputationManager computationManager,
                                   ReportNode reportNode, BalanceComputationParameters parameters) {
        List<BalanceComputationArea> areas = createBalanceComputationAreas(network, reportNode);

        BalanceComputation balanceComputation = new BalanceComputationFactoryImpl()
                .create(areas, LoadFlow.find(), computationManager);

        balanceComputation
                .run(network, network.getVariantManager().getWorkingVariantId(), parameters, reportNode)
                .join();
    }

    private void applyWithoutLoadFlow(Network network, ReportNode reportNode, BalanceComputationParameters parameters) {
        balancesAdjustmentModificationInfos.getAreas().forEach(area -> {
            CountryArea countryArea = new CountryArea(network, area.getCountries());
            double offset = area.getNetPosition() - countryArea.getNetPosition();
            Scalable scalable = createScalable(area, network, reportNode);
            double done = scalable.scale(network, offset, parameters.getScalingParameters());
            Reports.reportScaling(reportNode, area.getName(), offset, done);
            LOGGER.info("Scaling for area {}: offset={}, done={}", area.getName(), offset, done);
        });
    }

    private List<BalanceComputationArea> createBalanceComputationAreas(Network network, ReportNode reportNode) {
        return balancesAdjustmentModificationInfos
                .getAreas()
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
