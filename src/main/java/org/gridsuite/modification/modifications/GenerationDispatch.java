/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.google.common.util.concurrent.AtomicDouble;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import lombok.Builder;
import lombok.Getter;

import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Comparator.comparingInt;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.toCollection;
import static org.gridsuite.modification.NetworkModificationException.Type.GENERATION_DISPATCH_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class GenerationDispatch extends AbstractModification {
    private static final String POWER_TO_DISPATCH = "network.modification.PowerToDispatch";
    private static final String STACKING = "network.modification.Stacking";
    private static final String RESULT = "network.modification.Result";
    private static final String GENERATOR = "generator";
    private static final String SUBSTATION = "substation";
    private static final String REGION_CVG = "regionCvg";
    private static final String IS_PLURAL = "isPlural";
    private static final double EPSILON = 0.001;
    private static final String GENERATORS_WITH_FIXED_SUPPLY = "generatorsWithFixedSupply";
    private static final String GENERATORS_WITHOUT_OUTAGE = "generatorsWithoutOutage";
    private static final String GENERATORS_FREQUENCY_RESERVE = "generatorsFrequencyReserve";

    private final GenerationDispatchInfos generationDispatchInfos;

    protected IFilterService filterService;

    public GenerationDispatch(GenerationDispatchInfos generationDispatchInfos) {
        this.generationDispatchInfos = generationDispatchInfos;
    }

    private static void report(ReportNode reportNode, String key, Map<String, Object> values, TypedValue severity) {
        ModificationUtils.createReport(reportNode, key, values, severity);
    }

    private static double computeTotalActiveLoad(Component component) {
        Objects.requireNonNull(component);
        return component.getBusStream().flatMap(Bus::getLoadStream)
            .filter(load -> load.getTerminal().isConnected())
            .mapToDouble(Load::getP0)
            .sum();
    }

    private static double computeTotalDemand(Component component, double lossCoefficient) {
        double totalLoad = computeTotalActiveLoad(component);
        return totalLoad * (1. + lossCoefficient / 100.);
    }

    private static double computeTotalActiveBatteryTargetP(Component component) {
        Objects.requireNonNull(component);
        return component.getBusStream().flatMap(Bus::getBatteryStream)
                .filter(battery -> battery.getTerminal().isConnected())
                .mapToDouble(Battery::getTargetP)
                .sum();
    }

    private static double computeTotalAmountFixedSupply(Network network, Component component, List<String> generatorsWithFixedSupply, ReportNode reportNode) {
        double totalAmountFixedSupply = 0.;
        List<Generator> generatorsWithoutSetpointList = new ArrayList<>();
        totalAmountFixedSupply += generatorsWithFixedSupply.stream().map(network::getGenerator)
                .filter(generator -> generator != null && generator.getTerminal().isConnected() &&
                        generator.getTerminal().getBusView().getBus().getSynchronousComponent().getNum() == component.getNum())
                .peek(generator -> {
                    GeneratorStartup startupExtension = generator.getExtension(GeneratorStartup.class);
                    if (startupExtension != null && !Double.isNaN(startupExtension.getPlannedActivePowerSetpoint())) {
                        generator.setTargetP(startupExtension.getPlannedActivePowerSetpoint());
                    } else {
                        generator.setTargetP(0.);
                        generatorsWithoutSetpointList.add(generator);

                    }
                })
                .mapToDouble(Generator::getTargetP).sum();
        if (!generatorsWithoutSetpointList.isEmpty()) {
            report(reportNode, "network.modification.GeneratorsWithoutPredefinedActivePowerSetpoint",
                    Map.of("numGeneratorsWithoutSetpoint", generatorsWithoutSetpointList.size(),
                            IS_PLURAL, generatorsWithoutSetpointList.size() > 1 ? "s do" : " does"), TypedValue.WARN_SEVERITY);
        }

        // Report details for each generator without a predefined setpoint
        generatorsWithoutSetpointList.forEach(generator ->
                report(reportNode, "network.modification.MissingPredefinedActivePowerSetpointForGenerator",
                        Map.of("generatorId", generator.getId()), TypedValue.DETAIL_SEVERITY));
        return totalAmountFixedSupply;
    }

    private static boolean inDifferentSynchronousComponent(HvdcConverterStation<?> station, int componentNum) {
        Bus bus = station.getTerminal().getBusView().getBus();
        return bus != null && bus.getSynchronousComponent().getNum() != componentNum;
    }

    private static double computeHvdcBalance(Component component) {
        AtomicDouble balance = new AtomicDouble(0.);

        component.getBusStream().forEach(bus -> {
            double hdvcFlow = Stream.concat(bus.getLccConverterStationStream(), bus.getVscConverterStationStream())
                .filter(station -> {
                    // Keep only hvdc linking to another synchronous component
                    HvdcLine hvdcLine = station.getHvdcLine();
                    HvdcConverterStation<?> station1 = hvdcLine.getConverterStation1();
                    HvdcConverterStation<?> station2 = hvdcLine.getConverterStation2();

                    boolean station2NotInComponent = station1.getId().equals(station.getId()) && inDifferentSynchronousComponent(station2, component.getNum());
                    boolean station1NotInComponent = station2.getId().equals(station.getId()) && inDifferentSynchronousComponent(station1, component.getNum());
                    return station1NotInComponent || station2NotInComponent;
                })
                .mapToDouble(station -> {
                    // compute hvdc flux : import or export
                    HvdcLine hvdcLine = station.getHvdcLine();
                    HvdcConverterStation<?> station1 = hvdcLine.getConverterStation1();
                    HvdcConverterStation<?> station2 = hvdcLine.getConverterStation2();

                    if (station1.getId().equals(station.getId()) &&
                        hvdcLine.getConvertersMode() == HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER ||
                        station2.getId().equals(station.getId()) &&
                            hvdcLine.getConvertersMode() == HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER) {
                        return -hvdcLine.getActivePowerSetpoint();
                    } else {
                        return hvdcLine.getActivePowerSetpoint();
                    }
                }).sum();
            balance.addAndGet(hdvcFlow);
        });
        return balance.get();
    }

    private static Double getGeneratorMarginalCost(Generator generator) {
        GeneratorStartup startupExtension = generator.getExtension(GeneratorStartup.class);
        if (startupExtension != null && !Double.isNaN(startupExtension.getMarginalCost())) {
            return startupExtension.getMarginalCost();
        }
        return null;
    }

    private static Map<Double, List<String>> getGeneratorsByMarginalCost(List<Generator> generators, ReportNode reportNode) {
        Map<Double, List<String>> generatorsByMarginalCost = new TreeMap<>();

        // set targetP to 0
        generators.forEach(generator -> generator.setTargetP(0.));

        // get generators with marginal cost
        List<Generator> generatorsWithMarginalCost = generators.stream()
                .filter(generator -> getGeneratorMarginalCost(generator) != null)
                .collect(Collectors.toList());
        int nbNoCost = generators.size() - generatorsWithMarginalCost.size();
        if (nbNoCost > 0) {
            report(reportNode, "network.modification.NbGeneratorsWithNoCost",
                    Map.of("nbNoCost", nbNoCost,
                            IS_PLURAL, nbNoCost > 1 ? "s have" : " has"),
                    TypedValue.INFO_SEVERITY);
        }
        generators.stream()
            .filter(generator -> getGeneratorMarginalCost(generator) == null)
            .forEach(g -> report(reportNode, "network.modification.MissingMarginalCostForGenerator",
                    Map.of(GENERATOR, g.getId()), TypedValue.DETAIL_SEVERITY)
            );

        // build map of generators by marginal cost
        generatorsWithMarginalCost.sort(Comparator.comparing(GenerationDispatch::getGeneratorMarginalCost));
        generatorsWithMarginalCost.forEach(g -> {
            Double marginalCost = getGeneratorMarginalCost(g);
            generatorsByMarginalCost.computeIfAbsent(marginalCost, k -> new ArrayList<>());
            generatorsByMarginalCost.get(marginalCost).add(g.getId());
        });

        return generatorsByMarginalCost;
    }

    private static void reportUnknownSubstations(Network network, List<SubstationsGeneratorsOrderingInfos> substationsGeneratorsOrderingInfos, ReportNode reportNode) {
        if (!CollectionUtils.isEmpty(substationsGeneratorsOrderingInfos)) {
            substationsGeneratorsOrderingInfos.forEach(sInfo ->
                    sInfo.getSubstationIds().forEach(sId -> {
                        Substation substation = network.getSubstation(sId);
                        if (substation == null) {
                            report(reportNode, "network.modification.SubstationNotFound",
                                    Map.of(SUBSTATION, sId), TypedValue.WARN_SEVERITY);
                        }
                    }));
        }
    }

    private static List<Generator> computeAdjustableGenerators(Network network, Component component, List<String> generatorsWithFixedSupply,
                                                               List<SubstationsGeneratorsOrderingInfos> substationsGeneratorsOrderingInfos,
                                                               ReportNode reportNode) {
        List<String> generatorsToReturn = new ArrayList<>();

        // log substations not found
        reportUnknownSubstations(network, substationsGeneratorsOrderingInfos, reportNode);

        // get all connected generators in the component
        List<Generator> generators = component.getBusStream().flatMap(Bus::getGeneratorStream).collect(Collectors.toList());

        // remove generators with fixed supply
        generators.removeIf(generator -> generatorsWithFixedSupply.contains(generator.getId()));

        Map<Double, List<String>> generatorsByMarginalCost = getGeneratorsByMarginalCost(generators, reportNode);
        generatorsByMarginalCost.forEach((mCost, gList) -> {  // loop on generators of same cost
            if (!CollectionUtils.isEmpty(substationsGeneratorsOrderingInfos)) {  // substations hierarchy provided
                // build mapGeneratorsBySubstationsList, that will contain all the generators with the same marginal cost as mCost contained in each list of substations
                LinkedHashMap<Integer, Set<String>> mapGeneratorsBySubstationsList = new LinkedHashMap<>();

                AtomicInteger i = new AtomicInteger(0);
                substationsGeneratorsOrderingInfos.forEach(sInfo -> {
                    mapGeneratorsBySubstationsList.computeIfAbsent(i.get(), k -> new TreeSet<>());

                    // get generators with marginal cost == mCost in all substations of the current list
                    sInfo.getSubstationIds().forEach(sId -> {
                        Substation substation = network.getSubstation(sId);
                        if (substation != null) {
                            substation.getVoltageLevelStream().forEach(v ->
                                v.getGeneratorStream().filter(g -> {
                                    Double generatorCost = getGeneratorMarginalCost(g);
                                    return generatorCost != null && generatorCost.equals(mCost) && !generatorsWithFixedSupply.contains(g.getId());
                                }).forEach(g -> mapGeneratorsBySubstationsList.get(i.get()).add(g.getId())));
                        }
                    });

                    i.incrementAndGet();
                });

                // loop until all the generators have been encountered
                AtomicBoolean finished = new AtomicBoolean(false);
                while (!finished.get()) {
                    finished.set(true);
                    mapGeneratorsBySubstationsList.values().forEach(generatorsSet -> {
                        if (generatorsSet.isEmpty()) {  // no generators
                            return;
                        }
                        Optional<String> gId = generatorsSet.stream().findFirst();
                        generatorsToReturn.add(gId.get());
                        generatorsSet.remove(gId.get());
                        finished.set(false);
                    });
                }

                // add in the result the generators with same cost not found in mapGeneratorsBySubstationsList sorted in alphabetical order
                gList.stream().sorted().forEach(gId -> {
                    if (!generatorsToReturn.contains(gId)) {
                        generatorsToReturn.add(gId);
                    }
                });
            } else {  // no substations hierarchy provided
                // add in the result the generators in gList sorted in alphabetical order
                gList.stream().sorted().forEach(generatorsToReturn::add);
            }
        });

        if (generatorsToReturn.isEmpty()) {
            report(reportNode, "network.modification.NoAvailableAdjustableGenerator",
                Map.of(), TypedValue.WARN_SEVERITY);
        }

        return generatorsToReturn.stream().map(network::getGenerator).toList();
    }

    private static class GeneratorTargetPListener extends DefaultNetworkListener {
        private final ReportNode reportNode;
        private final List<Generator> updatedGenerators = new ArrayList<>();

        GeneratorTargetPListener(ReportNode reportNode) {
            this.reportNode = reportNode;
        }

        @Override
        public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
            if (identifiable.getType() == IdentifiableType.GENERATOR && attribute.equals("targetP") && Double.compare((double) oldValue, (double) newValue) != 0) {
                updatedGenerators.add((Generator) identifiable);
            }
        }

        public void endReport(List<Generator> adjustableGenerators) {
            // report updated generators
            report(reportNode, "network.modification.TotalGeneratorSetTargetP",
                    Map.of("nbUpdatedGenerator", updatedGenerators.size(), IS_PLURAL, updatedGenerators.size() > 1 ? "s" : ""), TypedValue.INFO_SEVERITY);
            updatedGenerators.forEach(g -> report(reportNode, "network.modification.GeneratorSetTargetP",
                    Map.of(GENERATOR, g.getId(), "newValue", round(g.getTargetP())), TypedValue.DETAIL_SEVERITY));

            // report unchanged generators
            int nbUnchangedGenerators = adjustableGenerators.size() - updatedGenerators.size();
            if (nbUnchangedGenerators > 0) {
                List<String> updatedGeneratorsIds = updatedGenerators.stream().map(Identifiable::getId).toList();
                report(reportNode, "network.modification.TotalGeneratorUnchangedTargetP",
                        Map.of("nbUnchangedGenerator", nbUnchangedGenerators,
                                IS_PLURAL, nbUnchangedGenerators > 1 ? "s have" : " has"), TypedValue.INFO_SEVERITY);
                adjustableGenerators.stream()
                        .filter(g -> !updatedGeneratorsIds.contains(g.getId()))
                        .forEach(g -> report(reportNode, "network.modification.GeneratorUnchangedTargetP",
                                Map.of(GENERATOR, g.getId()), TypedValue.DETAIL_SEVERITY));
            }
            // report the max marginal cost used
            Double maxUsedMarginalCost = updatedGenerators.stream()
                    .map(GenerationDispatch::getGeneratorMarginalCost)
                    .filter(Objects::nonNull)
                    .mapToDouble(Double::doubleValue).max().orElseThrow();

            report(reportNode, "network.modification.MaxUsedMarginalCost",
                    Map.of("maxUsedMarginalCost", maxUsedMarginalCost), TypedValue.INFO_SEVERITY);
        }
    }

    @Builder
    @Getter
    private static final class GeneratorsFrequencyReserve {
        private final List<String> generators;
        private final double frequencyReserve;
    }

    @Override
    public void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService) {
        this.filterService = filterService;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        double lossCoefficient = generationDispatchInfos.getLossCoefficient();
        if (lossCoefficient < 0. || lossCoefficient > 100.) {
            throw new NetworkModificationException(GENERATION_DISPATCH_ERROR, "The loss coefficient must be between 0 and 100");
        }
        double defaultOutageRate = generationDispatchInfos.getDefaultOutageRate();
        if (defaultOutageRate < 0. || defaultOutageRate > 100.) {
            throw new NetworkModificationException(GENERATION_DISPATCH_ERROR, "The default outage rate must be between 0 and 100");
        }
    }

    private List<String> exportFilters(List<GeneratorsFilterInfos> generatorsFilters, Network network, ReportNode subReportNode, String generatorsType) {
        if (CollectionUtils.isEmpty(generatorsFilters)) {
            return List.of();
        }
        var filters = generatorsFilters.stream().collect(Collectors.toMap(GeneratorsFilterInfos::getId, GeneratorsFilterInfos::getName, (id1, id2) -> id1, LinkedHashMap::new));

        // export filters
        Map<UUID, FilterEquipments> exportedGenerators = filterService
            .exportFilters(new ArrayList<>(filters.keySet()), network)
            .map(f -> new FilterEquipments(f.getFilterId(), filters.get(f.getFilterId()),
                f.getIdentifiableAttributes().stream().map(i -> new IdentifiableAttributes(i.getId(), i.getType(), i.getDistributionKey())).toList(),
                f.getNotFoundEquipments()))
            .collect(Collectors.toMap(FilterEquipments::getFilterId, Function.identity()));

        // report filters with generators not found
        Map<UUID, FilterEquipments> filtersWithGeneratorsNotFound = exportedGenerators.entrySet().stream()
            .filter(e -> !CollectionUtils.isEmpty(e.getValue().getNotFoundEquipments()))
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        filtersWithGeneratorsNotFound.values().forEach(f -> {
            var filterName = filters.get(f.getFilterId());
            var notFoundGenerators = f.getNotFoundEquipments();
            report(subReportNode, "network.modification.filterGeneratorsNotFound." + generatorsType,
                Map.of("nbNotFoundGen", notFoundGenerators.size(), "filterName", filterName),
                TypedValue.WARN_SEVERITY);
            f.getNotFoundEquipments().forEach(e -> report(subReportNode, "network.modification.generatorNotFound." + generatorsType,
                Map.of("notFoundGeneratorId", e, "filterName", filterName), TypedValue.DETAIL_SEVERITY));
        });

        // return existing generators
        return exportedGenerators.values().stream()
            .flatMap(f -> f.getIdentifiableAttributes().stream())
            .map(IdentifiableAttributes::getId)
            .distinct()
            .collect(Collectors.toList());
    }

    private List<String> collectGeneratorsWithoutOutage(Network network, ReportNode subReportNode) {
        return exportFilters(generationDispatchInfos.getGeneratorsWithoutOutage(), network, subReportNode, GENERATORS_WITHOUT_OUTAGE);
    }

    private List<String> collectGeneratorsWithFixedSupply(Network network, ReportNode subReportNode) {
        return exportFilters(generationDispatchInfos.getGeneratorsWithFixedSupply(), network, subReportNode, GENERATORS_WITH_FIXED_SUPPLY);
    }

    private List<GeneratorsFrequencyReserve> collectGeneratorsWithFrequencyReserve(Network network, ReportNode subReportNode) {
        return generationDispatchInfos.getGeneratorsFrequencyReserve().stream().map(g -> {
            List<String> generators = exportFilters(g.getGeneratorsFilters(), network, subReportNode, GENERATORS_FREQUENCY_RESERVE);
            return GeneratorsFrequencyReserve.builder().generators(generators).frequencyReserve(g.getFrequencyReserve()).build();
        }).collect(Collectors.toList());
    }

    private static double computeGenFrequencyReserve(Generator generator,
                                                     List<GeneratorsFrequencyReserve> generatorsFrequencyReserve) {
        AtomicReference<Double> freqReserve = new AtomicReference<>(0.);
        generatorsFrequencyReserve.forEach(g -> {
            if (g.getGenerators().contains(generator.getId())) {
                freqReserve.set(g.getFrequencyReserve());
            }
        });
        return freqReserve.get();
    }

    private double reduceGeneratorMaxPValue(Generator generator,
                                            List<String> generatorsWithoutOutage,
                                            List<GeneratorsFrequencyReserve> generatorsFrequencyReserve) {
        double res = generator.getMaxP();
        if (!generatorsWithoutOutage.contains(generator.getId())) {
            GeneratorStartup startupExtension = generator.getExtension(GeneratorStartup.class);
            if (startupExtension != null &&
                !Double.isNaN(startupExtension.getForcedOutageRate()) &&
                !Double.isNaN(startupExtension.getPlannedOutageRate())) {
                res *= (1. - startupExtension.getForcedOutageRate()) * (1. - startupExtension.getPlannedOutageRate());
            } else {
                res *= 1. - generationDispatchInfos.getDefaultOutageRate() / 100.;
            }
        }
        double genFrequencyReserve = computeGenFrequencyReserve(generator, generatorsFrequencyReserve);
        return Math.max(generator.getMinP(), res * (1. - genFrequencyReserve / 100.));
    }

    private void reportDisconnectedGenerators(List<Generator> globalDisconnectedGenerators, int componentNum, ReportNode reportNode) {
        List<Generator> componentDisconnectedGenerators = globalDisconnectedGenerators.stream()
                .filter(g -> g.getTerminal().getBusView() != null && g.getTerminal().getBusView().getConnectableBus() != null &&
                        g.getTerminal().getBusView().getConnectableBus().getSynchronousComponent().getNum() == componentNum)
                .toList();
        if (!componentDisconnectedGenerators.isEmpty()) {
            report(reportNode, "network.modification.TotalDisconnectedGenerator",
                    Map.of("nbDisconnectedGenerator", componentDisconnectedGenerators.size(),
                            IS_PLURAL, componentDisconnectedGenerators.size() > 1 ? "s have" : " has"),
                    TypedValue.INFO_SEVERITY);
            componentDisconnectedGenerators.forEach(g ->
                report(reportNode, "network.modification.DisconnectedGenerator",
                        Map.of(GENERATOR, g.getId()), TypedValue.DETAIL_SEVERITY)
            );
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Collection<Component> synchronousComponents = network.getBusView().getBusStream()
            .filter(Bus::isInMainConnectedComponent)
            .map(Bus::getSynchronousComponent)
            .collect(collectingAndThen(toCollection(() -> new TreeSet<>(comparingInt(Component::getNum))), ArrayList::new));

        report(subReportNode, "network.modification.NbSynchronousComponents",
                Map.of("scNumber", synchronousComponents.size(),
                        IS_PLURAL, synchronousComponents.size() > 1 ? "s" : "",
                        "scList", synchronousComponents.stream().map(sc -> "SC" + sc.getNum()).collect(Collectors.joining(", "))),
                TypedValue.INFO_SEVERITY);

        // all disconnected generators at network level (for report purpose)
        List<Generator> disconnectedGenerators = network.getGeneratorStream()
                .filter(g -> !g.getTerminal().isConnected())
                .toList();

        // get generators for which there will be no reduction of maximal power
        List<String> generatorsWithoutOutage = collectGeneratorsWithoutOutage(network, subReportNode);

        // get generators with fixed supply
        List<String> generatorsWithFixedSupply = collectGeneratorsWithFixedSupply(network, subReportNode);

        // get generators with frequency reserve
        List<GeneratorsFrequencyReserve> generatorsWithFrequencyReserve = collectGeneratorsWithFrequencyReserve(network, subReportNode);

        for (Component component : synchronousComponents) {
            int componentNum = component.getNum();

            ReportNode componentReportNode = subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.NetworkCC0.SYNCHRONOUS_COMPONENT")
                    .withUntypedValue("componentNum", componentNum)
                    .add();

            ReportNode powerToDispatchReportNode = componentReportNode.newReportNode()
                    .withMessageTemplate(POWER_TO_DISPATCH)
                    .add();

            // log disconnected generators attached to this synchronous component
            reportDisconnectedGenerators(disconnectedGenerators, componentNum, powerToDispatchReportNode);

            // get total value of connected loads in the connected component
            double totalDemand = computeTotalDemand(component, generationDispatchInfos.getLossCoefficient());
            report(powerToDispatchReportNode, "network.modification.TotalDemand",
                Map.of("totalDemand", round(totalDemand)), TypedValue.INFO_SEVERITY);

            // get total supply value for generators with fixed supply
            double totalAmountFixedSupply = computeTotalAmountFixedSupply(network, component, generatorsWithFixedSupply, powerToDispatchReportNode);
            report(powerToDispatchReportNode, "network.modification.TotalAmountFixedSupply",
                Map.of("totalAmountFixedSupply", round(totalAmountFixedSupply)), TypedValue.INFO_SEVERITY);

            // compute hvdc balance to other synchronous components
            double hvdcBalance = computeHvdcBalance(component);
            report(powerToDispatchReportNode, "network.modification.TotalOutwardHvdcFlow",
                Map.of("hvdcBalance", round(hvdcBalance)), TypedValue.INFO_SEVERITY);

            double activeBatteryTotalTargetP = computeTotalActiveBatteryTargetP(component);
            report(powerToDispatchReportNode, "network.modification.TotalActiveBatteryTargetP",
                    Map.of("batteryBalance", round(activeBatteryTotalTargetP)), TypedValue.INFO_SEVERITY);

            double totalAmountSupplyToBeDispatched = totalDemand - totalAmountFixedSupply - hvdcBalance - activeBatteryTotalTargetP;
            if (totalAmountSupplyToBeDispatched < 0.) {
                report(powerToDispatchReportNode, "network.modification.TotalAmountFixedSupplyExceedsTotalDemand",
                    Map.of(), TypedValue.WARN_SEVERITY);
                continue;
            } else {
                report(powerToDispatchReportNode, "network.modification.TotalAmountSupplyToBeDispatched",
                    Map.of("totalAmountSupplyToBeDispatched", round(totalAmountSupplyToBeDispatched)), TypedValue.INFO_SEVERITY);
            }

            // get adjustable generators in the component
            List<Generator> adjustableGenerators = computeAdjustableGenerators(network, component, generatorsWithFixedSupply,
                                                                               generationDispatchInfos.getSubstationsGeneratorsOrdering(),
                                                                               powerToDispatchReportNode);

            double realized = 0.;
            if (!adjustableGenerators.isEmpty()) {
                // stacking of adjustable generators to ensure the totalAmountSupplyToBeDispatched
                List<Scalable> generatorsScalable = adjustableGenerators.stream().map(generator -> {
                    double minValue = generator.getMinP();
                    double maxValue = reduceGeneratorMaxPValue(generator, generatorsWithoutOutage, generatorsWithFrequencyReserve);
                    return (Scalable) Scalable.onGenerator(generator.getId(), minValue, maxValue);
                }).toList();

                ReportNode stackingReportNode = componentReportNode.newReportNode()
                        .withMessageTemplate(STACKING)
                        .add();

                GeneratorTargetPListener listener = new GeneratorTargetPListener(stackingReportNode);
                network.addListener(listener);

                Scalable scalable = Scalable.stack(generatorsScalable.toArray(Scalable[]::new));
                realized = scalable.scale(network, totalAmountSupplyToBeDispatched, new ScalingParameters().setAllowsGeneratorOutOfActivePowerLimits(true));

                listener.endReport(adjustableGenerators);
                network.removeListener(listener);
            }

            ReportNode resultReporter = componentReportNode.newReportNode()
                    .withMessageTemplate(RESULT)
                    .add();

            if (Math.abs(totalAmountSupplyToBeDispatched - realized) < EPSILON) {
                Map<String, List<Generator>> generatorsByRegion = getGeneratorsByRegion(network, component);

                report(resultReporter, "network.modification.SupplyDemandBalanceCouldBeMet",
                    Map.of(), TypedValue.INFO_SEVERITY);
                generatorsByRegion.forEach((region, generators) -> {
                    Map<EnergySource, Double> activePowerSumByEnergySource = getActivePowerSumByEnergySource(generators);
                    report(resultReporter, "network.modification.SumGeneratorActivePower",
                            Map.of("region", region,
                                    "sum", round(activePowerSumByEnergySource.values().stream().reduce(0d, Double::sum)),
                                    "nuclearSum", round(activePowerSumByEnergySource.getOrDefault(EnergySource.NUCLEAR, 0d)),
                                    "thermalSum", round(activePowerSumByEnergySource.getOrDefault(EnergySource.THERMAL, 0d)),
                                    "hydroSum", round(activePowerSumByEnergySource.getOrDefault(EnergySource.HYDRO, 0d)),
                                    "windAndSolarSum", round(activePowerSumByEnergySource.getOrDefault(EnergySource.WIND, 0d) + activePowerSumByEnergySource.getOrDefault(EnergySource.SOLAR, 0d)),
                                    "otherSum", round(activePowerSumByEnergySource.getOrDefault(EnergySource.OTHER, 0d))
                                    ), TypedValue.INFO_SEVERITY);
                });
            } else {
                double remainingPowerImbalance = totalAmountSupplyToBeDispatched - realized;
                report(resultReporter, "network.modification.SupplyDemandBalanceCouldNotBeMet",
                    Map.of("remainingPower", round(remainingPowerImbalance)), TypedValue.WARN_SEVERITY);
            }
        }
    }

    @Override
    public String getName() {
        return "GenerationDispatch";
    }

    private Map<String, List<Generator>> getGeneratorsByRegion(Network network, Component component) {
        // get all connected generators  that are inside the synchronous component and the substationIds associated.
        List<Generator> connectedGenerators = network.getGeneratorStream()
                .filter(g -> g.getTerminal().isConnected() && g.getTerminal().getBusView().getBus().getSynchronousComponent().getNum() == component.getNum())
                .toList();
        List<String> substationIds = connectedGenerators.stream()
                .map(g -> g.getTerminal().getVoltageLevel().getSubstation().map(Substation::getId).orElse(null))
                .filter(Objects::nonNull)
                .toList();
        // get all substations with "regionCvg" property name
        Map<String, String> substationIdPropertiesMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(substationIds)) {
            substationIds.forEach(sId -> {
                Substation substation = network.getSubstation(sId);
                if (!substation.getPropertyNames().isEmpty() && hasCvgPropertyName(substation.getPropertyNames())) {
                    substation.getPropertyNames().forEach(property -> {
                        if (REGION_CVG.equals(property)) {
                            substationIdPropertiesMap.put(substation.getId(), substation.getProperty(property));
                        }
                    });
                }
            });
        }

        // group substationIds by region
        Map<String, List<String>> groupedSubstationIds = substationIdPropertiesMap.keySet().stream().collect(Collectors.groupingBy(substationIdPropertiesMap::get));

        // iterate over groupedSubstationIds and check for each substation list if it's related to the connected generators
        Map<String, List<Generator>> generatorsByRegion = new HashMap<>();

        groupedSubstationIds.forEach((region, substationList) -> {
            List<Generator> connectedGeneratorsWithSubstation = connectedGenerators.stream()
                    .filter(g -> substationList.contains(g.getTerminal().getVoltageLevel().getSubstation().map(Substation::getId).orElse(null)))
                    .toList();
            generatorsByRegion.put(region, connectedGeneratorsWithSubstation);
        });

        return generatorsByRegion;
    }

    private boolean hasCvgPropertyName(Set<String> propertyNames) {
        return propertyNames.stream().anyMatch(REGION_CVG::equals);
    }

    private Map<EnergySource, Double> getActivePowerSumByEnergySource(List<Generator> generators) {
        return generators.stream().collect(Collectors.toMap(Generator::getEnergySource, Generator::getTargetP, Double::sum));
    }

    private static double round(double value) {
        return Math.round(value * 10) / 10.;
    }
}
