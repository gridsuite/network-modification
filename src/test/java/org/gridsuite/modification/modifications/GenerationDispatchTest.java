/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.FilterEquipments;
import org.gridsuite.filter.identifierlistfilter.IdentifiableAttributes;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.*;

import static org.gridsuite.modification.utils.TestUtils.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class GenerationDispatchTest extends AbstractNetworkModificationTest {
    private static final String GH1_ID = "GH1";
    private static final String GH2_ID = "GH2";
    private static final String GH3_ID = "GH3";
    private static final String GTH1_ID = "GTH1";
    private static final String GTH2_ID = "GTH2";
    private static final String BATTERY1_ID = "BATTERY1";
    private static final String BATTERY2_ID = "BATTERY2";
    private static final String BATTERY3_ID = "BATTERY3";
    private static final String TEST1_ID = "TEST1";
    private static final String GROUP1_ID = "GROUP1";
    private static final String GROUP2_ID = "GROUP2";
    private static final String GROUP3_ID = "GROUP3";
    private static final String ABC_ID = "ABC";
    private static final String NEW_GROUP1_ID = "newGroup1";
    private static final String NEW_GROUP2_ID = "newGroup2";
    private static final String GEN1_NOT_FOUND_ID = "notFoundGen1";
    private static final String GEN2_NOT_FOUND_ID = "notFoundGen2";
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
    private static final UUID FILTER_ID_6 = UUID.randomUUID();

    @Mock
    private IFilterService filterService;

    @BeforeEach
    public void specificSetUp() {
        MockitoAnnotations.openMocks(this);
    }

    private static IdentifiableAttributes getIdentifiableAttributes(String id) {
        return new IdentifiableAttributes(id, IdentifiableType.GENERATOR, null);
    }

    private void assertLogReportsForDefaultNetwork(double batteryBalanceOnSc2, ReportNode report) {
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "network.modification.TotalDemand", report);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessage("The battery balance is : 0.0 MW", "network.modification.TotalActiveBatteryTargetP", report);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 138.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", report);
        // on SC 2, we have to substract the battery balance
        final double defaultTotalAmount = 330.0;
        DecimalFormat df = new DecimalFormat("#0.0", new DecimalFormatSymbols(Locale.US));
        final String totalAmount = df.format(defaultTotalAmount - batteryBalanceOnSc2);
        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", report);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessageWithoutRank("The battery balance is : " + df.format(batteryBalanceOnSc2) + " MW", "network.modification.TotalActiveBatteryTargetP", report);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : " + totalAmount + " MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", report);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", report);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in SOUTH region: " + totalAmount + " MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: " + totalAmount + " MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "network.modification.SumGeneratorActivePower", report);
    }

    @Override
    @Test
    public void testApply() throws Exception {
        GenerationDispatch modif = (GenerationDispatch) buildModification().toModification();
        modif.initApplicationContext(filterService, null);
        modif.apply(getNetwork());
        assertAfterNetworkModificationApplication();
    }

    @Test
    void testGenerationDispatch() throws Exception {
        GenerationDispatchInfos modification = buildModification();

        // network with 2 synchronous components, no battery, 2 hvdc lines between them and no forcedOutageRate and plannedOutageRate for the generators
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));

        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        modif.apply(getNetwork(), report);
        assertNetworkAfterCreationWithStandardLossCoefficient();

        assertLogReportsForDefaultNetwork(0., report);
    }

    @Test
    void testGenerationDispatchWithBattery() throws Exception {
        GenerationDispatchInfos modification = buildModification();

        // same than testGenerationDispatch, with 3 Batteries (in 2nd SC)
        setNetwork(Network.read("testGenerationDispatchWithBatteries.xiidm", getClass().getResourceAsStream("/testGenerationDispatchWithBatteries.xiidm")));
        // only 2 are connected
        assertTrue(getNetwork().getBattery(BATTERY1_ID).getTerminal().isConnected());
        assertTrue(getNetwork().getBattery(BATTERY2_ID).getTerminal().isConnected());
        assertFalse(getNetwork().getBattery(BATTERY3_ID).getTerminal().isConnected());
        final double batteryTotalTargetP = getNetwork().getBattery(BATTERY1_ID).getTargetP() + getNetwork().getBattery(BATTERY2_ID).getTargetP();

        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        modif.apply(getNetwork(), report);
        assertLogReportsForDefaultNetwork(batteryTotalTargetP, report);
    }

    @Test
    void testGenerationDispatchWithBatteryConnection() throws Exception {
        ModificationInfos modification = buildModification();

        // network with 3 Batteries (in 2nd SC)
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatchWithBatteries.xiidm")));
        // connect the 3rd one
        assertTrue(getNetwork().getBattery(BATTERY1_ID).getTerminal().isConnected());
        assertTrue(getNetwork().getBattery(BATTERY2_ID).getTerminal().isConnected());
        assertFalse(getNetwork().getBattery(BATTERY3_ID).getTerminal().isConnected());
        assertTrue(getNetwork().getBattery(BATTERY3_ID).getTargetP() > 0);
        getNetwork().getBattery(BATTERY3_ID).getTerminal().connect();
        final double batteryTotalTargetP = getNetwork().getBattery(BATTERY1_ID).getTargetP() + getNetwork().getBattery(BATTERY2_ID).getTargetP() + getNetwork().getBattery(BATTERY3_ID).getTargetP();

        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        modif.apply(getNetwork(), report);
        assertLogReportsForDefaultNetwork(batteryTotalTargetP, report);
    }

    @Test
    void testGenerationDispatchWithMultipleEnergySource() throws Exception {
        ModificationInfos modification = buildModification();

        setNetwork(Network.read("testGenerationDispatchWithMultipleEnergySource.xiidm", getClass().getResourceAsStream("/testGenerationDispatchWithMultipleEnergySource.xiidm")));

        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        modif.apply(getNetwork(), report);

        assertLogMessageWithoutRank("The total demand is : 768.0 MW", "network.modification.TotalDemand", report);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 858.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessageWithoutRank("Marginal cost: 28.0", "network.modification.MaxUsedMarginalCost", report);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", report);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in SOUTH region: 858.0 MW (NUCLEAR: 150.0 MW, THERMAL: 200.0 MW, HYDRO: 108.0 MW, WIND AND SOLAR: 150.0 MW, OTHER: 250.0 MW).", "network.modification.SumGeneratorActivePower", report);
    }

    @Test
    void testGenerationDispatchWithHigherLossCoefficient() throws Exception {
        GenerationDispatchInfos modification = buildModification();
        modification.setLossCoefficient(90.);

        // network with 2 synchronous components, 2 hvdc lines between them and no forcedOutageRate and plannedOutageRate for the generators
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));

        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        modif.apply(getNetwork(), report);

        assertEquals(100., getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(70., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on synchronous components
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 836.0 MW", "network.modification.TotalDemand", report);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessage("The total amount of supply to be dispatched is : 746.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 446.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", report);

        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 380.0 MW", "network.modification.TotalDemand", report);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 470.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessageWithoutRank("The supply-demand balance could not be met : the remaining power imbalance is 70.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", report);
    }

    @Test
    void testGenerationDispatchWithInternalHvdc() throws Exception {
        GenerationDispatchInfos modification = buildModification();

        // network with unique synchronous component, 2 internal hvdc lines and no forcedOutageRate and plannedOutageRate for the generators
        setNetwork(Network.read("testGenerationDispatchInternalHvdc.xiidm", getClass().getResourceAsStream("/testGenerationDispatchInternalHvdc.xiidm")));

        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        modif.apply(getNetwork(), report);

        assertEquals(100., getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(70., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on unique synchronous component
        // GTH1 is in the unique synchronous component
        assertLogMessage("The total demand is : 768.0 MW", "network.modification.TotalDemand", report);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessage("The HVDC balance is : 0.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessage("The total amount of supply to be dispatched is : 768.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 68.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", report);
    }

    @Test
    void testGenerationDispatchWithMaxPReduction() throws Exception {
        GenerationDispatchInfos modification = buildModification();
        modification.setDefaultOutageRate(15.);
        modification.setGeneratorsWithoutOutage(
            List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build()));

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate and plannedOutageRate defined for the generators
        setNetwork(Network.read("testGenerationDispatchReduceMaxP.xiidm", getClass().getResourceAsStream("/testGenerationDispatchReduceMaxP.xiidm")));

        List<FilterEquipments> filterEquipments = List.of(new FilterEquipments(FILTER_ID_1, List.of(getIdentifiableAttributes(GTH2_ID), getIdentifiableAttributes(GROUP1_ID)), List.of()),
            new FilterEquipments(FILTER_ID_2, List.of(getIdentifiableAttributes(ABC_ID), getIdentifiableAttributes(GH3_ID)), List.of()),
            new FilterEquipments(FILTER_ID_3, List.of(), List.of(GEN1_NOT_FOUND_ID, GEN2_NOT_FOUND_ID)));

        when(filterService.exportFilters(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3), getNetwork())).thenReturn(filterEquipments.stream());

        List<AbstractFilter> filters = List.of(
            new IdentifierListFilter(FILTER_ID_1, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_2, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_3, new Date(), EquipmentType.GENERATOR, List.of()));
        when(filterService.getFilters(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3))).thenReturn(filters);

        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modif.apply(getNetwork(), report);

        assertEquals(74.82, getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(59.5, getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(76.5, getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(42.5, getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(65.68, getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on synchronous components
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "network.modification.TotalDemand", report);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 169.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", report);

        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", report);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 330.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", report);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", report);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in WEST region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "network.modification.SumGeneratorActivePower", report);
    }

    @Test
    void testGenerationDispatchGeneratorsWithFixedSupply() throws Exception {
        GenerationDispatchInfos modification = buildModification();
        modification.setDefaultOutageRate(15.);
        modification.setGeneratorsWithoutOutage(
            List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build()));
        modification.setGeneratorsWithFixedSupply(
            List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_4).name("filter4").build()));

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate, plannedOutageRate, predefinedActivePowerSetpoint defined for some generators
        setNetwork(Network.read("testGenerationDispatchFixedActivePower.xiidm", getClass().getResourceAsStream("/testGenerationDispatchFixedActivePower.xiidm")));

        List<FilterEquipments> filtersForPmaxReduction = List.of(new FilterEquipments(FILTER_ID_1, List.of(getIdentifiableAttributes(GTH1_ID), getIdentifiableAttributes(GROUP1_ID)), List.of()),
            new FilterEquipments(FILTER_ID_2, List.of(getIdentifiableAttributes(ABC_ID), getIdentifiableAttributes(GH3_ID)), List.of()),
            new FilterEquipments(FILTER_ID_3, List.of(), List.of(GEN1_NOT_FOUND_ID, GEN2_NOT_FOUND_ID)));
        when(filterService.exportFilters(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3), getNetwork())).thenReturn(filtersForPmaxReduction.stream());

        List<FilterEquipments> filtersForFixedSupply = List.of(new FilterEquipments(FILTER_ID_1, List.of(getIdentifiableAttributes(GTH1_ID), getIdentifiableAttributes(GROUP1_ID)), List.of(GEN1_NOT_FOUND_ID)),
            new FilterEquipments(FILTER_ID_4, List.of(getIdentifiableAttributes(TEST1_ID), getIdentifiableAttributes(GROUP2_ID)), List.of()));
        when(filterService.exportFilters(List.of(FILTER_ID_1, FILTER_ID_4), getNetwork())).thenReturn(filtersForFixedSupply.stream());

        List<AbstractFilter> filters = List.of(
            new IdentifierListFilter(FILTER_ID_1, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_2, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_3, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_4, new Date(), EquipmentType.GENERATOR, List.of()));
        when(filterService.getFilters(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4))).thenReturn(filters);

        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modif.apply(getNetwork(), report);

        assertEquals(74.82, getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(59.5, getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(90., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(0., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(65.68, getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        assertLogMessage("Generators without outage simulation: Cannot find 2 generators in filter filter3", "network.modification.filterGeneratorsNotFound.generatorsWithoutOutage", report);
        assertLogMessage("Generators without outage simulation: Cannot find generator notFoundGen1 in filter filter3", "network.modification.generatorNotFound.generatorsWithoutOutage", report);
        assertLogMessageWithoutRank("Generators without outage simulation: Cannot find generator notFoundGen2 in filter filter3", "network.modification.generatorNotFound.generatorsWithoutOutage", report);
        assertLogMessage("Generators with fixed active power: Cannot find 1 generators in filter filter1", "network.modification.filterGeneratorsNotFound.generatorsWithFixedSupply", report);
        assertLogMessage("Generators with fixed active power: Cannot find generator notFoundGen1 in filter filter1", "network.modification.generatorNotFound.generatorsWithFixedSupply", report);

        // test total demand and remaining power imbalance on synchronous components
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 60.0 MW", "network.modification.TotalDemand", report);
        assertLogMessage("The total amount of fixed supply is : 90.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessage("The total amount of fixed supply exceeds the total demand", "network.modification.TotalAmountFixedSupplyExceedsTotalDemand", report);

        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", report);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 330.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", report);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", report);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in EAST region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "network.modification.SumGeneratorActivePower", report);
    }

    private static List<GeneratorsFilterInfos> getGeneratorsFiltersInfosWithFilters123() {
        return List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build());
    }

    private static List<GeneratorsFrequencyReserveInfos> getGeneratorsFrequencyReserveInfosWithFilters456() {
        return List.of(GeneratorsFrequencyReserveInfos.builder().frequencyReserve(3.)
                        .generatorsFilters(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_4).name("filter4").build(),
                                GeneratorsFilterInfos.builder().id(FILTER_ID_5).name("filter5").build())).build(),
                GeneratorsFrequencyReserveInfos.builder().frequencyReserve(5.)
                        .generatorsFilters(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_6).name("filter6").build())).build());
    }

    private static List<FilterEquipments> getGeneratorsWithoutOutageFilters123() {
        return List.of(new FilterEquipments(FILTER_ID_1, List.of(getIdentifiableAttributes(GTH2_ID), getIdentifiableAttributes(GROUP1_ID)), List.of()),
                new FilterEquipments(FILTER_ID_2, List.of(getIdentifiableAttributes(ABC_ID), getIdentifiableAttributes(GH3_ID)), List.of()),
                new FilterEquipments(FILTER_ID_3, List.of(), List.of(GEN1_NOT_FOUND_ID, GEN2_NOT_FOUND_ID)));
    }

    private static List<FilterEquipments> getGeneratorsFrequencyReserveFilters45() {
        return List.of(new FilterEquipments(FILTER_ID_4, List.of(getIdentifiableAttributes(GTH1_ID)), List.of()),
                new FilterEquipments(FILTER_ID_5, List.of(getIdentifiableAttributes(GTH2_ID), getIdentifiableAttributes(GH3_ID)), List.of(GEN1_NOT_FOUND_ID)));
    }

    private static List<FilterEquipments> getGeneratorsFrequencyReserveFilter6() {
        return List.of(new FilterEquipments(FILTER_ID_6, List.of(getIdentifiableAttributes(TEST1_ID)), List.of()));
    }

    @Test
    void testGenerationDispatchWithFrequencyReserve() throws Exception {
        GenerationDispatchInfos modification = buildModification();
        modification.setDefaultOutageRate(15.);
        modification.setGeneratorsWithoutOutage(getGeneratorsFiltersInfosWithFilters123());
        modification.setGeneratorsFrequencyReserve(getGeneratorsFrequencyReserveInfosWithFilters456());

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate and plannedOutageRate defined for the generators
        setNetwork(Network.read("testGenerationDispatchReduceMaxP.xiidm", getClass().getResourceAsStream("/testGenerationDispatchReduceMaxP.xiidm")));
        getNetwork().getGenerator("GH1").setMinP(20.);  // to test scaling parameter allowsGeneratorOutOfActivePowerLimits

        List<FilterEquipments> filtersForPmaxReduction = getGeneratorsWithoutOutageFilters123();
        when(filterService.exportFilters(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3), getNetwork())).thenReturn(filtersForPmaxReduction.stream());

        List<FilterEquipments> filtersForFrequencyReserve = getGeneratorsFrequencyReserveFilters45();
        when(filterService.exportFilters(List.of(FILTER_ID_4, FILTER_ID_5), getNetwork())).thenReturn(filtersForFrequencyReserve.stream());
        when(filterService.exportFilters(List.of(FILTER_ID_6), getNetwork())).thenReturn(getGeneratorsFrequencyReserveFilter6().stream());

        List<AbstractFilter> filters = List.of(
            new IdentifierListFilter(FILTER_ID_1, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_2, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_3, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_4, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_5, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_6, new Date(), EquipmentType.GENERATOR, List.of()));
        when(filterService.getFilters(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5, FILTER_ID_6))).thenReturn(filters);

        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modif.apply(getNetwork(), report);

        assertEquals(74.82, getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(59.5, getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(126.1, getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(74.205, getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(145.5, getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(40.375, getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(69.58, getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on synchronous components
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "network.modification.TotalDemand", report);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 177.9 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", report);

        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", report);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 330.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", report);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", report);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in WEST region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "network.modification.SumGeneratorActivePower", report);
    }

    @Test
    void testGenerationDispatchWithSubstationsHierarchy() throws Exception {
        GenerationDispatchInfos modification = buildModification();
        modification.setLossCoefficient(10.);
        modification.setDefaultOutageRate(20.);
        modification.setSubstationsGeneratorsOrdering(List.of(
            SubstationsGeneratorsOrderingInfos.builder().substationIds(List.of("S5", "S4", "S54", "S15", "S74")).build(),
            SubstationsGeneratorsOrderingInfos.builder().substationIds(List.of("S27")).build(),
            SubstationsGeneratorsOrderingInfos.builder().substationIds(List.of("S113", "S74")).build()));

        // network
        setNetwork(Network.read("ieee118cdf_testDemGroupe.xiidm", getClass().getResourceAsStream("/ieee118cdf_testDemGroupe.xiidm")));
        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        modif.apply(getNetwork());

        // generators modified
        assertEquals(264, getNetwork().getGenerator("B4-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B8-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B15-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B19-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B24-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B25-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B27-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B40-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B42-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B46-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B49-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B54-G").getTargetP(), 0.001);
        assertEquals(74.8, getNetwork().getGenerator("B62-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B74-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B113-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("Group3").getTargetP(), 0.001);

        // other generators set to 0.
        assertEquals(0, getNetwork().getGenerator("B1-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B6-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B10-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B12-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B18-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B26-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B31-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B32-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B34-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B36-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B55-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B56-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B59-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B61-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B65-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B66-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B69-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B70-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B72-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B73-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B76-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B77-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B80-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B85-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B87-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B89-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B90-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B91-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B92-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B99-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B100-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B103-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B104-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B105-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B107-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B110-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B111-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B112-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B116-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("Group1").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("Group2").getTargetP(), 0.001);
    }

    @Test
    void testGenerationDispatchWithSubstationsHierarchyAndFixedSupply() {
        // Prepare modification parameters
        GenerationDispatchInfos modification = buildModification();
        modification.setLossCoefficient(10.);
        modification.setDefaultOutageRate(20.);
        modification.setGeneratorsWithFixedSupply(List.of(
                        GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("fixedGroups").build()));
        modification.setSubstationsGeneratorsOrdering(List.of(
                        SubstationsGeneratorsOrderingInfos.builder().substationIds(List.of("S5", "S4", "S54", "S15", "S74")).build(),
                        SubstationsGeneratorsOrderingInfos.builder().substationIds(List.of("S27")).build(),
                        SubstationsGeneratorsOrderingInfos.builder().substationIds(List.of("S113", "S74")).build()));

        // Load the test network
        setNetwork(Network.read("ieee118cdf_testDemGroupe_avecPimposee.xiidm",
                getClass().getResourceAsStream("/ieee118cdf_testDemGroupe_avecPimposee.xiidm")));

        List<FilterEquipments> filtersForFixedSupply = List.of(
                new FilterEquipments(
                        FILTER_ID_1,
                        List.of(
                                getIdentifiableAttributes("B19-G"),
                                getIdentifiableAttributes("B54-G")
                        ),
                        List.of()
                )
        );
        when(filterService.exportFilters(List.of(FILTER_ID_1), getNetwork())).thenReturn(filtersForFixedSupply.stream());

        List<AbstractFilter> filters = List.of(new IdentifierListFilter(FILTER_ID_1, new Date(), EquipmentType.GENERATOR, List.of()));
        when(filterService.getFilters(List.of(FILTER_ID_1))).thenReturn(filters);

        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        modif.apply(getNetwork());

        // Check expected target active power values
        Map<String, Double> expectedTargetPs = Map.ofEntries(
                Map.entry("B19-G", 27.0),
                Map.entry("B54-G", 27.0),
                Map.entry("B24-G", 264.0),
                Map.entry("B25-G", 264.0),
                Map.entry("B40-G", 264.0),
                Map.entry("B42-G", 264.0),
                Map.entry("B46-G", 264.0),
                Map.entry("B49-G", 264.0),
                Map.entry("B15-G", 264.0),
                Map.entry("B27-G", 264.0),
                Map.entry("B113-G", 264.0),
                Map.entry("B4-G", 264.0),
                Map.entry("B74-G", 264.0),
                Map.entry("Group3", 264.0),
                Map.entry("B8-G", 264.0),
                Map.entry("B62-G", 264.0),
                Map.entry("Group2", 264.0),
                Map.entry("Group1", 20.8)
        );

        // Assert each expected targetP
        expectedTargetPs.forEach((genId, expectedValue) ->
                assertEquals(expectedValue, getNetwork().getGenerator(genId).getTargetP(), 0.01,
                        "Unexpected targetP for " + genId));

        // Assert all other generators have targetP = 0
        getNetwork().getGeneratorStream()
                .filter(g -> !expectedTargetPs.containsKey(g.getId()))
                .forEach(g -> assertEquals(0.0, g.getTargetP(), 0.001, "Expected 0 targetP for " + g.getId()));
    }

    @Test
    void testGenerationDispatchErrorCheck() {
        final Network network = Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm"));
        setNetwork(network);

        GenerationDispatchInfos modification = GenerationDispatchInfos.builder().lossCoefficient(150.).defaultOutageRate(0.).build();
        final GenerationDispatch generationDispatch1 = new GenerationDispatch(modification);
        assertThrows(NetworkModificationRunException.class, () -> generationDispatch1.check(network), "GENERATION_DISPATCH_ERROR : The loss coefficient must be between 0 and 100");

        modification = GenerationDispatchInfos.builder().lossCoefficient(20.).defaultOutageRate(140.).build();
        final GenerationDispatch generationDispatch2 = new GenerationDispatch(modification);
        assertThrows(NetworkModificationRunException.class, () -> generationDispatch2.check(network), "GENERATION_DISPATCH_ERROR : The default outage rate must be between 0 and 100");
    }

    @Test
    void testGenerationDispatchWithMaxValueLessThanMinP() throws Exception {
        GenerationDispatchInfos modification = GenerationDispatchInfos.builder()
                .lossCoefficient(20.)
                .defaultOutageRate(15.)
                .generatorsWithoutOutage(getGeneratorsFiltersInfosWithFilters123())
                .generatorsWithFixedSupply(List.of())
                .generatorsFrequencyReserve(getGeneratorsFrequencyReserveInfosWithFilters456())
                .substationsGeneratorsOrdering(List.of())
                .build();

        // dedicated case
        setNetwork(Network.read("fourSubstations_abattementIndispo_modifPmin.xiidm", getClass().getResourceAsStream("/fourSubstations_abattementIndispo_modifPmin.xiidm")));

        // Stub filters queries
        when(filterService.exportFilters(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3), getNetwork())).thenReturn(getGeneratorsWithoutOutageFilters123().stream());
        when(filterService.exportFilters(List.of(FILTER_ID_4, FILTER_ID_5), getNetwork())).thenReturn(getGeneratorsFrequencyReserveFilters45().stream());
        when(filterService.exportFilters(List.of(FILTER_ID_6), getNetwork())).thenReturn(getGeneratorsFrequencyReserveFilter6().stream());

        List<AbstractFilter> filters = List.of(
            new IdentifierListFilter(FILTER_ID_1, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_2, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_3, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_4, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_5, new Date(), EquipmentType.GENERATOR, List.of()),
            new IdentifierListFilter(FILTER_ID_6, new Date(), EquipmentType.GENERATOR, List.of()));
        when(filterService.getFilters(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5, FILTER_ID_6))).thenReturn(filters);

        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modif.apply(getNetwork(), report);

        // check logs
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "network.modification.TotalDemand", report);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogNthMessage("The active power set point of generator TEST1 has been set to 40.4 MW", "network.modification.GeneratorSetTargetP", report, 1);
        assertLogNthMessage("The active power set point of generator GTH1 has been set to 80.0 MW", "network.modification.GeneratorSetTargetP", report, 2);
        assertLogNthMessage("The active power set point of generator GTH2 has been set to 146.0 MW", "network.modification.GeneratorSetTargetP", report, 3);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 171.6 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", report);
        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", report);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", report);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", report);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 330.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", report);
        assertLogMessageWithoutRank("The active power set point of generator GH1 has been set to 80.0 MW", "network.modification.GeneratorSetTargetP", report);
        assertLogMessageWithoutRank("The active power set point of generator GH2 has been set to 60.0 MW", "network.modification.GeneratorSetTargetP", report);
        assertLogMessageWithoutRank("The active power set point of generator GH3 has been set to 126.1 MW", "network.modification.GeneratorSetTargetP", report);
        assertLogMessageWithoutRank("The active power set point of generator ABC has been set to 63.9 MW", "network.modification.GeneratorSetTargetP", report);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", report);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", report);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in NORTH region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "network.modification.SumGeneratorActivePower", report);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm"));
    }

    @Override
    protected GenerationDispatchInfos buildModification() {
        return GenerationDispatchInfos.builder()
            .stashed(false)
            .lossCoefficient(20.)
            .defaultOutageRate(0.)
            .generatorsWithoutOutage(List.of())
            .generatorsWithFixedSupply(List.of())
            .generatorsFrequencyReserve(List.of())
            .substationsGeneratorsOrdering(List.of())
            .build();
    }

    private void assertNetworkAfterCreationWithStandardLossCoefficient() {
        assertEquals(100., getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(70., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(30., getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNetworkAfterCreationWithStandardLossCoefficient();
    }

    @Override
    protected void checkModification() {
        GenerationDispatchInfos modification = buildModification();
        modification.setLossCoefficient(150.);
        NetworkModificationRunException e = assertThrows(NetworkModificationRunException.class, () -> modification.toModification().check(getNetwork()));
        assertEquals("GENERATION_DISPATCH_ERROR : The loss coefficient must be between 0 and 100", e.getMessage());

        modification.setLossCoefficient(20.);
        modification.setDefaultOutageRate(140.);
        e = assertThrows(NetworkModificationRunException.class, () -> modification.toModification().check(getNetwork()));
        assertEquals("GENERATION_DISPATCH_ERROR : The default outage rate must be between 0 and 100", e.getMessage());
    }

    @Test
    void testGenerationDispatchWithMissingFilters() {
        GenerationDispatchInfos modification = buildModification();
        modification.setDefaultOutageRate(15.);
        modification.setGeneratorsWithoutOutage(
            List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build()));
        modification.setGeneratorsWithFixedSupply(
            List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build()));
        modification.setGeneratorsFrequencyReserve(List.of(GeneratorsFrequencyReserveInfos.builder().frequencyReserve(3.)
                .generatorsFilters(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_4).name("filter4").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_5).name("filter5").build())).build(),
            GeneratorsFrequencyReserveInfos.builder().frequencyReserve(5.)
                .generatorsFilters(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_6).name("filter6").build())).build()));

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate and plannedOutageRate defined for the generators
        setNetwork(Network.read("testGenerationDispatchReduceMaxP.xiidm", getClass().getResourceAsStream("/testGenerationDispatchReduceMaxP.xiidm")));

        GenerationDispatch modif = (GenerationDispatch) modification.toModification();
        modif.initApplicationContext(filterService, null);
        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test").build());
        modif.apply(getNetwork(), report);
        assertLogMessage("The modification points to at least 6 filters that does not exist anymore", "network.modification.missingFiltersInGenerationDispatch", report);
    }
}
