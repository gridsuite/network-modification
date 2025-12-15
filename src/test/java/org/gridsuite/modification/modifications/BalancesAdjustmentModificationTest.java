/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.loadflow.LoadFlowParameters;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.TestUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@ExtendWith(MockitoExtension.class)
class BalancesAdjustmentModificationTest extends AbstractNetworkModificationTest {
    private static final double PRECISION = 1d;
    private static final UUID LOADFLOW_PARAMETERS_UUID = UUID.randomUUID();
    private static final UUID INVALID_LOADFLOW_PARAMETERS_UUID = UUID.randomUUID();

    @Mock
    private ILoadFlowService loadFlowService;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return Network.read("fourSubstationsNb_country 2_N1.xiidm", getClass().getResourceAsStream("/fourSubstationsNb_country 2_N1.xiidm"));
    }

    @Override
    protected BalancesAdjustmentModificationInfos buildModification() {
        return BalancesAdjustmentModificationInfos.builder()
                .areas(List.of(
                        BalancesAdjustmentAreaInfos.builder()
                                .name("FR")
                                .countries(List.of(Country.FR))
                                .netPosition(-45d)
                                .shiftType(ShiftType.PROPORTIONAL)
                                .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                                .build(),
                        BalancesAdjustmentAreaInfos.builder()
                                .name("NE")
                                .countries(List.of(Country.NE))
                                .netPosition(-54d)
                                .shiftType(ShiftType.BALANCED)
                                .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                                .build(),
                        BalancesAdjustmentAreaInfos.builder()
                                .name("GE")
                                .countries(List.of(Country.GE))
                                .netPosition(0d)
                                .shiftType(ShiftType.PROPORTIONAL)
                                .shiftEquipmentType(ShiftEquipmentType.LOAD)
                                .build(),
                        BalancesAdjustmentAreaInfos.builder()
                                .name("AU")
                                .countries(List.of(Country.AU))
                                .netPosition(100d)
                                .shiftType(ShiftType.BALANCED)
                                .shiftEquipmentType(ShiftEquipmentType.LOAD)
                                .build()
                ))
                .withLoadFlow(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(58.d, getNetwork().getGenerator("GH1").getTargetP(), PRECISION);
        assertEquals(36d, getNetwork().getGenerator("GH2").getTargetP(), PRECISION);
        assertEquals(102d, getNetwork().getGenerator("GH3").getTargetP(), PRECISION);
        assertEquals(100d, getNetwork().getGenerator("GTH1").getTargetP(), PRECISION);
        assertEquals(147d, getNetwork().getGenerator("GTH2").getTargetP(), PRECISION);

        assertEquals(80d, getNetwork().getLoad("LD1").getP0(), PRECISION);
        assertEquals(60d, getNetwork().getLoad("LD2").getP0(), PRECISION);
        assertEquals(60d, getNetwork().getLoad("LD3").getP0(), PRECISION);
        assertEquals(40d, getNetwork().getLoad("LD4").getP0(), PRECISION);
        assertEquals(201d, getNetwork().getLoad("LD5").getP0(), PRECISION);
        assertEquals(0d, getNetwork().getLoad("LD6").getP0(), PRECISION);
    }

    @Override
    protected void checkModification() {
        // No checks implemented for this modification
    }

    @Override
    @Test
    public void testApply() throws Exception {
        buildModification().toModification().apply(getNetwork(), new DefaultNamingStrategy(), ReportNode.NO_OP);
        assertAfterNetworkModificationApplication();
    }

    @Test
    void testApplyWithLoadFlow() {
        var infos = BalancesAdjustmentModificationInfos.builder()
            .areas(List.of(
                BalancesAdjustmentAreaInfos.builder()
                    .name("FR")
                    .countries(List.of(Country.FR))
                    .netPosition(-45d)
                    .shiftType(ShiftType.PROPORTIONAL)
                    .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                    .build(),
                BalancesAdjustmentAreaInfos.builder()
                    .name("NE")
                    .countries(List.of(Country.NE))
                    .netPosition(-54d)
                    .shiftType(ShiftType.BALANCED)
                    .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                    .build(),
                BalancesAdjustmentAreaInfos.builder()
                    .name("GE")
                    .countries(List.of(Country.GE))
                    .netPosition(0d)
                    .shiftType(ShiftType.PROPORTIONAL)
                    .shiftEquipmentType(ShiftEquipmentType.LOAD)
                    .build(),
                BalancesAdjustmentAreaInfos.builder()
                    .name("AU")
                    .countries(List.of(Country.AU))
                    .netPosition(100d)
                    .shiftType(ShiftType.BALANCED)
                    .shiftEquipmentType(ShiftEquipmentType.LOAD)
                    .build()
            ))
            .withLoadFlow(true)
            .loadFlowParametersId(LOADFLOW_PARAMETERS_UUID)
            .build();

        when(loadFlowService.getLoadFlowParametersInfos(LOADFLOW_PARAMETERS_UUID))
                .thenReturn(LoadFlowParametersInfos.builder()
                        .provider("OpenLoadFlow")
                        .commonParameters(LoadFlowParameters.load())
                        .specificParametersPerProvider(Map.of("OpenLoadFlow", Map.of(
                                "key1", "value1"
                        )))
                        .build());
        BalancesAdjustmentModification modification = (BalancesAdjustmentModification) infos.toModification();
        modification.initApplicationContext(null, loadFlowService);
        modification.apply(getNetwork(), new DefaultNamingStrategy(), ReportNode.NO_OP);

        assertEquals(-58d, getNetwork().getGenerator("GH1").getTerminal().getP(), PRECISION);
        assertEquals(-36d, getNetwork().getGenerator("GH2").getTerminal().getP(), PRECISION);
        assertEquals(-102d, getNetwork().getGenerator("GH3").getTerminal().getP(), PRECISION);
        assertEquals(-100d, getNetwork().getGenerator("GTH1").getTerminal().getP(), PRECISION);
        assertEquals(-147d, getNetwork().getGenerator("GTH2").getTerminal().getP(), PRECISION);

        assertEquals(80d, getNetwork().getLoad("LD1").getTerminal().getP(), PRECISION);
        assertEquals(60d, getNetwork().getLoad("LD2").getTerminal().getP(), PRECISION);
        assertEquals(60d, getNetwork().getLoad("LD3").getTerminal().getP(), PRECISION);
        assertEquals(40d, getNetwork().getLoad("LD4").getTerminal().getP(), PRECISION);
        assertEquals(201d, getNetwork().getLoad("LD5").getTerminal().getP(), PRECISION);
        assertEquals(0d, getNetwork().getLoad("LD6").getTerminal().getP(), PRECISION);
    }

    @Test
    void testLoadFlowParametersNotFound() {
        var infos = BalancesAdjustmentModificationInfos.builder()
                .areas(List.of(
                        BalancesAdjustmentAreaInfos.builder()
                                .name("FR")
                                .countries(List.of(Country.FR))
                                .netPosition(-45d)
                                .shiftType(ShiftType.PROPORTIONAL)
                                .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                                .build()
                ))
                .withLoadFlow(true)
                .loadFlowParametersId(INVALID_LOADFLOW_PARAMETERS_UUID)
                .build();

        when(loadFlowService.getLoadFlowParametersInfos(INVALID_LOADFLOW_PARAMETERS_UUID))
                .thenReturn(null);

        BalancesAdjustmentModification modification = (BalancesAdjustmentModification) infos.toModification();
        modification.initApplicationContext(null, loadFlowService);

        Network network = getNetwork();
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build();

        // Should not throw exception but use default parameters and complete successfully
        assertDoesNotThrow(() -> modification.apply(network, new DefaultNamingStrategy(), reportNode));

        // Verify that the appropriate report message was generated
        TestUtils.assertLogMessage(
                "Using default load flow parameters: Load flow parameters with id " + INVALID_LOADFLOW_PARAMETERS_UUID + " not found",
                "network.modification.balancesAdjustment.usingDefaultLoadFlowParameters",
                reportNode
        );
    }

    @Test
    void testLoadFlowProviderNotSpecified() {
        var infos = BalancesAdjustmentModificationInfos.builder()
                .areas(List.of(
                        BalancesAdjustmentAreaInfos.builder()
                                .name("FR")
                                .countries(List.of(Country.FR))
                                .netPosition(-45d)
                                .shiftType(ShiftType.PROPORTIONAL)
                                .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                                .build()
                ))
                .withLoadFlow(true)
                .loadFlowParametersId(LOADFLOW_PARAMETERS_UUID)
                .build();

        when(loadFlowService.getLoadFlowParametersInfos(LOADFLOW_PARAMETERS_UUID))
                .thenReturn(LoadFlowParametersInfos.builder()
                        .provider(null) // No provider specified
                        .commonParameters(LoadFlowParameters.load())
                        .specificParametersPerProvider(Map.of())
                        .build());

        BalancesAdjustmentModification modification = (BalancesAdjustmentModification) infos.toModification();
        modification.initApplicationContext(null, loadFlowService);

        Network network = getNetwork();
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build();

        // Should not throw exception but use default parameters and complete successfully
        assertDoesNotThrow(() -> modification.apply(network, new DefaultNamingStrategy(), reportNode));

        // Verify that the appropriate report message was generated
        TestUtils.assertLogMessageWithoutRank(
                "Using default load flow parameters: Load flow provider is null in parameters with id " + LOADFLOW_PARAMETERS_UUID,
                "network.modification.balancesAdjustment.usingDefaultLoadFlowParameters",
                reportNode
        );
    }

    @Test
    void testLoadFlowParametersIdNull() {
        var infos = BalancesAdjustmentModificationInfos.builder()
                .areas(List.of(
                        BalancesAdjustmentAreaInfos.builder()
                                .name("FR")
                                .countries(List.of(Country.FR))
                                .netPosition(-45d)
                                .shiftType(ShiftType.PROPORTIONAL)
                                .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                                .build()
                ))
                .withLoadFlow(true)
                .loadFlowParametersId(null)
                .build();

        BalancesAdjustmentModification modification = (BalancesAdjustmentModification) infos.toModification();
        modification.initApplicationContext(null, loadFlowService);

        Network network = getNetwork();
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build();

        // Should not throw exception but use default parameters and complete successfully
        assertDoesNotThrow(() -> modification.apply(network, new DefaultNamingStrategy(), reportNode));

        // Verify that the appropriate report message was generated
        TestUtils.assertLogMessageWithoutRank(
                "Using default load flow parameters: Load flow parameters ID is null",
                "network.modification.balancesAdjustment.usingDefaultLoadFlowParameters",
                reportNode
        );
    }
}
