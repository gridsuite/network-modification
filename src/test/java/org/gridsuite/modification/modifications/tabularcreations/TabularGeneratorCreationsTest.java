/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularcreations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.GeneratorCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.TabularCreationInfos;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularGeneratorCreationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v2").busOrBusbarSectionId("1A")
                .connectionName("feederId2").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .energySource(EnergySource.NUCLEAR).minP(0).maxP(500)
                .targetP(300).targetQ(400D).voltageRegulationOn(false)
                .plannedActivePowerSetPoint(200D).forcedOutageRate(3D)
                .minQ(7D).participate(false)
                .stepUpTransformerX(45D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id3").voltageLevelId("v3").busOrBusbarSectionId("3A")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .energySource(EnergySource.WIND).minP(0).maxP(200)
                .targetP(150).voltageRegulationOn(true).targetV(375D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id4").equipmentName("name4").voltageLevelId("v4").busOrBusbarSectionId("1.A")
                .connectionName("feederId4").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .energySource(EnergySource.OTHER).minP(0).maxP(800)
                .targetP(700).targetQ(20D).voltageRegulationOn(true).targetV(373D)
                .marginalCost(5D).plannedOutageRate(8D)
                .participate(false)
                .directTransX(5D)
                .regulatingTerminalId("v5load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v5").qPercent(75D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("name5").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .energySource(EnergySource.WIND).minP(0).maxP(200)
                .targetP(150).voltageRegulationOn(true).targetV(375D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("v5generator").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("v5generator").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .energySource(EnergySource.WIND).minP(0).maxP(200)
                .targetP(150).voltageRegulationOn(true).targetV(375D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build()
        );
        return TabularCreationInfos.builder()
            .creationType(ModificationType.GENERATOR_CREATION)
            .creations(creations)
            .stashed(false)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getGenerator("id1"));
        assertNotNull(getNetwork().getGenerator("id2"));
        assertNotNull(getNetwork().getGenerator("id3"));
        assertNotNull(getNetwork().getGenerator("id4"));
        assertNotNull(getNetwork().getGenerator("id5"));
    }

    @Test
    void testAllModificationsHaveSucceeded() throws Exception {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v2").busOrBusbarSectionId("1A")
                .connectionName("feederId2").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .energySource(EnergySource.NUCLEAR).minP(0).maxP(500)
                .targetP(300).targetQ(400D).voltageRegulationOn(false)
                .plannedActivePowerSetPoint(200D).forcedOutageRate(3D)
                .minQ(7D).participate(false)
                .stepUpTransformerX(45D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build()
        );

        ModificationInfos creationInfos = TabularCreationInfos.builder()
            .creationType(ModificationType.GENERATOR_CREATION)
            .creations(creations)
            .build();
        ReportNode reportNode = creationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("test").build());
        creationInfos.toModification().apply(getNetwork(), reportNode);
        assertLogMessage("Tabular creation: 2 generators have been created", "network.modification.tabular.creation", reportNode);
    }

    @Test
    void testAllModificationsHaveFailed() throws Exception {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("unknown_vl").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v1").busOrBusbarSectionId("unknown_bbs")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id3").equipmentName("name3").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(-100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build()
        );
        ModificationInfos creationInfos = TabularCreationInfos.builder()
                .creationType(ModificationType.GENERATOR_CREATION)
                .creations(creations)
                .build();
        ReportNode reportNode = creationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("test").build());
        creationInfos.toModification().apply(getNetwork(), reportNode);
        assertLogMessage("Tabular creation: No generators have been created", "network.modification.tabular.creation.error", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.GENERATOR_CREATION.name(), createdValues.get("tabularCreationType"));
    }

    @Override
    protected void checkModification() {
    }
}
