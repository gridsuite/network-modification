/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularcreations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ReactiveLimitsKind;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.BatteryCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.dto.tabular.TabularCreationInfos;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
class TabularBatteryCreationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> creations = List.of(
            BatteryCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .minP(0).maxP(100)
                .targetP(50).targetQ(20D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v2").busOrBusbarSectionId("1A")
                .connectionName("feederId2").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .minP(0).maxP(500)
                .targetP(300).targetQ(40D)
                .minQ(7D).maxQ(100D).participate(false)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id3").voltageLevelId("v3").busOrBusbarSectionId("3A")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .minP(0).maxP(200)
                .targetP(150).targetQ(400D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id4").equipmentName("name4").voltageLevelId("v4").busOrBusbarSectionId("1.A")
                .connectionName("feederId4").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .minP(0).maxP(800)
                .targetP(700).targetQ(20D)
                .participate(false)
                .reactiveCapabilityCurve(true).reactiveCapabilityCurvePoints(List.of(ReactiveCapabilityCurvePointsInfos.builder().p(1.).minQ(2.).maxQ(3.).build(), ReactiveCapabilityCurvePointsInfos.builder().p(5.).minQ(6.).maxQ(7.).build(), ReactiveCapabilityCurvePointsInfos.builder().p(9.).minQ(10.).maxQ(11.).build()))
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("name5").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .minP(0).maxP(200).targetQ(400D)
                .targetP(150)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id6").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("v6battery").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .minP(0).maxP(200).targetQ(400D)
                .targetP(150)
                .reactiveCapabilityCurve(true).reactiveCapabilityCurvePoints(List.of(ReactiveCapabilityCurvePointsInfos.builder().p(1.).minQ(2.).maxQ(3.).build(), ReactiveCapabilityCurvePointsInfos.builder().p(5.).minQ(6.).maxQ(7.).build(), ReactiveCapabilityCurvePointsInfos.builder().p(9.).minQ(10.).maxQ(11.).build()))
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id7").voltageLevelId("v6").busOrBusbarSectionId("1B1")
                .connectionName("v5battery").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .minP(0).maxP(200).targetQ(400D)
                .targetP(150)
                .minQ(1.).maxQ(100.)
                .reactiveCapabilityCurve(true).reactiveCapabilityCurvePoints(List.of(ReactiveCapabilityCurvePointsInfos.builder().p(1.).minQ(2.).maxQ(3.).build(), ReactiveCapabilityCurvePointsInfos.builder().p(5.).minQ(6.).maxQ(7.).build(), ReactiveCapabilityCurvePointsInfos.builder().p(9.).minQ(10.).maxQ(11.).build()))
                .build()
        );
        return TabularCreationInfos.builder()
            .modificationType(ModificationType.BATTERY_CREATION)
            .modifications(creations)
            .stashed(false)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getBattery("id1"));
        assertNotNull(getNetwork().getBattery("id2"));
        assertNotNull(getNetwork().getBattery("id3"));
        assertNotNull(getNetwork().getBattery("id4"));
        assertNotNull(getNetwork().getBattery("id5"));
        assertNotNull(getNetwork().getBattery("id6"));

        // If reactiveCapabilityCurve is enabled while minQ and maxQ are set, reactiveCapabilityCurvePoints takes the priority
        Battery id7 = getNetwork().getBattery("id7");
        assertEquals(ReactiveLimitsKind.CURVE, id7.getReactiveLimits().getKind());

        // If reactiveCapabilityCurve isn't enabled, minQ and maxQ are set
        Battery id2 = getNetwork().getBattery("id2");
        assertEquals(ReactiveLimitsKind.MIN_MAX, id2.getReactiveLimits().getKind());

    }

    @Test
    void testAllModificationsHaveSucceeded() {
        List<ModificationInfos> creations = List.of(
            BatteryCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .minP(0).maxP(100)
                .targetP(50).targetQ(20D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build()
        );

        ModificationInfos creationInfos = TabularCreationInfos.builder()
            .modificationType(ModificationType.BATTERY_CREATION)
            .modifications(creations)
            .build();
        ReportNode reportNode = creationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        creationInfos.toModification().apply(getNetwork(), reportNode);
        assertLogMessage("Tabular creation: 1 battery have been created", "network.modification.tabular.creation", reportNode);
        assertLogMessage("Creation of id1", "network.modification.tabular.creation.equipmentId", reportNode);
    }

    @Test
    void testAllModificationsHaveFailed() {
        List<ModificationInfos> creations = List.of(
            BatteryCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("unknown_vl").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .minP(0).maxP(100)
                .targetP(50).targetQ(20D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .reactiveCapabilityCurve(false)
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v1").busOrBusbarSectionId("unknown_bbs")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .minP(0).maxP(100)
                .targetP(50).targetQ(20D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .reactiveCapabilityCurve(false)
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id3").equipmentName("name3").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .minP(0).maxP(-100)
                .targetP(50).targetQ(20D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .reactiveCapabilityCurve(false)
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id4").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("v4battery").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .minP(0).maxP(200)
                .targetP(150)
                .reactiveCapabilityCurve(true).reactiveCapabilityCurvePoints(List.of(ReactiveCapabilityCurvePointsInfos.builder().maxQ(3.).build(), ReactiveCapabilityCurvePointsInfos.builder().maxQ(3.).build(), ReactiveCapabilityCurvePointsInfos.builder().maxQ(3.).build()))
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("v5battery").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .minP(0).maxP(200)
                .targetP(150)
                .reactiveCapabilityCurve(true).reactiveCapabilityCurvePoints(List.of(ReactiveCapabilityCurvePointsInfos.builder().p(3.).build(), ReactiveCapabilityCurvePointsInfos.builder().p(3.).build(), ReactiveCapabilityCurvePointsInfos.builder().p(3.).build()))
                .build(),
            BatteryCreationInfos.builder()
                .equipmentId("id6").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("v6battery").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .minP(0).maxP(200)
                .targetP(150)
                .reactiveCapabilityCurve(true).reactiveCapabilityCurvePoints(List.of(ReactiveCapabilityCurvePointsInfos.builder().minQ(1.).p(3.).build(), ReactiveCapabilityCurvePointsInfos.builder().minQ(1.).p(3.).build(), ReactiveCapabilityCurvePointsInfos.builder().minQ(1.).p(3.).build()))
                .build()
        );
        ModificationInfos creationInfos = TabularCreationInfos.builder()
                .modificationType(ModificationType.BATTERY_CREATION)
                .modifications(creations)
                .build();
        ReportNode reportNode = creationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        creationInfos.toModification().apply(getNetwork(), reportNode);
        assertLogMessage("Tabular creation: No batteries have been created", "network.modification.tabular.creation.error", reportNode);
        assertLogMessage("Creation errors", "network.modification.tabular.creation.error.equipmentError", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.BATTERY_CREATION.name(), createdValues.get("tabularCreationType"));
    }

    @Override
    protected void checkModification() {
    }
}
