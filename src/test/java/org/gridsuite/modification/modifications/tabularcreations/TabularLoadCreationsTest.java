/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularcreations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.LoadCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.TabularCreationInfos;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
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
class TabularLoadCreationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> creations = List.of(
            LoadCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .loadType(LoadType.AUXILIARY).p0(0).q0(100)
                .build(),
            LoadCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v2").busOrBusbarSectionId("1A")
                .connectionName("feederId2").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .loadType(LoadType.FICTITIOUS).p0(0).q0(500)
                .build(),
            LoadCreationInfos.builder()
                .equipmentId("id3").voltageLevelId("v3").busOrBusbarSectionId("3A")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .loadType(LoadType.UNDEFINED).p0(0).q0(200)
                .build(),
            LoadCreationInfos.builder()
                .equipmentId("id4").equipmentName("name4").voltageLevelId("v4").busOrBusbarSectionId("1.A")
                .connectionName("feederId4").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .loadType(LoadType.AUXILIARY).p0(0).q0(800)
                .build(),
            LoadCreationInfos.builder()
                .equipmentId("id5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("name5").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .loadType(LoadType.FICTITIOUS).p0(0).q0(200)
                .build(),
            LoadCreationInfos.builder()
                .equipmentId("v5load").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("v5load").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .loadType(LoadType.UNDEFINED).p0(0).q0(200)
                .build()
        );
        return TabularCreationInfos.builder()
            .creationType(ModificationType.LOAD_CREATION)
            .creations(creations)
            .stashed(false)
            .csvFilename("filename")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getLoad("id1"));
        assertNotNull(getNetwork().getLoad("id2"));
        assertNotNull(getNetwork().getLoad("id3"));
        assertNotNull(getNetwork().getLoad("id4"));
        assertNotNull(getNetwork().getLoad("id5"));
    }

    @Test
    void testAllModificationsHaveSucceeded() {
        List<ModificationInfos> creations = List.of(
            LoadCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .loadType(LoadType.UNDEFINED).p0(0).q0(100)
                .build()
        );

        ModificationInfos creationInfos = TabularCreationInfos.builder()
            .creationType(ModificationType.LOAD_CREATION)
            .creations(creations)
            .build();
        ReportNode reportNode = creationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("test").build());
        creationInfos.toModification().apply(getNetwork(), reportNode);
        assertLogMessage("Tabular creation: 1 load have been created", "network.modification.tabular.creation", reportNode);
        assertLogMessage("Creation of id1", "network.modification.tabular.creation.equipmentId", reportNode);
    }

    @Test
    void testAllModificationsHaveFailed() {
        List<ModificationInfos> creations = List.of(
            LoadCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("unknown_vl").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .loadType(LoadType.UNDEFINED).p0(0).q0(100)
                .build(),
            LoadCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v1").busOrBusbarSectionId("unknown_bbs")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .loadType(LoadType.UNDEFINED).p0(0).q0(100)
                .build(),
            LoadCreationInfos.builder()
                .equipmentId("id3").equipmentName("name3").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .loadType(LoadType.AUXILIARY).p0(Double.NaN).q0(-100)
                .build()
        );
        ModificationInfos creationInfos = TabularCreationInfos.builder()
                .creationType(ModificationType.LOAD_CREATION)
                .creations(creations)
                .build();
        ReportNode reportNode = creationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("test").build());
        creationInfos.toModification().apply(getNetwork(), reportNode);
        assertLogMessage("Tabular creation: No loads have been created", "network.modification.tabular.creation.error", reportNode);
        assertLogMessage("Creation errors", "network.modification.tabular.creation.error.equipmentError", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.LOAD_CREATION.name(), createdValues.get("tabularCreationType"));
    }

    @Override
    protected void checkModification() {
    }
}
