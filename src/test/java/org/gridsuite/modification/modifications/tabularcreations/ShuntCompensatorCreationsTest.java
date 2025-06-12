/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularcreations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
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
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
class ShuntCompensatorCreationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> creations = List.of(
                ShuntCompensatorCreationInfos.builder()
                        .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                        .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                        .sectionCount(1).maximumSectionCount(1)
                        .maxSusceptance(2.)
                        .build(),
                ShuntCompensatorCreationInfos.builder()
                        .equipmentId("id2").equipmentName("name2").voltageLevelId("v2").busOrBusbarSectionId("1A")
                        .connectionName("feederId2").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                        .sectionCount(5).maximumSectionCount(10)
                        .shuntCompensatorType(ShuntCompensatorType.REACTOR).maxQAtNominalV(2.)
                        .build(),
                ShuntCompensatorCreationInfos.builder()
                        .equipmentId("id3").equipmentName("name3").voltageLevelId("v3").busOrBusbarSectionId("3A")
                        .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                        .sectionCount(6).maximumSectionCount(8)
                        .maxSusceptance(88.)
                        .build(),
                ShuntCompensatorCreationInfos.builder()
                        .equipmentId("id4").equipmentName("name4").voltageLevelId("v4").busOrBusbarSectionId("1.A")
                        .connectionName("feederId4").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                        .sectionCount(2).maximumSectionCount(2)
                        .shuntCompensatorType(ShuntCompensatorType.CAPACITOR).maxQAtNominalV(7.)
                        .build(),
                ShuntCompensatorCreationInfos.builder()
                        .equipmentId("id5").equipmentName("name5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                        .connectionName("name5").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                        .sectionCount(15).maximumSectionCount(20)
                        .shuntCompensatorType(ShuntCompensatorType.CAPACITOR).maxQAtNominalV(2.)
                        .build()
        );
        return TabularCreationInfos.builder()
                .creationType(ModificationType.SHUNT_COMPENSATOR_CREATION)
                .creations(creations)
                .stashed(false)
                .build();
    }

    @Test
    void testAllModificationsHaveSucceeded() {
        List<ModificationInfos> creations = List.of(
                ShuntCompensatorCreationInfos.builder()
                        .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                        .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                        .sectionCount(1).maximumSectionCount(1)
                        .shuntCompensatorType(ShuntCompensatorType.CAPACITOR).maxSusceptance(2.)
                        .build()
        );

        ModificationInfos creationInfos = TabularCreationInfos.builder()
                .creationType(ModificationType.SHUNT_COMPENSATOR_CREATION)
                .creations(creations)
                .build();
        ReportNode reportNode = creationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("test").build());
        creationInfos.toModification().apply(getNetwork(), reportNode);
        assertLogMessage("Tabular creation: 1 shunt compensator have been created", "network.modification.tabular.creation", reportNode);
    }

    @Test
    void testAllModificationsHaveFailed() {
        List<ModificationInfos> creations = List.of(
                ShuntCompensatorCreationInfos.builder()
                        .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                        .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                        .sectionCount(15).maximumSectionCount(1)
                        .shuntCompensatorType(ShuntCompensatorType.CAPACITOR).maxSusceptance(2.)
                        .build()
        );
        ModificationInfos creationInfos = TabularCreationInfos.builder()
                .creationType(ModificationType.SHUNT_COMPENSATOR_CREATION)
                .creations(creations)
                .build();
        ReportNode reportNode = creationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("test").build());
        creationInfos.toModification().apply(getNetwork(), reportNode);
        assertLogMessage("Tabular creation: No shunt compensator have been created", "network.modification.tabular.creation.error", reportNode);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getShuntCompensator("id1"));
        assertNotNull(getNetwork().getShuntCompensator("id2"));
        assertNotNull(getNetwork().getShuntCompensator("id3"));
        assertNotNull(getNetwork().getShuntCompensator("id4"));
        assertNotNull(getNetwork().getShuntCompensator("id5"));
    }

    @Override
    protected void checkModification() {
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() {
        });
        assertEquals(ModificationType.SHUNT_COMPENSATOR_CREATION.name(), createdValues.get("tabularCreationType"));
    }

}
