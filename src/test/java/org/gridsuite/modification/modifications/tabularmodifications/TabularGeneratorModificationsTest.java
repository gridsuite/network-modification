/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.gridsuite.modification.utils.TestUtils.assertLogNthMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularGeneratorModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("unknownGenerator").maxP(new AttributeModification<>(500., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Test
    @Override
    public void testApply() {
        ModificationInfos modificationInfos = buildModification();
        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);
        assertAfterNetworkModificationApplication(reportNode);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(500., getNetwork().getGenerator("idGenerator").getMaxP(), 0.001);
        assertEquals(500., getNetwork().getGenerator("v5generator").getMaxP(), 0.001);
        assertEquals(500., getNetwork().getGenerator("v6generator").getMaxP(), 0.001);
    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogNthMessage("Modification of idGenerator", "network.modification.tabular.modification.equipmentId", reportNode, 1);
        assertLogNthMessage("Modification of v5generator", "network.modification.tabular.modification.equipmentId", reportNode, 2);
        assertLogNthMessage("Modification of v6generator", "network.modification.tabular.modification.equipmentId", reportNode, 3);
        assertLogMessageWithoutRank("Tabular modification: 3 generators have been modified and 1 have not been modified", "network.modification.tabular.modification.partial", reportNode);
    }

    @Test
    void testAllModificationsHaveFailed() {
        List<ModificationInfos> modifications = List.of(
            GeneratorModificationInfos.builder().equipmentId("idGenerator").maxP(new AttributeModification<>(-500., OperationType.SET)).build(),
            GeneratorModificationInfos.builder().equipmentId("v5Generator").maxP(new AttributeModification<>(-200., OperationType.SET)).build(),
            GeneratorModificationInfos.builder().equipmentId("unknownGenerator").maxP(new AttributeModification<>(500., OperationType.SET)).build()
        );
        ModificationInfos modificationInfos = TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        assertLogMessage("Tabular modification: No generators have been modified", "network.modification.tabular.modification.error", reportNode);
        assertLogMessage("Modification errors", "network.modification.tabular.modification.error.equipmentError", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.GENERATOR_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void checkModification() {
    }
}
