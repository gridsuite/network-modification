/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularmodifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.AttributeModification;
import org.gridsuite.modification.model.LoadModificationModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.OperationType;
import org.gridsuite.modification.model.tabular.TabularModificationModel;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.gridsuite.modification.utils.TestUtils.assertLogNthMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularLoadModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationModel buildModification() {
        List<ModificationModel> modifications = List.of(
            LoadModificationModel.builder().equipmentId("v1load").q0(new AttributeModification<>(300., OperationType.SET)).build(),
            LoadModificationModel.builder().equipmentId("v2load").q0(new AttributeModification<>(300., OperationType.SET)).build(),
            LoadModificationModel.builder().equipmentId("v3load").q0(new AttributeModification<>(300., OperationType.SET)).build()
        );
        return TabularModificationModel.builder()
            .modificationType(ModificationType.LOAD_MODIFICATION)
            .modifications(modifications)
            .build();
    }

    @Test
    @Override
    public void testApply() {
        ModificationModel modificationModel = buildModification();
        ReportNode reportNode = modificationModel.createSubReportNode(ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test").build());
        modificationModel.toModification().apply(getNetwork(), reportNode);
        assertAfterNetworkModificationApplication(reportNode);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(300., getNetwork().getLoad("v1load").getQ0(), 0.001);
        assertEquals(300., getNetwork().getLoad("v2load").getQ0(), 0.001);
        assertEquals(300., getNetwork().getLoad("v3load").getQ0(), 0.001);
    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogNthMessage("Modification of v1load", "network.modification.tabular.modification.equipmentId", reportNode, 1);
        assertLogNthMessage("Modification of v2load", "network.modification.tabular.modification.equipmentId", reportNode, 2);
        assertLogNthMessage("Modification of v3load", "network.modification.tabular.modification.equipmentId", reportNode, 3);
        assertLogMessageWithoutRank("Tabular modification: 3 loads have been modified", "network.modification.tabular.modification", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        // assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationModel.getMessageType());
        // Map<String, String> createdValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals(ModificationType.LOAD_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void checkModification() {
    }
}
