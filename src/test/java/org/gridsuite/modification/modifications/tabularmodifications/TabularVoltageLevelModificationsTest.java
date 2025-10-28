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
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.gridsuite.modification.utils.TestUtils.assertLogNthMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author AJELLAL Ali <ali.ajellal@rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularVoltageLevelModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                VoltageLevelModificationInfos.builder().equipmentId("v1").nominalV(new AttributeModification<>(300., OperationType.SET)).highVoltageLimit(new AttributeModification<>(400., OperationType.SET)).lowVoltageLimit(new AttributeModification<>(299., OperationType.SET)).build(),
                VoltageLevelModificationInfos.builder().equipmentId("v2").nominalV(new AttributeModification<>(300., OperationType.SET)).highVoltageLimit(new AttributeModification<>(400., OperationType.SET)).lowVoltageLimit(new AttributeModification<>(299., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.VOLTAGE_LEVEL_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Test
    @Override
    public void testApply() {
        ModificationInfos modificationInfos = buildModification();
        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);
        assertAfterNetworkModificationApplication(reportNode);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(300., getNetwork().getVoltageLevel("v1").getNominalV(), 0.001);
        assertEquals(299., getNetwork().getVoltageLevel("v1").getLowVoltageLimit(), 0.001);
        assertEquals(400., getNetwork().getVoltageLevel("v1").getHighVoltageLimit(), 0.001);
        assertEquals(300., getNetwork().getVoltageLevel("v2").getNominalV(), 0.001);
        assertEquals(299., getNetwork().getVoltageLevel("v2").getLowVoltageLimit(), 0.001);
        assertEquals(400., getNetwork().getVoltageLevel("v2").getHighVoltageLimit(), 0.001);
    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogNthMessage("Modification of v1", "network.modification.tabular.modification.equipmentId", reportNode, 1);
        assertLogNthMessage("Modification of v2", "network.modification.tabular.modification.equipmentId", reportNode, 2);
        assertLogMessageWithoutRank("Tabular modification: 2 voltage levels have been modified", "network.modification.tabular.modification", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.VOLTAGE_LEVEL_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void checkModification() {
    }
}
