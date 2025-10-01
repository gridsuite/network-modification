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
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularBatteryModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                BatteryModificationInfos.builder().equipmentId("v1Battery").maxP(new AttributeModification<>(50., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v2Battery").minP(new AttributeModification<>(5., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v3Battery").targetP(new AttributeModification<>(5., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("unknownBattery").targetQ(new AttributeModification<>(500., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.BATTERY_MODIFICATION)
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
        assertEquals(50., getNetwork().getBattery("v1Battery").getMaxP(), 0.001);
        assertEquals(5., getNetwork().getBattery("v2Battery").getMinP(), 0.001);
        assertEquals(5., getNetwork().getBattery("v3Battery").getTargetP(), 0.001);
    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogNthMessage("Modification of v1Battery", "network.modification.tabular.modification.equipmentId", reportNode, 1);
        assertLogNthMessage("Modification of v2Battery", "network.modification.tabular.modification.equipmentId", reportNode, 2);
        assertLogNthMessage("Modification of v3Battery", "network.modification.tabular.modification.equipmentId", reportNode, 3);
        assertLogMessageWithoutRank("Tabular modification: 3 batteries have been modified and 1 have not been modified", "network.modification.tabular.modification.warning", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.BATTERY_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void checkModification() {
        // TODO
    }
}
