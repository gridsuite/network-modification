/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Country;
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

import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.gridsuite.modification.utils.TestUtils.assertLogNthMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author AJELLAL Ali <ali.ajellal@rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularSubstationModificationsTest extends AbstractNetworkModificationTest {
    public static final ModificationType MOFIFICATION_TYPE = ModificationType.SUBSTATION_MODIFICATION;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {

        List<ModificationInfos> modifications = List.of(
                SubstationModificationInfos.builder().equipmentId("s1").equipmentName(new AttributeModification<>("s1", OperationType.SET)).country(new AttributeModification<>(Country.BE, OperationType.SET)).build(),
                SubstationModificationInfos.builder().equipmentId("s2").equipmentName(new AttributeModification<>("s2", OperationType.SET)).country(new AttributeModification<>(Country.BE, OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(MOFIFICATION_TYPE)
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
        assertEquals(Country.BE, getNetwork().getSubstation("s1").getCountry().orElse(Country.AF));
        assertEquals("s1", getNetwork().getSubstation("s1").getOptionalName().orElse("s2"));
        assertEquals(Country.BE, getNetwork().getSubstation("s2").getCountry().orElse(Country.AF));
        assertEquals("s2", getNetwork().getSubstation("s2").getOptionalName().orElse("s1"));
    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogNthMessage("Modification of s1", "network.modification.tabular.modification.equipmentId", reportNode, 1);
        assertLogNthMessage("Modification of s2", "network.modification.tabular.modification.equipmentId", reportNode, 2);
        assertLogMessageWithoutRank("Tabular modification: 2 substations have been modified", "network.modification.tabular.modification", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(MOFIFICATION_TYPE.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void checkModification() {
    }
}
