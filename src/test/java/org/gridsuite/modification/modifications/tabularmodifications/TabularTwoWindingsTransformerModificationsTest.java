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

import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.gridsuite.modification.utils.TestUtils.assertLogNthMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularTwoWindingsTransformerModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                buildOneModification("trf1", 0.0),
                buildOneModification("trf2", 1.0),
                buildOneModification("unknownTwt", 1.0)
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    protected TwoWindingsTransformerModificationInfos buildOneModification(String equipmentId, Double seriesResistance) {
        return TwoWindingsTransformerModificationInfos.builder().equipmentId(equipmentId)
                .r(new AttributeModification<>(seriesResistance, OperationType.SET))
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
        assertEquals(0.0, getNetwork().getTwoWindingsTransformer("trf1").getR(), 0.001);
        assertEquals(1.0, getNetwork().getTwoWindingsTransformer("trf2").getR(), 0.001);
    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogNthMessage("Modification of trf1", "network.modification.tabular.modification.equipmentId", reportNode, 1);
        assertLogNthMessage("Modification of trf2", "network.modification.tabular.modification.equipmentId", reportNode, 2);
        assertLogMessageWithoutRank("Tabular modification: 2 two windings transformers have been modified and 1 have not been modified", "network.modification.tabular.modification.partial", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void checkModification() {
    }
}
