/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Anis Touri <anis.touri at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularLineModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                LineModificationInfos.builder().equipmentId("line1").r(new AttributeModification<>(10., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line2").x(new AttributeModification<>(20., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").g1(new AttributeModification<>(30., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").b1(new AttributeModification<>(40., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("unknownLine").b2(new AttributeModification<>(60., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(10., getNetwork().getLine("line1").getR(), 0.001);
        assertEquals(20., getNetwork().getLine("line2").getX(), 0.001);
        assertEquals(30., getNetwork().getLine("line3").getG1(), 0.001);
        assertEquals(40., getNetwork().getLine("line3").getB1(), 0.001);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.LINE_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void checkModification() {
    }
}
