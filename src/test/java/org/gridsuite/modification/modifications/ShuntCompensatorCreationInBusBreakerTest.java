/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class ShuntCompensatorCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected void checkModification() {
        ShuntCompensatorCreationInfos shunt = (ShuntCompensatorCreationInfos) buildModification();
        shunt.setBusOrBusbarSectionId("notFoundBus");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> shunt.toModification().check(getNetwork()));
        assertEquals("BUS_NOT_FOUND : notFoundBus", exception.getMessage());
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorCreationInfos.builder()
            .stashed(false)
            .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
            .equipmentId("shuntOneId")
            .equipmentName("hopOne")
            .maximumSectionCount(10)
            .sectionCount(6)
            .maxSusceptance(0.)
            .voltageLevelId("v2")
            .busOrBusbarSectionId("bus2")
            .connectionName("cn2")
            .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getShuntCompensator("shuntOneId"));
        assertEquals(PROPERTY_VALUE, getNetwork().getShuntCompensator("shuntOneId").getProperty(PROPERTY_NAME));

    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SHUNT_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("shuntOneId", createdValues.get("equipmentId"));
    }
}
