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
class ShuntCompensatorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorCreationInfos.builder()
                .stashed(false)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                .equipmentId("shuntOneId")
                .equipmentName("hop")
                .maximumSectionCount(10)
                .sectionCount(6)
                .maxSusceptance(0.)
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .connectionName("cn")
                .connectionPosition(99)
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
    protected void checkModification() {
        ShuntCompensatorCreationInfos modificationToCreate = (ShuntCompensatorCreationInfos) buildModification();
        // try to create an existing equipment
        modificationToCreate.setEquipmentId("v5shunt");
        assertNotNull(getNetwork().getShuntCompensator("v5shunt"));
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> modificationToCreate.toModification().check(getNetwork()));
        assertEquals("SHUNT_COMPENSATOR_ALREADY_EXISTS : v5shunt", exception.getMessage());

        // CreateWithMaximumSectionCountError
        modificationToCreate.setEquipmentId("newShunt");
        modificationToCreate.setMaximumSectionCount(0);
        exception = assertThrows(NetworkModificationException.class, () -> modificationToCreate.toModification().check(getNetwork()));
        assertEquals("CREATE_SHUNT_COMPENSATOR_ERROR : Maximum section count should be greater or equal to 1", exception.getMessage());

        // CreateWithSectionError
        modificationToCreate.setMaximumSectionCount(2);
        modificationToCreate.setSectionCount(3);
        exception = assertThrows(NetworkModificationException.class, () -> modificationToCreate.toModification().check(getNetwork()));
        assertEquals("CREATE_SHUNT_COMPENSATOR_ERROR : Section count should be between 0 and Maximum section count (2), actual : 3", exception.getMessage());

        // CreateWithExistingConnectionPosition
        ShuntCompensatorCreationInfos dto = (ShuntCompensatorCreationInfos) buildModification();
        dto.setConnectionPosition(2);
        exception = assertThrows(NetworkModificationException.class, () -> dto.toModification().check(getNetwork()));
        assertEquals("CONNECTION_POSITION_ERROR : PositionOrder '2' already taken", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SHUNT_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("shuntOneId", updatedValues.get("equipmentId"));
    }
}
