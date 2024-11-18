/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;

import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class SubstationModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return SubstationModificationInfos.builder()
            .stashed(false)
            .equipmentId("s3")
            .equipmentName(new AttributeModification<>("newName", OperationType.SET))
            .country(new AttributeModification<>(Country.BQ, OperationType.SET))
            .properties(List.of(FreePropertyInfos.builder().name("p1").value("v1").build(), // new
                FreePropertyInfos.builder().name("p2").value("v2").build(), // new
                FreePropertyInfos.builder().name("region").value("south").build(), // update
                FreePropertyInfos.builder().name("tso").value("").deletionMark(true).build()))// deletion
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Substation modifiedStation = getNetwork().getSubstation("s3");
        assertNotNull(modifiedStation);
        assertEquals("newName", modifiedStation.getOptionalName().orElse(""));
        assertEquals(Country.BQ, modifiedStation.getCountry().orElse(Country.ZW));
        // 2 properties added and 1 updated, 'tso' removed
        assertEquals(Set.of("p1", "p2", "region"), modifiedStation.getPropertyNames());
        assertEquals("v1", modifiedStation.getProperty("p1", ""));
        assertEquals("v2", modifiedStation.getProperty("p2", ""));
        assertEquals("south", modifiedStation.getProperty("region", ""));
    }

    @Override
    protected void checkModification() {
        // Try to modify an unknown substation
        SubstationModificationInfos infos = SubstationModificationInfos.builder()
                .equipmentId("unknown")
                .country(new AttributeModification<>(Country.JP, OperationType.SET))
                .build();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> infos.toModification().check(getNetwork()));
        assertEquals("SUBSTATION_NOT_FOUND : Substation unknown does not exist in network", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SUBSTATION_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("s3", createdValues.get("equipmentId"));
    }
}
