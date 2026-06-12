/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.model.EquipmentDeletionModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class VoltageLevelDeletionTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationModel buildModification() {
        return EquipmentDeletionModel.builder()
            .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
            .equipmentId("v1")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNull(getNetwork().getVoltageLevel("v1"));
        assertNull(getNetwork().getLoad("v1load"));
        assertNull(getNetwork().getLccConverterStation("v1lcc"));
        assertNull(getNetwork().getSwitch("v1d1"));
        assertNull(getNetwork().getLine("line2"));
        assertNull(getNetwork().getHvdcLine("hvdcLine"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        // assertEquals("EQUIPMENT_DELETION", modificationModel.getMessageType());
        // Map<String, String> createdValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals("v1", createdValues.get("equipmentId"));
    }

    @Override
    protected void checkModification() {
    }
}
