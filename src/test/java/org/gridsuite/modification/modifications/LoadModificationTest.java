/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ValidationException;

import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LoadModificationTest extends AbstractInjectionModificationTest {
    private static String PROPERTY_NAME = "property-name";
    private static String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadModificationInfos.builder()
            .stashed(false)
            .equipmentId("v1load")
            .equipmentName(new AttributeModification<>("nameLoad1", OperationType.SET))
            .loadType(new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET))
            .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
            .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
            .p0(new AttributeModification<>(200.0, OperationType.SET))
            .q0(new AttributeModification<>(30.0, OperationType.SET))
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Load modifiedLoad = getNetwork().getLoad("v1load");
        assertNotNull(modifiedLoad);
        assertEquals(LoadType.FICTITIOUS, modifiedLoad.getLoadType());
        assertEquals(200.0, modifiedLoad.getP0(), 0.0);
        assertEquals(30.0, modifiedLoad.getQ0(), 0.0);
        assertEquals("nameLoad1", modifiedLoad.getNameOrId());
        assertEquals(PROPERTY_VALUE, modifiedLoad.getProperty(PROPERTY_NAME));
    }

    @Override
    protected void checkModification() {
        // Unset an attribute that should not be null
        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
                .stashed(false)
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(null, OperationType.UNSET))
                .build();
        ValidationException exception = assertThrows(ValidationException.class, () -> loadModificationInfos.toModification().apply(getNetwork()));
        assertEquals("Load 'v1load': load type is null", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LOAD_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1load", createdValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        assertChangeConnectionState(getNetwork().getLoad("v1load"), false);
    }

    @Test
    void testConnection() throws Exception {
        assertChangeConnectionState(getNetwork().getLoad("v1load"), true);
    }
}
