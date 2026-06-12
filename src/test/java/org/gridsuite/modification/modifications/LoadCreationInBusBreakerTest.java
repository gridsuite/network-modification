/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.model.FreePropertyModel;
import org.gridsuite.modification.model.LoadCreationModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LoadCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    @SuppressWarnings("checkstyle:AbbreviationAsWordInName")
    private static String PROPERTY_NAME = "property-name";
    @SuppressWarnings("checkstyle:AbbreviationAsWordInName")
    private static String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationModel buildModification() {
        return LoadCreationModel.builder()
            .equipmentId("idLoad1")
            .equipmentName("nameLoad1")
            .voltageLevelId("v1")
            .busOrBusbarSectionId("bus1")
            .loadType(LoadType.FICTITIOUS)
            .p0(200.0)
            .q0(30.0)
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getLoad("idLoad1"));
        assertEquals(PROPERTY_VALUE, getNetwork().getLoad("idLoad1").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        // assertEquals("LOAD_CREATION", modificationModel.getMessageType());
        // Map<String, String> createdValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals("idLoad1", createdValues.get("equipmentId"));
    }

    @Override
    protected void checkModification() {
    }
}
