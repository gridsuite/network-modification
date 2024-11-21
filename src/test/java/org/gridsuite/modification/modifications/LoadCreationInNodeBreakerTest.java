/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ValidationException;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LoadCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LoadCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static String PROPERTY_NAME = "property-name";
    private static String PROPERTY_VALUE = "property-value";

    @Override
    protected void checkModification() {
        LoadCreationInfos loadCreationInfos = (LoadCreationInfos) buildModification();
        // VoltageLevel not found
        loadCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> loadCreationInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(), exception.getMessage());

        loadCreationInfos.setEquipmentId("idLoad1");
        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        exception = assertThrows(NetworkModificationException.class, () -> loadCreationInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage(), exception.getMessage());

        loadCreationInfos.setBusOrBusbarSectionId("1B");
        loadCreationInfos.setP0(Double.NaN);
        ValidationException exception1 = assertThrows(ValidationException.class, () -> loadCreationInfos.toModification().apply(getNetwork()));
        assertEquals("Load 'idLoad1': p0 is invalid", exception1.getMessage());
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadCreationInfos.builder()
            .stashed(false)
            .equipmentId("idLoad1")
            .equipmentName("nameLoad1")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .loadType(LoadType.AUXILIARY)
            .p0(100.0)
            .q0(60.0)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .connectionName("top")
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getLoad("idLoad1"));
        assertEquals(PROPERTY_VALUE, getNetwork().getLoad("idLoad1").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LOAD_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLoad1", createdValues.get("equipmentId"));
    }
}
