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
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LineCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LineCreationInMixedTypologyTest extends AbstractNetworkModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createMixedTopology(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new line in voltage levels with node breaker topology and bus breaker topology
        // between voltage level "v1" and busbar section "1.1" type NODE_BREAKER and
        //         voltage level "v2" and busbar section "bus2 type BUS_BREAKER"
        return LineCreationInfos.builder()
            .stashed(false)
            .equipmentId("idLine1")
            .equipmentName("nameLine1")
            .r(100.0)
            .x(100.0)
            .g1(10.0)
            .b1(10.0)
            .g2(20.0)
            .b2(20.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .connectionName1("cn1Line1")
            .connectionDirection1(ConnectablePosition.Direction.TOP)
            .connectionName2("cn2Line1")
            .connectionDirection2(ConnectablePosition.Direction.TOP)
            .connectionPosition1(0)
            .connectionPosition2(0)
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getLine("idLine1"));
        assertEquals(PROPERTY_VALUE, getNetwork().getLine("idLine1").getProperty(PROPERTY_NAME));

    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLine1", createdValues.get("equipmentId"));
    }

    @Override
    protected void checkModification() {
    }
}
