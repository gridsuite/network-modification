/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ValidationException;

import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CurrentLimitsInfos;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LineCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LineCreationInBusBreakerTest extends AbstractNetworkModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected void checkModification() {
        LineCreationInfos lineCreationInfos = (LineCreationInfos) buildModification();
        lineCreationInfos.setBusOrBusbarSectionId2("notFoundBus");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> lineCreationInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                exception.getMessage());
    }

    @Test
    void testCreateLineOptionalParameters5() throws Exception {
        LineCreationInfos lineCreationInfosPermanentLimitNOK = LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("idLine2")
                .equipmentName("nameLine2")
                .r(100.0)
                .x(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits1(
                        List.of(CurrentLimitsInfos.builder().permanentLimit(-1.0).build())
                )
                .build();
        ValidationException exception = assertThrows(ValidationException.class, () -> lineCreationInfosPermanentLimitNOK.toModification().apply(getNetwork()));
        assertEquals("AC Line 'idLine2': permanent limit must be >= 0", exception.getMessage());
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "bus1" and
        //         voltage level "v2" and busbar section "bus2"
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
            .busOrBusbarSectionId1("bus1")
            .currentLimits1(
                    List.of(CurrentLimitsInfos.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build())
            )
            .currentLimits2(
                    List.of(CurrentLimitsInfos.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build())
            )
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
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
}
