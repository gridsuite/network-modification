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
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LineCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LineCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected void checkModification() {
        LineCreationInfos lineCreationInfos = (LineCreationInfos) buildModification();
        lineCreationInfos.setEquipmentId("idLine4");
        lineCreationInfos.setVoltageLevelId1("notFoundVoltageLevelId1");
        Exception exception = assertThrows(NetworkModificationException.class, () -> lineCreationInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId1").getMessage(),
                exception.getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBusbarSection1");
        exception = assertThrows(NetworkModificationException.class, () -> lineCreationInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection1").getMessage(),
                exception.getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setR(Double.NaN);
        exception = assertThrows(ValidationException.class, () -> lineCreationInfos.toModification().apply(getNetwork()));
        assertEquals("AC Line 'idLine4': r is invalid", exception.getMessage());

        lineCreationInfos.setR(100.0);
        lineCreationInfos.setX(Double.NaN);
        exception = assertThrows(ValidationException.class, () -> lineCreationInfos.toModification().apply(getNetwork()));
        assertEquals("AC Line 'idLine4': x is invalid", exception.getMessage());

        // try to create an existing line
        lineCreationInfos.setEquipmentId("line2");
        exception = assertThrows(NetworkModificationException.class, () -> lineCreationInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(LINE_ALREADY_EXISTS, "line2").getMessage(),
                exception.getMessage());

        LineCreationInfos lineCreationInfos1 = LineCreationInfos.builder()
            .equipmentId("line8")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .r(-1d)
            .build();
        String message = assertThrows(NetworkModificationException.class,
            () -> lineCreationInfos1.toModification().check(getNetwork())).getMessage();
        assertEquals("CREATE_LINE_ERROR : Line 'line8' : can not have a negative value for R", message);

        LineCreationInfos lineModificationInfos2 = LineCreationInfos.builder()
            .equipmentId("line8")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .g1(-2d)
            .build();
        message = assertThrows(NetworkModificationException.class,
            () -> lineModificationInfos2.toModification().check(getNetwork())).getMessage();
        assertEquals("CREATE_LINE_ERROR : Line 'line8' : can not have a negative value for G1", message);

        LineCreationInfos lineModificationInfos3 = lineCreationInfos.builder()
            .equipmentId("line8")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .g2(-100d)
            .build();
        message = assertThrows(NetworkModificationException.class,
            () -> lineModificationInfos3.toModification().check(getNetwork())).getMessage();
        assertEquals("CREATE_LINE_ERROR : Line 'line8' : can not have a negative value for G2", message);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("idLine")
                .equipmentName("nameLine")
                .r(100.0)
                .x(100.0)
                .g1(10.0)
                .b1(10.0)
                .g2(20.0)
                .b2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .connectionName1("cn1Line")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn2Line")
                .connectionDirection2(ConnectablePosition.Direction.BOTTOM)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getLine("idLine"));
        assertEquals(PROPERTY_VALUE, getNetwork().getLine("idLine").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLine", createdValues.get("equipmentId"));
    }
}
