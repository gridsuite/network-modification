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
import org.gridsuite.modification.dto.BatteryCreationInfos;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class BatteryCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    public void checkModification() {
        BatteryCreationInfos batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        assertThrows(NetworkModificationException.class, () -> batteryCreationInfos.toModification().check(getNetwork()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return BatteryCreationInfos.builder()
                .stashed(false)
                .equipmentId("idBattery2")
                .equipmentName("nameBattery2")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .minP(100.0)
                .maxP(600.0)
                .targetP(400.)
                .targetQ(50.)
                .minQ(20.0)
                .maxQ(25.0)
                .droop(5f)
                .participate(true)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsInfos(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurvePointsInfos(5.6, 9.8, 10.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getBattery("idBattery2"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getBatteryStream()
                .filter(transformer -> transformer.getId().equals("idBattery2")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getBattery("idBattery2").getProperty(PROPERTY_NAME));
    }

    @Test
    void testCreateWithBusbarSectionErrors() throws Exception {
        BatteryCreationInfos batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
                () -> batteryCreationInfos.toModification().apply(getNetwork()));
        assertEquals("BUS_NOT_FOUND : notFoundBus", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BATTERY_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idBattery2", createdValues.get("equipmentId"));
    }
}
