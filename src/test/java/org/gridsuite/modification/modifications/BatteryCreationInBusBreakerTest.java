/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.BatteryCreationModel;
import org.gridsuite.modification.model.FreePropertyModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.ReactiveCapabilityCurvePointsModel;
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
        BatteryCreationModel batteryCreationModel = (BatteryCreationModel) buildModification();
        batteryCreationModel.setBusOrBusbarSectionId("notFoundBus");
        assertThrows(NetworkModificationException.class, () -> batteryCreationModel.toModification().check(getNetwork()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationModel buildModification() {
        return BatteryCreationModel.builder()
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
            .stepUpTransformerX(60.0)
            .directTransX(61.0)
            .participate(true)
            .reactiveCapabilityCurve(true)
            .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsModel(2.0, 3.0, 3.1),
                new ReactiveCapabilityCurvePointsModel(5.6, 9.8, 10.8)))
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
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
        BatteryCreationModel batteryCreationModel = (BatteryCreationModel) buildModification();
        batteryCreationModel.setBusOrBusbarSectionId("notFoundBus");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
            () -> batteryCreationModel.toModification().apply(getNetwork()));
        assertEquals("BUS_NOT_FOUND : notFoundBus", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("BATTERY_CREATION", modificationModel.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        });
        assertEquals("idBattery2", createdValues.get("equipmentId"));
        Battery battery = getNetwork().getBattery("idBattery2");
        assertNotNull(battery.getExtension(ActivePowerControl.class));
        ActivePowerControl activePowerControl = battery.getExtension(ActivePowerControl.class);
        assertEquals(5, activePowerControl.getDroop());
        assertTrue(activePowerControl.isParticipate());
    }

    @Test
    void testCreateWithDroopNull() {
        Network network = getNetwork();
        BatteryCreationModel batteryCreationModel = BatteryCreationModel.builder()
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
            .droop(null)
            .participate(false)
            .reactiveCapabilityCurve(false)
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        batteryCreationModel.toModification().apply(network);
        Battery battery = network.getBattery("idBattery2");
        assertNotNull(battery.getExtension(ActivePowerControl.class));
        ActivePowerControl activePowerControl = battery.getExtension(ActivePowerControl.class);
        assertEquals(Double.NaN, activePowerControl.getDroop());
        assertFalse(activePowerControl.isParticipate());
    }
}
