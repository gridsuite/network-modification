/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ValidationException;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class BatteryCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
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
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new battery in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        return BatteryCreationInfos.builder()
                .stashed(false)
                .equipmentId("idBattery1")
                .equipmentName("idBattery1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .minP(100.0)
                .maxP(600.0)
                .targetP(400.)
                .targetQ(50.)
                .minQ(20.0)
                .maxQ(25.0)
                .droop(5f)
                .participate(true)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurveCreationInfos(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurveCreationInfos(5.6, 9.8, 10.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getBattery("idBattery1"));
        assertEquals(1, getNetwork().getVoltageLevel("v2").getBatteryStream()
                .filter(transformer -> transformer.getId().equals("idBattery1")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getBattery("idBattery1").getProperty(PROPERTY_NAME));
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // invalid Battery id
        BatteryCreationInfos batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setEquipmentId("");
        PowsyblException exception = assertThrows(PowsyblException.class, () -> batteryCreationInfos.toModification().apply(getNetwork()));
        assertEquals("Invalid id ''", exception.getMessage());

        // not found voltage level
        batteryCreationInfos.setEquipmentId("idBattery1");
        batteryCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationInfos.toModification().check(getNetwork()));
        assertEquals("VOLTAGE_LEVEL_NOT_FOUND : notFoundVoltageLevelId", exception.getMessage());

        // not found busbar section
        batteryCreationInfos.setVoltageLevelId("v2");
        batteryCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationInfos.toModification().check(getNetwork()));
        assertEquals("BUSBAR_SECTION_NOT_FOUND : notFoundBusbarSection", exception.getMessage());

        // invalid min active power
        batteryCreationInfos.setVoltageLevelId("v2");

        batteryCreationInfos.setBusOrBusbarSectionId("1B");
        batteryCreationInfos.setMinP(Double.NaN);
        exception = assertThrows(ValidationException.class, () -> batteryCreationInfos.toModification().apply(getNetwork()));
        assertEquals("Battery 'idBattery1': invalid value (NaN) for minimum P", exception.getMessage());

        // invalid min max reactive limit
        BatteryCreationInfos batteryCreationInfos1 = (BatteryCreationInfos) buildModification();
        batteryCreationInfos1.setReactiveCapabilityCurve(false);
        batteryCreationInfos1.setMinQ(Double.NaN);

        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationInfos1.toModification().check(getNetwork()));
        assertEquals("CREATE_BATTERY_ERROR : Battery 'idBattery1' : minimum reactive power is not set", exception.getMessage());

        BatteryCreationInfos batteryCreationInfos2 = (BatteryCreationInfos) buildModification();
        batteryCreationInfos2.setReactiveCapabilityCurve(false);
        batteryCreationInfos2.setMaxQ(Double.NaN);

        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationInfos2.toModification().check(getNetwork()));
        assertEquals("CREATE_BATTERY_ERROR : Battery 'idBattery1' : maximum reactive power is not set", exception.getMessage());

        batteryCreationInfos2.setReactiveCapabilityCurve(false);
        batteryCreationInfos2.setMinQ(200.);
        batteryCreationInfos2.setMaxQ(100.);
        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationInfos2.toModification().check(getNetwork()));
        assertEquals("CREATE_BATTERY_ERROR : Battery 'idBattery1' : maximum reactive power is expected to be greater than or equal to minimum reactive power", exception.getMessage());

        // invalid reactive capability curve limit
        BatteryCreationInfos batteryCreationInfos3 = (BatteryCreationInfos) buildModification();
        batteryCreationInfos3.getReactiveCapabilityCurvePoints().get(0).setP(Double.NaN);

        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationInfos3.toModification().check(getNetwork()));
        assertEquals("CREATE_BATTERY_ERROR : Battery 'idBattery1' : P is not set in a reactive capability curve limits point", exception.getMessage());
        // try to create an existing battery
        BatteryCreationInfos batteryCreationInfos4 = (BatteryCreationInfos) buildModification();
        batteryCreationInfos4.setEquipmentId("v3Battery");
        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationInfos4.toModification().check(getNetwork()));
        assertEquals("BATTERY_ALREADY_EXISTS : v3Battery", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BATTERY_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idBattery1", updatedValues.get("equipmentId"));
    }
}
