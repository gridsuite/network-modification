/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.SubstationCreationInfos;
import org.gridsuite.modification.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.utils.ModificationCreation;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class VoltageLevelCreationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        VoltageLevelCreationInfos voltageLevelInfos = ModificationCreation.getCreationVoltageLevel("s2", "vlId", "vlName");
        voltageLevelInfos.setProperties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()));
        return voltageLevelInfos;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getVoltageLevel("vlId"));
        assertNotNull(getNetwork().getBusbarSection("vlId_1_1"));
        assertNotNull(getNetwork().getBusbarSection("vlId_1_2"));
        assertNotNull(getNetwork().getBusbarSection("vlId_2_1"));
        assertNotNull(getNetwork().getBusbarSection("vlId_2_2"));
        assertTrue(getNetwork().getSubstation("s2").getVoltageLevelStream().anyMatch(vl -> vl.getId().equals("vlId")));
        assertEquals(1, getNetwork().getSubstation("s2").getVoltageLevelStream().filter(vl -> vl.getId().equals("vlId")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getVoltageLevel("vlId").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void checkModification() {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setSubstationId("absent_station");

        Exception exception = assertThrows(NetworkModificationException.class, () -> vli.toModification().apply(getNetwork()));
        assertEquals(new NetworkModificationException(SUBSTATION_NOT_FOUND, "absent_station").getMessage(),
                exception.getMessage());

        VoltageLevelCreationInfos vli1 = (VoltageLevelCreationInfos) buildModification();
        vli1.getCouplingDevices().get(0).setBusbarSectionId1("1.1");
        vli1.getCouplingDevices().get(0).setBusbarSectionId2("1.1");
        exception = assertThrows(NetworkModificationException.class, () -> vli1.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Coupling between same bus bar section is not allowed").getMessage(),
                exception.getMessage());

        VoltageLevelCreationInfos vli2 = (VoltageLevelCreationInfos) buildModification();
        vli2.setIpMin(0.0);
        vli2.setIpMax(null);
        exception = assertThrows(NetworkModificationException.class, () -> vli2.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMax is required").getMessage(),
                exception.getMessage());

        // try to create an existing VL
        VoltageLevelCreationInfos vli3 = (VoltageLevelCreationInfos) buildModification();
        vli3.setEquipmentId("v1");
        exception = assertThrows(NetworkModificationException.class, () -> vli3.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, "v1").getMessage(),
                exception.getMessage());

        // check values
        VoltageLevelCreationInfos vli4 = (VoltageLevelCreationInfos) buildModification();
        vli4.setNominalV(-400);
        exception = assertThrows(NetworkModificationException.class, () -> vli4.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Voltage level 'vlId' : can not have a negative value for Nominal Voltage").getMessage(),
            exception.getMessage());

        VoltageLevelCreationInfos vli5 = (VoltageLevelCreationInfos) buildModification();
        vli5.setLowVoltageLimit(-100d);
        exception = assertThrows(NetworkModificationException.class, () -> vli5.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Voltage level 'vlId' : can not have a negative value for Low voltage limit").getMessage(),
            exception.getMessage());

        VoltageLevelCreationInfos vli6 = (VoltageLevelCreationInfos) buildModification();
        vli6.setHighVoltageLimit(-50d);
        exception = assertThrows(NetworkModificationException.class, () -> vli6.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Voltage level 'vlId' : can not have a negative value for High voltage limit").getMessage(),
            exception.getMessage());
    }

    @Test
    void testCreateWithBbsNotExist() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_1");
        vli.getCouplingDevices().get(0).setBusbarSectionId1("1.1");
        vli.getCouplingDevices().get(0).setBusbarSectionId2("bbs");
        vli.toModification().apply(getNetwork());
        assertNotNull(getNetwork().getVoltageLevel("vl_1"));

        vli.setEquipmentId("vl_2");
        vli.getCouplingDevices().get(0).setBusbarSectionId1("bbs");
        vli.getCouplingDevices().get(0).setBusbarSectionId2("1.1");
        vli.toModification().apply(getNetwork());
        assertNotNull(getNetwork().getVoltageLevel("vl_2"));
    }

    @Test
    void testCreateWithSubstationCreation() throws Exception {
        SubstationCreationInfos substationCreationInfos = SubstationCreationInfos.builder()
                .stashed(false)
                .equipmentId("newSubstationId")
                .equipmentName("newSubstationName")
                .country(Country.AF)
                .build();
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setSubstationId(substationCreationInfos.getEquipmentId());
        vli.setSubstationCreation(substationCreationInfos);
        vli.toModification().apply(getNetwork());
        assertNotNull(getNetwork().getVoltageLevel("vlId"));
        assertNotNull(getNetwork().getSubstation("newSubstationId"));
        assertTrue(getNetwork().getSubstation("newSubstationId").getVoltageLevelStream()
                .anyMatch(vl -> vl.getId().equals("vlId")));
        assertEquals(1, getNetwork().getSubstation("newSubstationId").getVoltageLevelStream()
                .filter(vl -> vl.getId().equals("vlId")).count());
    }

    @Test
    void testIpMinEqualsIpMax() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_ok");
        vli.setIpMin(25.0);
        vli.setIpMax(25.0);
        vli.toModification().apply(getNetwork());
        // VL is created
        assertNotNull(getNetwork().getVoltageLevel("vl_ok"));
    }

    @Test
    void testCreateWithIpMinNull() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_ok");
        vli.setIpMin(null);
        vli.setIpMax(25.0);
        vli.toModification().apply(getNetwork());
        // VL is created
        assertNotNull(getNetwork().getVoltageLevel("vl_ok"));
    }

    private void testIccWithError(Double ipMin, Double ipMax, String reportError) throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_ko");
        vli.setIpMin(ipMin);
        vli.setIpMax(ipMax);
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> vli.toModification().check(getNetwork()));
        // VL could not have been created
        assertNull(getNetwork().getVoltageLevel("vl_ko"));
        assertEquals(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, reportError).getMessage(), exception.getMessage());
    }

    @Test
    void testIpMinGreaterThanIpMax() throws Exception {
        testIccWithError(15.1, 15.0, "IpMin cannot be greater than IpMax");
    }

    @Test
    void testIpMinNegative() throws Exception {
        testIccWithError(-25.0, 15.0, "IpMin must be positive");
    }

    @Test
    void testIpMaxNegative() throws Exception {
        testIccWithError(25.0, -15.0, "IpMax must be positive");
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VOLTAGE_LEVEL_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("vlId", createdValues.get("equipmentId"));
    }
}
