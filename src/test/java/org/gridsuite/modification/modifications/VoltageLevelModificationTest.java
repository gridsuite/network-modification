/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_VOLTAGE_LEVEL_ERROR;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class VoltageLevelModificationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId("v1")
                .equipmentName(new AttributeModification<>("test 1", OperationType.SET))
                .nominalV(new AttributeModification<>(420D, OperationType.SET))
                .lowVoltageLimit(new AttributeModification<>(30D, OperationType.SET))
                .highVoltageLimit(new AttributeModification<>(50D, OperationType.SET))
                .ipMax(new AttributeModification<>(0.8, OperationType.SET))
                .ipMin(new AttributeModification<>(0.7, OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        VoltageLevel voltageLevel = getNetwork().getVoltageLevel("v1");
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit);
        assertNotNull(voltageLevel);
        assertEquals("test 1", voltageLevel.getNameOrId());
        assertEquals(420D, voltageLevel.getNominalV(), 0);
        assertEquals(30D, voltageLevel.getLowVoltageLimit(), 0);
        assertEquals(50D, voltageLevel.getHighVoltageLimit(), 0);
        assertEquals(0.8, identifiableShortCircuit.getIpMax(), 0);
        assertEquals(0.7, identifiableShortCircuit.getIpMin(), 0);
        assertEquals(PROPERTY_VALUE, getNetwork().getVoltageLevel("v1").getProperty(PROPERTY_NAME));
    }

    @Test
    void testModifyShortCircuitExtension() {
        VoltageLevelModificationInfos infos = (VoltageLevelModificationInfos) buildModification();
        applyModification(infos);

        VoltageLevelModificationInfos updatedInfos = VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId("v1")
                .ipMax(new AttributeModification<>(0.9, OperationType.SET))
                .build();
        applyModification(updatedInfos);
        VoltageLevel voltageLevel = getNetwork().getVoltageLevel("v1");
        assertNotNull(voltageLevel);

        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit1 = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit1);
        assertEquals(0.9, identifiableShortCircuit1.getIpMax(), 0);
        assertEquals(0.7, identifiableShortCircuit1.getIpMin(), 0);

        VoltageLevelModificationInfos updatedInfos2 = VoltageLevelModificationInfos.builder()
                .equipmentId("v1")
                .ipMin(new AttributeModification<>(0.2, OperationType.SET))
                .build();
        applyModification(updatedInfos2);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit2 = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        assertEquals(0.9, identifiableShortCircuit2.getIpMax(), 0);
        assertEquals(0.2, identifiableShortCircuit2.getIpMin(), 0);
    }

    private void testIpMinIpMaxNotChanged(Double ipMin, Double ipMax, String reportError) {
        final String vlWithBothIcc = "v3";

        VoltageLevelModificationInfos vli = VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId(vlWithBothIcc)
                .build();
        if (ipMin != null) {
            vli.setIpMin(new AttributeModification<>(ipMin, OperationType.SET));
        }
        if (ipMax != null) {
            vli.setIpMax(new AttributeModification<>(ipMax, OperationType.SET));
        }
        Network network = getNetwork();
        VoltageLevelModification voltageLevelModification = (VoltageLevelModification) vli.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> voltageLevelModification.check(network));
        assertEquals(new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, reportError).getMessage(), exception.getMessage());
    }

    private void testIpMinGreaterThanIpMax() {
        // check only modification inputs
        testIpMinIpMaxNotChanged(30.0, 29.0, "IpMin cannot be greater than IpMax");
    }

    private void testIpMinNegative() {
        // check only modification inputs
        testIpMinIpMaxNotChanged(-30.0, 0.0, "IpMin must be positive");
    }

    private void testIpMaxNegative() {
        // check only modification inputs
        testIpMinIpMaxNotChanged(0.0, -12.0, "IpMax must be positive");
    }

    private void testIpMinGreaterThanEquipmentIpMax() {
        // check ipMin modification input against equipement ipMax real value (25.0)
        testIpMinIpMaxNotChanged(30.0, null, "IpMin cannot be greater than IpMax");
    }

    private void testEquipmentIpMinGreaterThanIpMax() {
        // check ipMax modification input against equipement ipMin real value (15.0)
        testIpMinIpMaxNotChanged(null, 14.9, "IpMin cannot be greater than IpMax");
    }

    @Test
    void testIpMinEqualsIpMax() {
        final String vlWithBothIcc = "v3";
        final double iccValue = 29.0;
        VoltageLevelModificationInfos vli = (VoltageLevelModificationInfos) buildModification();
        vli.setIpMin(new AttributeModification<>(iccValue, OperationType.SET));
        vli.setIpMax(new AttributeModification<>(iccValue, OperationType.SET));
        vli.setEquipmentId(vlWithBothIcc);
        applyModification(vli);

        // check the update has been made
        VoltageLevel voltageLevelUpdated = getNetwork().getVoltageLevel(vlWithBothIcc);
        assertNotNull(voltageLevelUpdated);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit1 = voltageLevelUpdated.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit1);
        assertEquals(iccValue, identifiableShortCircuit1.getIpMin(), 0);
        assertEquals(iccValue, identifiableShortCircuit1.getIpMax(), 0);
    }

    private void testSetIpMinOnEquipmentWithoutExtension() {
        final String vlWithNoIcc = "v2";
        VoltageLevelModificationInfos vli = VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId(vlWithNoIcc)
                .ipMin(new AttributeModification<>(10.0, OperationType.SET))
                .build();
        Network network = getNetwork();
        VoltageLevelModification voltageLevelModification = (VoltageLevelModification) vli.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> voltageLevelModification.check(network));
        assertEquals(new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMax is required").getMessage(), exception.getMessage());
    }

    private void testSetNominalVoltage() {
        final String vlWithNoIcc = "v2";
        VoltageLevelModificationInfos vli = VoltageLevelModificationInfos.builder()
            .stashed(false)
            .equipmentId(vlWithNoIcc)
            .nominalV(new AttributeModification<>(-10.0, OperationType.SET))
            .build();
        Network network = getNetwork();
        VoltageLevelModification voltageLevelModification = (VoltageLevelModification) vli.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> voltageLevelModification.check(network));
        assertEquals(new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "Voltage level 'v2' : can not have a negative value for Nominal Voltage").getMessage(), exception.getMessage());
    }

    private void testSetLowVoltageLimit() {
        final String vlWithNoIcc = "v2";
        VoltageLevelModificationInfos vli = VoltageLevelModificationInfos.builder()
            .stashed(false)
            .equipmentId(vlWithNoIcc)
            .lowVoltageLimit(new AttributeModification<>(-11.0, OperationType.SET))
            .build();
        Network network = getNetwork();
        VoltageLevelModification voltageLevelModification = (VoltageLevelModification) vli.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> voltageLevelModification.check(network));
        assertEquals(new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "Voltage level 'v2' : can not have a negative value for Low voltage limit").getMessage(), exception.getMessage());
    }

    private void testSetHighVoltageLimit() {
        final String vlWithNoIcc = "v2";
        VoltageLevelModificationInfos vli = VoltageLevelModificationInfos.builder()
            .stashed(false)
            .equipmentId(vlWithNoIcc)
            .highVoltageLimit(new AttributeModification<>(-12.0, OperationType.SET))
            .build();
        Network network = getNetwork();
        VoltageLevelModification voltageLevelModification = (VoltageLevelModification) vli.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> voltageLevelModification.check(network));
        assertEquals(new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "Voltage level 'v2' : can not have a negative value for High voltage limit").getMessage(), exception.getMessage());
    }

    @Test
    void testSetIpMaxOnEquipmentWitOnlyIpMaxExtension() {
        final String vlName = "v2"; // has no ICC
        getNetwork().getVoltageLevel(vlName)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMax(30.0).add();

        final double targetIpMax = 29.0;
        VoltageLevelModificationInfos vli = VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId(vlName)
                .ipMax(new AttributeModification<>(targetIpMax, OperationType.SET))
                .build();
        applyModification(vli);
        // check the update has been made
        VoltageLevel voltageLevelUpdated = getNetwork().getVoltageLevel(vlName);
        assertNotNull(voltageLevelUpdated);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit1 = voltageLevelUpdated.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit1);
        assertEquals(0, identifiableShortCircuit1.getIpMin(), 0);
        assertEquals(targetIpMax, identifiableShortCircuit1.getIpMax(), 0);
    }

    private void applyModification(VoltageLevelModificationInfos infos) {
        infos.toModification().apply(getNetwork());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VOLTAGE_LEVEL_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1", createdValues.get("equipmentId"));
    }

    @Override
    protected void checkModification() {
        testIpMinGreaterThanIpMax();
        testIpMinNegative();
        testIpMaxNegative();
        testIpMinGreaterThanEquipmentIpMax();
        testEquipmentIpMinGreaterThanIpMax();
        testSetIpMinOnEquipmentWithoutExtension();
        testSetNominalVoltage();
        testSetLowVoltageLimit();
        testSetHighVoltageLimit();
    }
}
