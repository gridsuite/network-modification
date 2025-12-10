/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.StaticVarCompensatorCreationInfos;
import org.gridsuite.modification.dto.VoltageRegulationType;
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 * @auther Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class StaticVarCompensatorCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return StaticVarCompensatorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idStaticVarCompensator2")
                .equipmentName("nameStaticVarCompensator2")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .maxSusceptance(null)
                .minSusceptance(null)
                .maxQAtNominalV(224.0)
                .minQAtNominalV(200.0)
                .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
                .voltageSetpoint(120.0)
                .reactivePowerSetpoint(300.0)
                .voltageRegulationType(VoltageRegulationType.DISTANT)
                .regulatingTerminalVlId("v1")
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
                .standbyAutomatonOn(true)
                .standby(true)
                .b0(221.0)
                .lowVoltageSetpoint(200.0)
                .highVoltageSetpoint(400.0)
                .lowVoltageThreshold(250.0)
                .highVoltageThreshold(300.0)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getStaticVarCompensator("idStaticVarCompensator2"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getStaticVarCompensatorStream()
                .filter(transformer -> transformer.getId().equals("idStaticVarCompensator2")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getStaticVarCompensator("idStaticVarCompensator2").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void checkModification() {
        StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos = (StaticVarCompensatorCreationInfos) buildModification();
        staticVarCompensatorCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        NetworkModificationRunException exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("BUS_NOT_FOUND : notFoundBus", exception.getMessage());

        // CreateWithRegulatedTerminalError
        StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos1 = (StaticVarCompensatorCreationInfos) buildModification();
        staticVarCompensatorCreationInfos1.setVoltageRegulationType(VoltageRegulationType.DISTANT);
        staticVarCompensatorCreationInfos1.setRegulatingTerminalVlId("v1");
        staticVarCompensatorCreationInfos1.setRegulatingTerminalId("test");
        staticVarCompensatorCreationInfos1.setRegulatingTerminalType("STATIC_VAR_COMPENSATOR");
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreationInfos1.toModification().check(getNetwork()));
        assertEquals("EQUIPMENT_NOT_FOUND : Equipment with id=test not found with type STATIC_VAR_COMPENSATOR", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("STATIC_VAR_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idStaticVarCompensator2", createdValues.get("equipmentId"));
    }
}
