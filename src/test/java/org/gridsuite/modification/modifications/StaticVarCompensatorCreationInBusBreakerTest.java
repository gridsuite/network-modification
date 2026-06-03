/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.FreePropertyModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.StaticVarCompensatorCreationModel;
import org.gridsuite.modification.model.constants.VoltageRegulationType;
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
    protected ModificationModel buildModification() {
        return StaticVarCompensatorCreationModel.builder()
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
                .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
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
        StaticVarCompensatorCreationModel staticVarCompensatorCreationModel = (StaticVarCompensatorCreationModel) buildModification();
        staticVarCompensatorCreationModel.setBusOrBusbarSectionId("notFoundBus");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreationModel.toModification().check(getNetwork()));
        assertEquals("BUS_NOT_FOUND : notFoundBus", exception.getMessage());

        // CreateWithRegulatedTerminalError
        StaticVarCompensatorCreationModel staticVarCompensatorCreationModel1 = (StaticVarCompensatorCreationModel) buildModification();
        staticVarCompensatorCreationModel1.setVoltageRegulationType(VoltageRegulationType.DISTANT);
        staticVarCompensatorCreationModel1.setRegulatingTerminalVlId("v1");
        staticVarCompensatorCreationModel1.setRegulatingTerminalId("test");
        staticVarCompensatorCreationModel1.setRegulatingTerminalType("STATIC_VAR_COMPENSATOR");
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreationModel1.toModification().check(getNetwork()));
        assertEquals("EQUIPMENT_NOT_FOUND : Equipment with id=test not found with type STATIC_VAR_COMPENSATOR", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("STATIC_VAR_COMPENSATOR_CREATION", modificationModel.getType().toString());
        Map<String, String> createdValues = modificationModel.getMapMessageValues();
        assertEquals("idStaticVarCompensator2", createdValues.get("equipmentId"));
    }
}
