/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.FreePropertyModel;
import org.gridsuite.modification.model.GeneratorCreationModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.ReactiveCapabilityCurvePointsModel;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class GeneratorCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected void checkModification() {
        GeneratorCreationModel generatorCreationInfos = (GeneratorCreationModel) buildModification();
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
                () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals(BUS_NOT_FOUND, exception.getType());
        assertEquals("BUS_NOT_FOUND : notFoundBus", exception.getMessage());
    }

    @Override
    protected ModificationModel buildModification() {
        return GeneratorCreationModel.builder()
                .equipmentId("idGenerator2")
                .equipmentName("nameGenerator2")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .energySource(EnergySource.HYDRO)
                .minP(100.0)
                .maxP(600.0)
                .ratedS(10.)
                .targetP(400.)
                .targetQ(50.)
                .voltageRegulationOn(true)
                .targetV(225.)
                .stepUpTransformerX(60.0)
                .directTransX(61.0)
                .minQ(20.0)
                .maxQ(25.0)
                .plannedActivePowerSetPoint(222.)
                .marginalCost(0.50)
                .plannedOutageRate(.85)
                .forcedOutageRate(.96)
                .droop(5f)
                .participate(true)
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
                .regulatingTerminalVlId("v1")
                .qPercent(25.)
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
        assertNotNull(getNetwork().getGenerator("idGenerator2"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator2")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getGenerator("idGenerator2").getProperty(PROPERTY_NAME));
    }

    @Test
    void testCreateWithBusbarSectionErrors() throws Exception {
        GeneratorCreationModel generatorCreationInfos = (GeneratorCreationModel) buildModification();
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
                () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("BUS_NOT_FOUND : notFoundBus", exception.getMessage());
    }

    @Test
    void testCreateWithRegulatedTerminalError() throws Exception {
        // invalid regulating terminal id <---> regulation terminal type
        GeneratorCreationModel generatorCreationInfos = (GeneratorCreationModel) buildModification();
        generatorCreationInfos.setRegulatingTerminalType("LINE");
        generatorCreationInfos.setRegulatingTerminalId("titi");

        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
                () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("EQUIPMENT_NOT_FOUND : Equipment with id=titi not found with type LINE", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationInfos) throws Exception {
        // assertEquals("GENERATOR_CREATION", modificationInfos.getMessageType());
        // Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals("idGenerator2", createdValues.get("equipmentId"));
    }
}
