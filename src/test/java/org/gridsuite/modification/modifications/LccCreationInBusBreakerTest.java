/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LccConverterStationCreationInfos;
import org.gridsuite.modification.dto.LccCreationInfos;
import org.gridsuite.modification.dto.LccShuntCompensatorInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ghazwa REHILI <ghazwa.rehili at rte-france.com>
 */
class LccCreationInBusBreakerTest extends AbstractNetworkModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LccCreationInfos.builder()
                .stashed(false)
                .equipmentId("lcc1")
                .equipmentName("lcc1Name")
                .nominalV(39.)
                .r(4.)
                .maxP(56.)
                .convertersMode(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER)
                .activePowerSetpoint(5.)
                .converterStation1(buildConverterStation1WithShuntCompensatorsOnSide())
                .converterStation2(buildConverterStation2WithShuntCompensatorsOnSide())
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    private static LccConverterStationCreationInfos buildConverterStation1WithShuntCompensatorsOnSide() {
        var filter1 = LccShuntCompensatorInfos.builder()
                .id("ShuntStation1Id1")
                .name("ShuntStation1Name1")
                .maxQAtNominalV(0.1)
                .connectedToHvdc(true)
                .build();

        var filter2 = LccShuntCompensatorInfos.builder()
                .id("ShuntStation1Id2")
                .name("ShuntStation1Name2")
                .maxQAtNominalV(0.1)
                .connectedToHvdc(false)
                .build();

        return LccConverterStationCreationInfos.builder()
                .equipmentId("lcc1Station1Id")
                .equipmentName("lcc1Station1Name")
                .lossFactor(40F)
                .powerFactor(1F)
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .shuntCompensatorsOnSide(List.of(filter1, filter2))
                .build();
    }

    private static LccConverterStationCreationInfos buildConverterStation2WithShuntCompensatorsOnSide() {
        return LccConverterStationCreationInfos.builder()
                .equipmentId("lcc2Station2Id")
                .equipmentName("lcc2Station2Name")
                .lossFactor(40F)
                .powerFactor(1F)
                .voltageLevelId("v2")
                .busOrBusbarSectionId("bus2")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .shuntCompensatorsOnSide(List.of())
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getHvdcLine("lcc1"));
        assertEquals(PROPERTY_VALUE, getNetwork().getHvdcLine("lcc1").getProperty(PROPERTY_NAME));

    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LCC_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("lcc1", createdValues.get("equipmentId"));
    }

    @Override
    protected void checkModification() {
        LccCreationInfos lccCreationInfos = (LccCreationInfos) buildModification();
        lccCreationInfos.getConverterStation1().setBusOrBusbarSectionId("notFoundBus");
        LccCreation lccCreation = (LccCreation) lccCreationInfos.toModification();
        Network network = getNetwork();
        NetworkModificationRunException exception = assertThrows(NetworkModificationRunException.class, () -> lccCreation.check(network));
        assertEquals("Bus notFoundBus does not exist in network", exception.getMessage());
    }
}
