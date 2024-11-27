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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LccConverterStationCreationInfos;
import org.gridsuite.modification.dto.LccCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
class LccCreationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
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
                .converterStation1(buildConverterStationWithMcsOnSide("lccStation1Id", "lccStation1Name"))
                .converterStation2(buildConverterStationWithMcsOnSide("lccStation2Id", "lccStation2Name"))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {

    }

    private static LccConverterStationCreationInfos buildConverterStationWithMcsOnSide(String equipmentId, String equipmentName) {
        var filter1 = LccConverterStationCreationInfos.ShuntCompensatorInfos.builder()
                .shuntCompensatorId("Filter1")
                .shuntCompensatorName("filter1")
                .maxQAtNominalV(0.1)
                .connectedToHvdc(true)
                .build();
        var filter2 = LccConverterStationCreationInfos.ShuntCompensatorInfos.builder()
                .shuntCompensatorId("Filter2")
                .shuntCompensatorName("filter2")
                .maxQAtNominalV(0.1)
                .connectedToHvdc(true)
                .build();

        return LccConverterStationCreationInfos.builder()
                .equipmentId(equipmentId)
                .equipmentName(equipmentName)
                .lossFactor(40F)
                .powerFactor(1F)
                .voltageLevelId("v1")
                .busOrBusbarSectionId("1.1")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .mcsOnSide(List.of(filter1, filter2))
                .build();
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
        // not found voltage level
        lccCreationInfos.setEquipmentId("lccId");
        LccConverterStationCreationInfos converterStationCreationInfos = buildConverterStationWithMcsOnSide("lccStation1Id", "lccStation1Name");
        converterStationCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        lccCreationInfos.setConverterStation2(converterStationCreationInfos);
        LccCreation lccCreation = (LccCreation) lccCreationInfos.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> lccCreation.check(getNetwork()));
        assertEquals(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(), exception.getMessage());
    }

}
