/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularBatteryModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                BatteryModificationInfos.builder().equipmentId("v1Battery").maxP(new AttributeModification<>(50., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v2Battery").minP(new AttributeModification<>(5., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v3Battery").targetP(new AttributeModification<>(5., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("unknownBattery").targetQ(new AttributeModification<>(500., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.BATTERY_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(50., getNetwork().getBattery("v1Battery").getMaxP(), 0.001);
        assertEquals(5., getNetwork().getBattery("v2Battery").getMinP(), 0.001);
        assertEquals(5., getNetwork().getBattery("v3Battery").getTargetP(), 0.001);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.BATTERY_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void checkModification() {
        List<ModificationInfos> batteryCreations = new ArrayList<>();
        batteryCreations.add(BatteryCreationInfos.builder()
            .equipmentId("v4Battery")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .droop(101f)
            .build());
        TabularCreationInfos tabularCreationInfos = TabularCreationInfos.builder()
            .creationType(ModificationType.BATTERY_CREATION)
            .creations(batteryCreations)
            .build();
        String message = assertThrows(NetworkModificationException.class,
            () -> tabularCreationInfos.toModification().check(getNetwork())).getMessage();
        assertEquals("CREATE_BATTERY_ERROR : Battery 'v4Battery' : must have Droop between 0 and 100", message);
    }
}
