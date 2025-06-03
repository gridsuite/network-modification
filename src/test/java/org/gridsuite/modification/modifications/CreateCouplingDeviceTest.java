/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.SwitchKind;
import org.gridsuite.modification.dto.CouplingDeviceInfos;
import org.gridsuite.modification.dto.CreateCouplingDeviceInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkWithTeePoint;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createBusBarSection;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
class CreateCouplingDeviceTest extends AbstractNetworkModificationTest {
    @Override
    public void checkModification() {
        // test
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkWithTeePoint.create(networkUuid);
        createBusBarSection(network.getVoltageLevel("v1"), "bbs5", null, 1);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return CreateCouplingDeviceInfos.builder()
            .stashed(false)
            .voltageLevelId("v1")
            .couplingDeviceInfos(CouplingDeviceInfos.builder()
                .busbarSectionId1("bbs1")
                .busbarSectionId2("bbs5")
                .build())
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Switch switch1 = getNetwork().getSwitch("v1_BREAKER");
        Assertions.assertNotNull(switch1);
        Assertions.assertEquals(SwitchKind.BREAKER, switch1.getKind());
        Assertions.assertEquals("v1", switch1.getVoltageLevel().getId());
        Assertions.assertFalse(switch1.isOpen());
        Assertions.assertTrue(switch1.isRetained());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("ADD_COUPLING_DEVICE", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1", updatedValues.get("voltageLevelId"));
    }

    @Test
    void testCreateCouplingDeviceFail() {
        CreateCouplingDeviceInfos createCouplingDeviceInfos = CreateCouplingDeviceInfos.builder()
            .stashed(false)
            .voltageLevelId("v1")
            .couplingDeviceInfos(CouplingDeviceInfos.builder()
                .busbarSectionId1("bbs1")
                .busbarSectionId2("bbs2")
                .build())
            .build();
        Map<String, String> updatedValues = createCouplingDeviceInfos.getMapMessageValues();
        assertEquals("v1", updatedValues.get("voltageLevelId"));
        Network network = getNetwork();
        AbstractModification modification = createCouplingDeviceInfos.toModification();
        String message = Assertions.assertThrows(PowsyblException.class, () -> modification.apply(network)).getMessage();
        Assertions.assertEquals("bbs1 and bbs2 are in two different voltage levels.", message);
    }

}
