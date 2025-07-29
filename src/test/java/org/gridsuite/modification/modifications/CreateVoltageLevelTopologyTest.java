/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.SwitchKind;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CreateVoltageLevelTopologyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkWithTeePoint;
import org.junit.jupiter.api.Assertions;

import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
class CreateVoltageLevelTopologyTest extends AbstractNetworkModificationTest {
    @Override
    public void checkModification() {
        CreateVoltageLevelTopologyInfos createVoltageLevelTopologyInfos = new CreateVoltageLevelTopologyInfos();
        AbstractModification modification = createVoltageLevelTopologyInfos.toModification();
        Network network = getNetwork();
        String message = assertThrows(NetworkModificationException.class,
            () -> modification.check(network)).getMessage();
        assertEquals("CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR : Missing required attributes to modify the equipment", message);

        createVoltageLevelTopologyInfos.setVoltageLevelId("notFoundVoltageLevel");
        createVoltageLevelTopologyInfos.setSectionCount(3);
        createVoltageLevelTopologyInfos.setSwitchKinds(List.of(SwitchKind.DISCONNECTOR));
        message = assertThrows(NetworkModificationException.class,
            () -> modification.check(network)).getMessage();
        assertEquals("CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR : The switch kinds list size must be the section count minus one", message);

        createVoltageLevelTopologyInfos.setSectionCount(2);
        message = assertThrows(NetworkModificationException.class,
            () -> modification.check(network)).getMessage();
        assertEquals("CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR : " + "voltage level notFoundVoltageLevel is not found", message);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return CreateVoltageLevelTopologyInfos.builder()
            .stashed(false)
            .voltageLevelId("v1")
            .sectionCount(3)
            .switchKinds(List.of(SwitchKind.BREAKER, SwitchKind.DISCONNECTOR))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        List<String> busBarIds = new ArrayList<>();
        getNetwork().getBusbarSections().forEach(busbarSection -> busBarIds.add(busbarSection.getId()));
        Assertions.assertEquals(7, busBarIds.size());
        Assertions.assertTrue(busBarIds.containsAll(List.of("v1_1_1", "v1_1_2", "v1_1_3", "bbs1", "bbs2", "bbs3", "bbs4")));
        assertTrue(getNetwork().getSwitchStream().map(Switch::getId).collect(Collectors.toSet())
            .containsAll(Set.of("l11_DISCONNECTOR_13_0", "v1_DISCONNECTOR_10_7", "ld1_BREAKER", "ld1_DISCONNECTOR_11_0",
                "v1_DISCONNECTOR_6_9", "v1_DISCONNECTOR_7_8", "v1_BREAKER_1_1", "l11_BREAKER")));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("CREATE_VOLTAGE_LEVEL_TOPOLOGY", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() {
        });
        assertEquals("v1", updatedValues.get("voltageLevelId"));
    }
}
