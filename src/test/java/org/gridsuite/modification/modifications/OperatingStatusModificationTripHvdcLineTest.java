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
import com.powsybl.iidm.network.extensions.OperatingStatus;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import org.gridsuite.modification.utils.TestUtils;

import java.util.Map;
import java.util.UUID;

import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.FORCED_OUTAGE;
import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.PLANNED_OUTAGE;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class OperatingStatusModificationTripHvdcLineTest extends AbstractNetworkModificationTest {
    private static final String TARGET_HVDC_LINE_ID = "hvdcLine";

    private static final OperatingStatus.Status TARGET_HVDC_LINE_STATUS = FORCED_OUTAGE;
    private static final OperatingStatus.Status OTHER_HVDC_LINE_STATUS = PLANNED_OUTAGE;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force operating status different from the expected one, after testCreate
        TestUtils.setOperatingStatus(network, TARGET_HVDC_LINE_ID, OTHER_HVDC_LINE_STATUS);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(TARGET_HVDC_LINE_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationInfos.ActionType.TRIP).build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_HVDC_LINE_ID, TARGET_HVDC_LINE_STATUS);
        assertTerminalsStatusAfterNetworkModification(false);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelId", createdValues.get("energizedVoltageLevelId"));
        assertEquals("TRIP", createdValues.get("action"));
        assertEquals("hvdcLine", createdValues.get("equipmentId"));
    }

    private void assertTerminalsStatusAfterNetworkModification(boolean shouldBeConnected) {
        HvdcLine hvdcLine = getNetwork().getHvdcLine(TARGET_HVDC_LINE_ID);
        assertEquals(hvdcLine.getConverterStation1().getTerminal().isConnected(), shouldBeConnected);
        assertEquals(hvdcLine.getConverterStation2().getTerminal().isConnected(), shouldBeConnected);
    }

    @Override
    protected void checkModification() {
    }
}
