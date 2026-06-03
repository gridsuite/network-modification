/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.OperatingStatusModificationModel;
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
class OperatingStatusModificationTrip2WTransformerTest extends AbstractNetworkModificationTest {
    private static final String TARGET_BRANCH_ID = "trf1";
    private static final OperatingStatus.Status TARGET_BRANCH_STATUS = FORCED_OUTAGE;
    private static final OperatingStatus.Status OTHER_BRANCH_STATUS = PLANNED_OUTAGE;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a branch status different from the expected one, after testCreate
        TestUtils.setOperatingStatus(network, TARGET_BRANCH_ID, OTHER_BRANCH_STATUS);
        return network;
    }

    @Override
    protected ModificationModel buildModification() {
        return OperatingStatusModificationModel.builder()
                .equipmentId(TARGET_BRANCH_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationModel.ActionType.TRIP).build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_BRANCH_ID, TARGET_BRANCH_STATUS);
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationModel.getType().toString());
        Map<String, String> createdValues = modificationModel.getMapMessageValues();
        assertEquals("energizedVoltageLevelId", createdValues.get("energizedVoltageLevelId"));
        assertEquals("TRIP", createdValues.get("action"));
        assertEquals("trf1", createdValues.get("equipmentId"));
    }

    @Override
    protected void checkModification() {
    }
}
