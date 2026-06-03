/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.OperatingStatus;

import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.OperatingStatusModificationModel;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.gridsuite.modification.utils.TestUtils;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.UUID;

import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.FORCED_OUTAGE;
import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.PLANNED_OUTAGE;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class OperatingStatusModificationTripLineTest extends AbstractNetworkModificationTest {
    private static final String TARGET_LINE_ID = "line2";
    private static final OperatingStatus.Status TARGET_BRANCH_STATUS = FORCED_OUTAGE;
    private static final OperatingStatus.Status OTHER_BRANCH_STATUS = PLANNED_OUTAGE;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a branch status different from the expected one, after testCreate
        TestUtils.setOperatingStatus(network, TARGET_LINE_ID, OTHER_BRANCH_STATUS);
        return network;
    }

    @Override
    protected ModificationModel buildModification() {
        return OperatingStatusModificationModel.builder()
                .equipmentId(TARGET_LINE_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationModel.ActionType.TRIP).build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_LINE_ID, TARGET_BRANCH_STATUS);
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationModel.getType().toString());
        Map<String, String> createdValues = modificationModel.getMapMessageValues();
        assertEquals("energizedVoltageLevelId", createdValues.get("energizedVoltageLevelId"));
        assertEquals("TRIP", createdValues.get("action"));
        assertEquals("line2", createdValues.get("equipmentId"));
    }

    @Override
    protected void checkModification() {
        OperatingStatusModificationModel modification = (OperatingStatusModificationModel) buildModification();
        modification.setEquipmentId("NotFoundEquipmentId");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> modification.toModification().check(getNetwork()));
        assertEquals("EQUIPMENT_NOT_FOUND : NotFoundEquipmentId", exception.getMessage());
    }

    @Test
    void testCreateSubReportNode() {
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build();

        OperatingStatusModificationModel modification = (OperatingStatusModificationModel) buildModification();

        modification.createSubReportNode(reportNode);
        assertLogMessage("Trip " + TARGET_LINE_ID, "network.modification.OPERATING_STATUS_MODIFICATION_TRIP", reportNode);
    }
}
