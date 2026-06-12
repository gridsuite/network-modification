/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.SwitchKind;
import org.gridsuite.modification.model.CouplingDeviceModel;
import org.gridsuite.modification.model.CreateCouplingDeviceModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkWithTeePoint;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createBusBarSection;

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
    protected ModificationModel buildModification() {
        return CreateCouplingDeviceModel.builder()
            .voltageLevelId("v1")
            .couplingDeviceInfos(CouplingDeviceModel.builder()
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
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        // assertEquals("ADD_COUPLING_DEVICE", modificationModel.getMessageType());
        // Map<String, String> updatedValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals("v1", updatedValues.get("voltageLevelId"));
    }

    @Test
    void testCreateCouplingDeviceFail() {
        CreateCouplingDeviceModel createCouplingDeviceModel = CreateCouplingDeviceModel.builder()
            .voltageLevelId("v1")
            .couplingDeviceInfos(CouplingDeviceModel.builder()
                .busbarSectionId1("bbs1")
                .busbarSectionId2("bbs2")
                .build())
            .build();
        // Map<String, String> updatedValues = createCouplingDeviceModel.getMapMessageValues();
        // assertEquals("v1", updatedValues.get("voltageLevelId"));
        Network network = getNetwork();
        AbstractModification modification = createCouplingDeviceModel.toModification();
        ReportNode report = ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test")
            .build();
        Assertions.assertDoesNotThrow(() -> modification.apply(network, report));
        Assertions.assertEquals(1, report.getChildren().size());
        Assertions.assertEquals("core.iidm.modification.unexpectedDifferentVoltageLevels", report.getChildren().getFirst().getMessageKey());
    }
}
