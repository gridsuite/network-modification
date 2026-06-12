/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.RemoveSubstation;
import com.powsybl.iidm.modification.topology.RemoveSubstationBuilder;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.EquipmentDeletionModel;
import org.gridsuite.modification.model.HvdcLccDeletionModel;
import org.gridsuite.modification.model.HvdcLccDeletionModel.ShuntCompensatorModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class EquipmentDeletionTest extends AbstractNetworkModificationTest {

    @Override
    public void checkModification() {
        EquipmentDeletionModel equipmentDeletionModel = (EquipmentDeletionModel) buildModification();
        equipmentDeletionModel.setEquipmentId("notFoundLoad");
        assertThrows(NetworkModificationException.class, () -> equipmentDeletionModel.toModification().check(getNetwork()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationModel buildModification() {
        return EquipmentDeletionModel.builder()
            .equipmentType(IdentifiableType.LOAD)
            .equipmentId("v1load")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNull(getNetwork().getLoad("v1load"));
    }

    @Test
    void testOkWhenRemovingIsolatedEquipment() {

        EquipmentDeletionModel equipmentDeletionModel = EquipmentDeletionModel.builder()
            .equipmentType(IdentifiableType.LOAD)
            .equipmentId("v5load")
            .build();

        // delete load with error removing dangling switches, because the load connection node is not linked to any other node
        equipmentDeletionModel.toModification().apply(getNetwork());
        var v5 = getNetwork().getVoltageLevel("v5");
        assertThrows(PowsyblException.class, () -> v5.getNodeBreakerView().getTerminal(2));
    }

    private void deleteHvdcLineWithShuntCompensator(String shuntNameToBeRemoved, boolean selected, int side, boolean warningCase) {
        final String hvdcLineName = "hvdcLine"; // this line uses LCC converter stations
        assertNotNull(getNetwork().getHvdcLine(hvdcLineName));
        assertEquals(warningCase, getNetwork().getShuntCompensator(shuntNameToBeRemoved) == null);

        List<ShuntCompensatorModel> shuntCompensatorModel = List.of(new ShuntCompensatorModel(shuntNameToBeRemoved, selected));
        HvdcLccDeletionModel hvdcLccDeletionModel = new HvdcLccDeletionModel();
        if (side == 1) {
            hvdcLccDeletionModel.setMcsOnSide1(shuntCompensatorModel);
        } else {
            hvdcLccDeletionModel.setMcsOnSide2(shuntCompensatorModel);
        }
        EquipmentDeletionModel equipmentDeletionModel = EquipmentDeletionModel.builder()
            .equipmentType(IdentifiableType.HVDC_LINE)
            .equipmentId(hvdcLineName)
            .equipmentInfos(hvdcLccDeletionModel)
            .build();

        equipmentDeletionModel.toModification().apply(getNetwork());

        assertNull(getNetwork().getHvdcLine(hvdcLineName));
        assertEquals(selected, getNetwork().getShuntCompensator(shuntNameToBeRemoved) == null);
    }

    @CsvSource({"true,  1", "true,  2", "false, 1", "false, 2"})
    @ParameterizedTest(name = ParameterizedTest.ARGUMENTS_WITH_NAMES_PLACEHOLDER)
    void testDeleteHvdcWithLCCWithShuntCompensator(final boolean selected, final int side) {
        deleteHvdcLineWithShuntCompensator("v2shunt", selected, side, false);
    }

    @Test
    void testDeleteHvdcWithLCCWithAlreadyDeletedShuntCompensator() {
        // we select a nonexistent shunt: will produce a warning
        deleteHvdcLineWithShuntCompensator("deletedOrMissingShuntId", true, 1, true);
    }

    @Test
    void testRemoveUnknownSubstation() {
        Network network = Network.create("empty", "test");
        RemoveSubstation removeSubstation = new RemoveSubstationBuilder().withSubstationId("unknownSubstation").build();
        PowsyblException e = assertThrows(PowsyblException.class, () -> removeSubstation.apply(network, true, ReportNode.NO_OP));
        assertEquals("Substation not found: unknownSubstation", e.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        // assertEquals("EQUIPMENT_DELETION", modificationModel.getMessageType());
        // Map<String, String> createdValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals("v1load", createdValues.get("equipmentId"));
    }
}
