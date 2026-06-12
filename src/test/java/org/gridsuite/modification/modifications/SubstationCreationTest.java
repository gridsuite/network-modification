/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import org.gridsuite.modification.model.FreePropertyModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.SubstationCreationModel;
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.List;
import java.util.UUID;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class SubstationCreationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationModel buildModification() {
        return SubstationCreationModel.builder()
                .equipmentId("SubstationId")
                .equipmentName("SubstationName")
                .country(Country.AF)
                .properties(List.of(FreePropertyModel.builder().name("DEMO").value("DemoC").build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Substation substation = getNetwork().getSubstation("SubstationId");
        assertNotNull(substation);
        assertEquals("DemoC", substation.getProperty("DEMO"));
    }

    @Override
    protected void checkModification() {
        SubstationCreationModel substationCreationInfos = (SubstationCreationModel) buildModification();
        substationCreationInfos.setEquipmentId("");
        PowsyblException exception = assertThrows(PowsyblException.class, () -> substationCreationInfos.toModification().apply(getNetwork()));
        assertEquals("Invalid id ''", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationInfos) throws Exception {
        // assertEquals("SUBSTATION_CREATION", modificationInfos.getMessageType());
        // Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals("SubstationId", createdValues.get("equipmentId"));
    }
}
