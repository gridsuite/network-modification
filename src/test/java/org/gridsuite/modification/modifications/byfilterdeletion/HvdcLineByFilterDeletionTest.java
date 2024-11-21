/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilterdeletion;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class HvdcLineByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String HVDC_LINE_ID_1 = "hvdcLine";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNull(getNetwork().getHvdcLine(HVDC_LINE_ID_1));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.HVDC_LINE;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.HVDC_LINE;
    }

    @Override
    protected String getExistingId() {
        return HVDC_LINE_ID_1;
    }

    @Override
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
                new IdentifiableAttributes(HVDC_LINE_ID_1, IdentifiableType.HVDC_LINE, null)
        )).build();
        return Map.of(FILTER_ID_1, filter1);
    }
}
