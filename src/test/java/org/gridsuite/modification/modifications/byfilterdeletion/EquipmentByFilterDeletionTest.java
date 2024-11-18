/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilterdeletion;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
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
class EquipmentByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String LOAD_ID_1 = "load1";
    private static final String LOAD_ID_2 = "load2";
    private static final String LOAD_ID_3 = "load3";
    private static final String LOAD_ID_4 = "load4";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createLoadNetwork(networkUuid, new NetworkFactoryImpl());
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNull(getNetwork().getLoad(LOAD_ID_1));
        assertNull(getNetwork().getLoad(LOAD_ID_2));
        assertNull(getNetwork().getLoad(LOAD_ID_3));
        assertNull(getNetwork().getLoad(LOAD_ID_4));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.LOAD;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.LOAD;
    }

    @Override
    protected String getExistingId() {
        return LOAD_ID_1;
    }

    @Override
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
            new IdentifiableAttributes(LOAD_ID_1, IdentifiableType.LOAD, null),
            new IdentifiableAttributes(LOAD_ID_2, IdentifiableType.LOAD, null)
        )).build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(
            new IdentifiableAttributes(LOAD_ID_3, IdentifiableType.LOAD, null),
            new IdentifiableAttributes(LOAD_ID_4, IdentifiableType.LOAD, null)
        )).build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2);
    }
}
