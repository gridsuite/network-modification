/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilterdeletion;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.impl.NetworkFactoryImpl;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.Map;
import java.util.Set;
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
    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(LOAD_ID_1, LOAD_ID_2),
            FILTER_ID_2, Set.of(LOAD_ID_3, LOAD_ID_4)
    );

    @Override
    public Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
    }

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
}
