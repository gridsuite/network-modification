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
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class VoltageLevelByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String VOLTAGE_LEVEL_ID_1 = "v1";
    private static final String VOLTAGE_LEVEL_ID_2 = "v2";
    private static final String VOLTAGE_LEVEL_ID_3 = "v3";
    private static final String VOLTAGE_LEVEL_ID_4 = "v4";
    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(VOLTAGE_LEVEL_ID_1, VOLTAGE_LEVEL_ID_2),
            FILTER_ID_2, Set.of(VOLTAGE_LEVEL_ID_3, VOLTAGE_LEVEL_ID_4)
    );

    @Override
    public Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_1));
        assertNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_2));
        assertNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_3));
        assertNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_4));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.VOLTAGE_LEVEL;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.VOLTAGE_LEVEL;
    }
}
