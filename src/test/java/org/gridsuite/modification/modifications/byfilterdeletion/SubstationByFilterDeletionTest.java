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
class SubstationByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String SUBSTATION_ID_1 = "s1";
    private static final String SUBSTATION_ID_2 = "s2";
    private static final String SUBSTATION_ID_3 = "s3";
    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(SUBSTATION_ID_1, SUBSTATION_ID_2),
            FILTER_ID_2, Set.of(SUBSTATION_ID_3)
    );

    @Override
    public Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
    }

    @Override
    public Set<String> getExistingEquipments() {
        return Set.of(SUBSTATION_ID_1, SUBSTATION_ID_2, SUBSTATION_ID_3);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNull(getNetwork().getSubstation(SUBSTATION_ID_1));
        assertNull(getNetwork().getSubstation(SUBSTATION_ID_2));
        assertNull(getNetwork().getSubstation(SUBSTATION_ID_3));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.SUBSTATION;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.SUBSTATION;
    }
}
