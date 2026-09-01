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
class HvdcLineByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String HVDC_LINE_ID_1 = "hvdcLine";
    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(HVDC_LINE_ID_1)
    );

    @Override
    public Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
    }

    @Override
    public Set<String> getExistingEquipments() {
        return Set.of(HVDC_LINE_ID_1);
    }

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
}
