/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.dto.ShiftEquipmentType;
import org.gridsuite.modification.dto.ShiftType;
import org.gridsuite.modification.dto.BalancesAdjustmentAreaInfos;
import org.gridsuite.modification.dto.BalancesAdjustmentModificationInfos;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
class BalancesAdjustmentModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return Network.read("fourSubstationsNb_country 2_N1.xiidm", getClass().getResourceAsStream("/fourSubstationsNb_country 2_N1.xiidm"));
    }

    @Override
    protected BalancesAdjustmentModificationInfos buildModification() {
        return BalancesAdjustmentModificationInfos.builder()
            .areas(List.of(
                BalancesAdjustmentAreaInfos.builder()
                    .name("FR")
                    .countries(List.of(Country.FR))
                    .netPosition(80d)
                    .shiftType(ShiftType.PROPORTIONAL)
                    .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                    .build()
            ))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(-82.4d, getNetwork().getGenerator("GH1").getTerminal().getP(), 0.1);
        assertEquals(-84.1d, getNetwork().getGenerator("GH2").getTerminal().getP(), 0.1);
        assertEquals(-149.8d, getNetwork().getGenerator("GH3").getTerminal().getP(), 0.1);
    }

    @Override
    protected void checkModification() {

    }
}
