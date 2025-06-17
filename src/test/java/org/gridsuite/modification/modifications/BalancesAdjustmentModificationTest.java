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
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
class BalancesAdjustmentModificationTest extends AbstractNetworkModificationTest {
    private static final double PRECISION = 1d;

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
                    .netPosition(-45d)
                    .shiftType(ShiftType.PROPORTIONAL)
                    .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                    .build(),
                BalancesAdjustmentAreaInfos.builder()
                    .name("NE")
                    .countries(List.of(Country.NE))
                    .netPosition(-54d)
                    .shiftType(ShiftType.BALANCED)
                    .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                    .build(),
                BalancesAdjustmentAreaInfos.builder()
                    .name("GE")
                    .countries(List.of(Country.GE))
                    .netPosition(0d)
                    .shiftType(ShiftType.PROPORTIONAL)
                    .shiftEquipmentType(ShiftEquipmentType.LOAD)
                    .build(),
                BalancesAdjustmentAreaInfos.builder()
                    .name("AU")
                    .countries(List.of(Country.AU))
                    .netPosition(100d)
                    .shiftType(ShiftType.BALANCED)
                    .shiftEquipmentType(ShiftEquipmentType.LOAD)
                    .build()
            ))
            .withLoadFlow(true)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(-58d, getNetwork().getGenerator("GH1").getTerminal().getP(), PRECISION);
        assertEquals(-36d, getNetwork().getGenerator("GH2").getTerminal().getP(), PRECISION);
        assertEquals(-102d, getNetwork().getGenerator("GH3").getTerminal().getP(), PRECISION);
        assertEquals(-100d, getNetwork().getGenerator("GTH1").getTerminal().getP(), PRECISION);
        assertEquals(-147d, getNetwork().getGenerator("GTH2").getTerminal().getP(), PRECISION);

        assertEquals(80d, getNetwork().getLoad("LD1").getTerminal().getP(), PRECISION);
        assertEquals(60d, getNetwork().getLoad("LD2").getTerminal().getP(), PRECISION);
        assertEquals(60d, getNetwork().getLoad("LD3").getTerminal().getP(), PRECISION);
        assertEquals(40d, getNetwork().getLoad("LD4").getTerminal().getP(), PRECISION);
        assertEquals(201d, getNetwork().getLoad("LD5").getTerminal().getP(), PRECISION);
        assertEquals(0d, getNetwork().getLoad("LD6").getTerminal().getP(), PRECISION);
    }

    @Override
    protected void checkModification() {
        // No checks implemented for this modification
    }

    @Test
    void testApplyWithoutLoadFlow() {
        var infos = BalancesAdjustmentModificationInfos.builder()
            .areas(List.of(
                BalancesAdjustmentAreaInfos.builder()
                    .name("FR")
                    .countries(List.of(Country.FR))
                    .netPosition(-45d)
                    .shiftType(ShiftType.PROPORTIONAL)
                    .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                    .build(),
                BalancesAdjustmentAreaInfos.builder()
                    .name("NE")
                    .countries(List.of(Country.NE))
                    .netPosition(-54d)
                    .shiftType(ShiftType.BALANCED)
                    .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                    .build(),
                BalancesAdjustmentAreaInfos.builder()
                    .name("GE")
                    .countries(List.of(Country.GE))
                    .netPosition(0d)
                    .shiftType(ShiftType.PROPORTIONAL)
                    .shiftEquipmentType(ShiftEquipmentType.LOAD)
                    .build(),
                BalancesAdjustmentAreaInfos.builder()
                    .name("AU")
                    .countries(List.of(Country.AU))
                    .netPosition(100d)
                    .shiftType(ShiftType.BALANCED)
                    .shiftEquipmentType(ShiftEquipmentType.LOAD)
                    .build()
            ))
            .withLoadFlow(false)
            .build();

        infos.toModification().apply(getNetwork(), false);

        assertEquals(58.d, getNetwork().getGenerator("GH1").getTargetP(), PRECISION);
        assertEquals(36d, getNetwork().getGenerator("GH2").getTargetP(), PRECISION);
        assertEquals(102d, getNetwork().getGenerator("GH3").getTargetP(), PRECISION);
        assertEquals(100d, getNetwork().getGenerator("GTH1").getTargetP(), PRECISION);
        assertEquals(147d, getNetwork().getGenerator("GTH2").getTargetP(), PRECISION);

        assertEquals(80d, getNetwork().getLoad("LD1").getP0(), PRECISION);
        assertEquals(60d, getNetwork().getLoad("LD2").getP0(), PRECISION);
        assertEquals(60d, getNetwork().getLoad("LD3").getP0(), PRECISION);
        assertEquals(40d, getNetwork().getLoad("LD4").getP0(), PRECISION);
        assertEquals(201d, getNetwork().getLoad("LD5").getP0(), PRECISION);
        assertEquals(0d, getNetwork().getLoad("LD6").getP0(), PRECISION);
    }
}
