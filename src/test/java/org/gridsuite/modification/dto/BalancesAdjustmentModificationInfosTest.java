/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.powsybl.iidm.network.Country;
import com.powsybl.loadflow.LoadFlowParameters;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class BalancesAdjustmentModificationInfosTest {

    @Test
    void buildModificationInfosWithDefaultValues() {
        BalancesAdjustmentModificationInfos infos = BalancesAdjustmentModificationInfos.builder()
            .areas(List.of(BalancesAdjustmentAreaInfos.builder()
                .name("FR")
                .countries(List.of(Country.FR))
                .netPosition(500d)
                .shiftEquipmentType(ShiftEquipmentType.LOAD)
                .shiftType(ShiftType.BALANCED)
                .build()))
            .build();

        assertEquals(BalancesAdjustmentModificationInfos.DEFAULT_MAX_NUMBER_ITERATIONS, infos.getMaxNumberIterations());
        assertEquals(BalancesAdjustmentModificationInfos.DEFAULT_THRESHOLD_NET_POSITION, infos.getThresholdNetPosition());
        assertEquals(BalancesAdjustmentModificationInfos.DEFAULT_COUNTRIES_TO_BALANCE, infos.getCountriesToBalance());
        assertEquals(BalancesAdjustmentModificationInfos.DEFAULT_BALANCE_TYPE, infos.getBalanceType());
        assertEquals(BalancesAdjustmentModificationInfos.DEFAULT_WITH_LOAD_FLOW, infos.isWithLoadFlow());
        assertEquals(1, infos.getAreas().size());

        BalancesAdjustmentAreaInfos areaInfos = infos.getAreas().get(0);
        assertEquals("FR", areaInfos.getName());
        assertEquals(500d, areaInfos.getNetPosition(), 0.001);
        assertEquals(ShiftType.BALANCED, areaInfos.getShiftType());
        assertEquals(ShiftEquipmentType.LOAD, areaInfos.getShiftEquipmentType());
        assertEquals(1, areaInfos.getCountries().size());
        assertEquals(Country.FR, areaInfos.getCountries().get(0));
    }

    @Test
    void buildModificationInfosWithoutDefaultValues() {
        BalancesAdjustmentModificationInfos infos = BalancesAdjustmentModificationInfos.builder()
            .areas(List.of(BalancesAdjustmentAreaInfos.builder()
                .name("FR")
                .countries(List.of(Country.FR))
                .netPosition(500d)
                .shiftEquipmentType(ShiftEquipmentType.LOAD)
                .shiftType(ShiftType.BALANCED)
                .build()))
            .maxNumberIterations(10)
            .thresholdNetPosition(250d)
            .countriesToBalance(List.of(Country.FR, Country.DE))
            .balanceType(LoadFlowParameters.BalanceType.PROPORTIONAL_TO_GENERATION_P)
            .withLoadFlow(true)
            .build();

        assertEquals(10, infos.getMaxNumberIterations());
        assertEquals(250d, infos.getThresholdNetPosition(), 0.001);
        assertEquals(List.of(Country.FR, Country.DE), infos.getCountriesToBalance());
        assertEquals(LoadFlowParameters.BalanceType.PROPORTIONAL_TO_GENERATION_P, infos.getBalanceType());
        assertTrue(infos.isWithLoadFlow());
        assertEquals(1, infos.getAreas().size());

        BalancesAdjustmentAreaInfos areaInfos = infos.getAreas().get(0);
        assertEquals("FR", areaInfos.getName());
        assertEquals(500d, areaInfos.getNetPosition(), 0.001);
        assertEquals(ShiftType.BALANCED, areaInfos.getShiftType());
        assertEquals(ShiftEquipmentType.LOAD, areaInfos.getShiftEquipmentType());
        assertEquals(1, areaInfos.getCountries().size());
        assertEquals(Country.FR, areaInfos.getCountries().get(0));
    }

}
