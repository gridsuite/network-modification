/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.powsybl.iidm.network.Country;
import com.powsybl.loadflow.LoadFlowParameters;
import org.gridsuite.modification.model.BalancesAdjustmentAreaModel;
import org.gridsuite.modification.model.BalancesAdjustmentModificationModel;
import org.gridsuite.modification.model.ShiftEquipmentType;
import org.gridsuite.modification.model.ShiftType;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BalancesAdjustmentModificationModelTest {

    @Test
    void buildModificationModelWithDefaultValues() {
        BalancesAdjustmentModificationModel infos = BalancesAdjustmentModificationModel.builder()
            .areas(List.of(BalancesAdjustmentAreaModel.builder()
                .name("FR")
                .countries(List.of(Country.FR))
                .netPosition(500d)
                .shiftEquipmentType(ShiftEquipmentType.LOAD)
                .shiftType(ShiftType.BALANCED)
                .build()))
            .build();

        assertEquals(BalancesAdjustmentModificationModel.DEFAULT_MAX_NUMBER_ITERATIONS, infos.getMaxNumberIterations());
        assertEquals(BalancesAdjustmentModificationModel.DEFAULT_THRESHOLD_NET_POSITION, infos.getThresholdNetPosition());
        assertEquals(BalancesAdjustmentModificationModel.DEFAULT_COUNTRIES_TO_BALANCE, infos.getCountriesToBalance());
        assertEquals(BalancesAdjustmentModificationModel.DEFAULT_BALANCE_TYPE, infos.getBalanceType());
        assertEquals(BalancesAdjustmentModificationModel.DEFAULT_WITH_LOAD_FLOW, infos.isWithLoadFlow());
        assertEquals(BalancesAdjustmentModificationModel.DEFAULT_WITH_RATIO_TAP_CHANGERS, infos.isWithRatioTapChangers());
        assertEquals(BalancesAdjustmentModificationModel.DEFAULT_SUBTRACT_LOAD_FLOW_BALANCING, infos.isSubtractLoadFlowBalancing());
        assertEquals(1, infos.getAreas().size());

        BalancesAdjustmentAreaModel areaModel = infos.getAreas().get(0);
        assertEquals("FR", areaModel.getName());
        assertEquals(500d, areaModel.getNetPosition(), 0.001);
        assertEquals(ShiftType.BALANCED, areaModel.getShiftType());
        assertEquals(ShiftEquipmentType.LOAD, areaModel.getShiftEquipmentType());
        assertEquals(1, areaModel.getCountries().size());
        assertEquals(Country.FR, areaModel.getCountries().get(0));
    }

    @Test
    void buildModificationModelWithoutDefaultValues() {
        BalancesAdjustmentModificationModel infos = BalancesAdjustmentModificationModel.builder()
            .areas(List.of(BalancesAdjustmentAreaModel.builder()
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
            .withRatioTapChangers(true)
            .subtractLoadFlowBalancing(true)
            .build();

        assertEquals(10, infos.getMaxNumberIterations());
        assertEquals(250d, infos.getThresholdNetPosition(), 0.001);
        assertEquals(List.of(Country.FR, Country.DE), infos.getCountriesToBalance());
        assertEquals(LoadFlowParameters.BalanceType.PROPORTIONAL_TO_GENERATION_P, infos.getBalanceType());
        assertTrue(infos.isWithLoadFlow());
        assertTrue(infos.isWithRatioTapChangers());
        assertTrue(infos.isSubtractLoadFlowBalancing());
        assertEquals(1, infos.getAreas().size());

        BalancesAdjustmentAreaModel areaModel = infos.getAreas().get(0);
        assertEquals("FR", areaModel.getName());
        assertEquals(500d, areaModel.getNetPosition(), 0.001);
        assertEquals(ShiftType.BALANCED, areaModel.getShiftType());
        assertEquals(ShiftEquipmentType.LOAD, areaModel.getShiftEquipmentType());
        assertEquals(1, areaModel.getCountries().size());
        assertEquals(Country.FR, areaModel.getCountries().get(0));
    }

}
