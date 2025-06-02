/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.powsybl.iidm.network.Country;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString
@Schema(name = "balances adjustment area description")
public class BalancesAdjustmentAreaInfos {

    @Schema(name = "name")
    private String name;

    @Schema(name = "countries")
    List<Country> countries;

    @Schema(name = "net position")
    Double netPosition;

    @Schema(name = "type of equipments to shift")
    ShiftEquipmentType shiftEquipmentType;

    @Schema(name = "shift type")
    ShiftType shiftType;
}
