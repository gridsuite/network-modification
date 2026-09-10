/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 */
@SuperBuilder
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Schema(description = "Voltage init battery modification infos")

public class VoltageInitBatteryModificationInfos {
    @Schema(description = "Battery id")
    private String batteryId;

    @Schema(description = "Voltage set point")
    private Double targetV;

    @Schema(description = "Reactive power set point")
    private Double targetQ;
}
