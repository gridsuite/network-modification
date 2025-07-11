/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Operational limits group")
public class OperationalLimitsGroupModificationInfos {
    @Schema(description = "Operational limit group id")
    private String id;

    @Schema(description = "Current limits")
    CurrentLimitsModificationInfos currentLimits;

    @Schema(description = "modification type")
    private OperationalLimitsGroupModificationType modificationType;

    @Schema(description = "operation group side")
    private String side;
}
