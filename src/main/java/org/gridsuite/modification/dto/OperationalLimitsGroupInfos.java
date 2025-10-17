/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString
@Schema(description = "Operational limits group")
public class OperationalLimitsGroupInfos {
    @Schema(description = "Operational limit group id")
    private String id;

    @Schema(description = "Current limits")
    private CurrentLimitsInfos currentLimits;

    @Schema(description = "application side")
    private Applicability applicability;

    @Schema(description = "limits properties")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<LimitsPropertyInfos> limitsProperties;

    public enum Applicability {
        EQUIPMENT, // SIDE1 + SIDE2
        SIDE1,
        SIDE2,
    }
}
