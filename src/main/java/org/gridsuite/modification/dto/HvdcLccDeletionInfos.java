/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.List;

/**
 * @author David Braquart<david.braquart at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Hvdc with Lcc deletion")
public class HvdcLccDeletionInfos extends AbstractEquipmentDeletionInfos {

    @Builder
    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ShuntCompensatorInfos {
        private String id;
        private boolean connectedToHvdc;
    }

    @Schema(description = "LCC HVDC converter station Shunt Compensator side 1")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ShuntCompensatorInfos> mcsOnSide1;

    @Schema(description = "LCC HVDC converter station Shunt Compensator side 2")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ShuntCompensatorInfos> mcsOnSide2;

}
