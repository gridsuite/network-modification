/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

import java.util.List;

/**
 * @author David Braquart<david.braquart at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Setter
public class HvdcLccDeletionModel extends AbstractEquipmentDeletionModel {

    @Builder
    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ShuntCompensatorModel {
        private String id;
        private boolean connectedToHvdc;
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ShuntCompensatorModel> mcsOnSide1;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ShuntCompensatorModel> mcsOnSide2;

}
