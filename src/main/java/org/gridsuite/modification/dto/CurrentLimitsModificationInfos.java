/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
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

import java.util.List;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Current Limits")
public class CurrentLimitsModificationInfos {

    @Schema(description = "Permanent current limit")
    private Double permanentLimit;

    @Schema(description = "Temporary current limits")
    private List<CurrentTemporaryLimitModificationInfos> temporaryLimits;

    public boolean isThisLimitDeleted(int acceptableDuration) {
        return temporaryLimits.stream()
                .filter(temporaryLimit -> temporaryLimit.getAcceptableDuration() != null)
                .anyMatch(temporaryLimit -> temporaryLimit.getAcceptableDuration() == acceptableDuration && temporaryLimit.getModificationType() == TemporaryLimitModificationType.DELETE);
    }

}
