/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Lcc converter station modification")
@JsonTypeName("LCC_CONVERTER_STATION_MODIFICATION")
public class LccConverterStationModificationInfos extends InjectionModificationInfos {
    @Schema(description = "Loss Factor")
    private AttributeModification<Float> lossFactor;

    @Schema(description = "Power Factor")
    private AttributeModification<Float> powerFactor;

    @Schema(description = "LCC HVDC Converter Station Shunt Compensator")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<LccShuntCompensatorModificationInfos> shuntCompensatorsOnSide;

    public boolean hasModifications() {
        return getEquipmentName() != null || lossFactor != null || powerFactor != null || !CollectionUtils.isEmpty(shuntCompensatorsOnSide);
    }
}
