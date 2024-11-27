/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Lcc converter station creation")
@JsonTypeName("LCC_CONVERTER_STATION_CREATION")
@ModificationErrorTypeName("LCC_CREATE_CONVERTER_STATION_ERROR")
public class LccConverterStationCreationInfos extends InjectionCreationInfos {
    @Builder
    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ShuntCompensatorInfos {
        private String shuntCompensatorId;
        private String shuntCompensatorName;
        private Double maxQAtNominalV;
        private Boolean connectedToHvdc;
    }

    @Schema(description = "Loss Factor")
    private Float lossFactor;

    @Schema(description = "Power Factor")
    private Float powerFactor;

    @Schema(description = "LCC HVDC Converter Station Shunt Compensator")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ShuntCompensatorInfos> mcsOnSide;
}
