/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;

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
public class LccConverterStationCreationModel extends InjectionCreationModel implements ModificationModel {
    @Schema(description = "Loss Factor")
    private Float lossFactor;

    @Schema(description = "Power Factor")
    private Float powerFactor;

    @Schema(description = "LCC HVDC Converter Station Shunt Compensator")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<LccShuntCompensatorModel> shuntCompensatorsOnSide;

    @Override
    public ModificationType getType() {
        return ModificationType.LCC_CONVERTER_STATION_CREATION;
    }
}
