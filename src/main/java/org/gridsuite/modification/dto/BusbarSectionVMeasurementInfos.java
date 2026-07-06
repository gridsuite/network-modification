/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

/**
 * @author Mohamed Ben Rejeb <mohamed.ben-rejeb at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Busbar section voltage measurement modification")
public class BusbarSectionVMeasurementInfos {
    @Schema(description = "Busbar section id")
    private String busbarSectionId;

    @Schema(description = "Voltage measurement value")
    @JsonProperty("vMeasurementValue")
    private AttributeModification<Double> vMeasurementValue;

    @Schema(description = "Voltage measurement validity")
    @JsonProperty("vMeasurementValidity")
    private AttributeModification<Boolean> vMeasurementValidity;
}
