/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.modifications.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;

import java.util.List;
import java.util.Map;

/**
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ScalingVariationData {

    @Schema(description = "List of filters")
    private List<Filter> filters;

    @Schema(description = "Distribution key of each selected equipment")
    private Map<String, Double> distributionKeys;

    @Schema(description = "Variation mode")
    private VariationMode variationMode;

    @Schema(description = "Variation value")
    private Double variationValue;

    @Schema(description = "Reactive variation mode")
    private ReactiveVariationMode reactiveVariationMode;
}
