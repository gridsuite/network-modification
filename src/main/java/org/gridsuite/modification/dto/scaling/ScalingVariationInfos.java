/*
 * Copyright (c) 2023-2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.dto.scaling;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.gridsuite.modification.context.utils.FilterUtils;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.modifications.data.scaling.ScalingVariationData;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Getter
@Setter
@ToString
@SuperBuilder
@EqualsAndHashCode
@NoArgsConstructor
@Schema(description = "Scaling creation")
public class ScalingVariationInfos {
    @Schema(description = "id")
    private UUID id;

    @Schema(description = "filters")
    private List<FilterInfos> filters;

    @Schema(description = "variation mode")
    private VariationMode variationMode;

    @Schema(description = "variation value")
    private Double variationValue;

    @Schema(description = "reactiveVariationMode")
    private ReactiveVariationMode reactiveVariationMode;

    /**
     * Builds the data of this variation from the filters already resolved for the whole scaling.
     *
     * <p>The given map is shared by all the variations, so only the references of <b>this</b> variation are
     * looked up. Being shared, a filter used by several variations is the same
     * {@link org.gridsuite.filter.wip.Filter} instance in each of them, and its name is set again for every
     * variation: the name of the last one referencing it wins.
     */
    @JsonIgnore
    public ScalingVariationData toData(Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys) {
        return ScalingVariationData.builder()
                .filters(FilterUtils.loadFiltersWithDistributionKeys(getFilters(), filtersWithDistributionKeys))
                .variationMode(variationMode)
                .variationValue(variationValue)
                .reactiveVariationMode(reactiveVariationMode)
                .build();
    }
}
