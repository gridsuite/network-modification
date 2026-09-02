/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.filter.wip.FilterLoader;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.modifications.data.ScalingVariationData;

import java.util.List;
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

    @JsonIgnore
    public ScalingVariationData toData(FilterLoader filterLoader) {
        List<UUID> filterUuids = filters.stream().map(FilterInfos::getId).distinct().toList();
        return ScalingVariationData.builder()
                .filters(filterLoader.load(filterUuids))
                .distributionKeysPerEquipmentId(filterLoader.loadDistributionKeys(filterUuids))
                .variationMode(variationMode)
                .variationValue(variationValue)
                .reactiveVariationMode(reactiveVariationMode)
                .build();
    }
}
