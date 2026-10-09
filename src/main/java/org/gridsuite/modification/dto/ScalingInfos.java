/*
 * Copyright (c) 2023-2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.error.NetworkModificationException.createModificationAttributeMissing;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Scaling infos")
@ToString(callSuper = true)
public class ScalingInfos extends ModificationInfos {
    @Schema(description = "scaling variations")
    private List<ScalingVariationInfos> variations;

    @Schema(description = "variation type")
    private VariationType variationType;

    /**
     * Resolves, in one call, every filter referenced by the variations of this scaling, with its
     * distribution keys.
     *
     * <p>Identifiers are deduplicated, so that a filter shared by several variations is only asked for
     * once. The result is the union of what every variation needs: a variation keeps only the filters it
     * references.
     */
    protected Map<UUID, FilterWithDistributionKeys> resolveFilters(ModificationContext modificationContext) {
        List<UUID> allFilterUuids = getVariations().stream()
                .flatMap(variation -> variation.getFilters().stream())
                .map(FilterInfos::getId)
                .distinct()
                .toList();
        return modificationContext.filterWithDistributionKeysLoader().load(allFilterUuids);
    }

    @Override
    public void check() {
        super.check();

        for (ScalingVariationInfos variation : getVariations()) {
            if (variation.getVariationMode() == null) {
                createModificationAttributeMissing("variationMode");
            }
            if (variation.getVariationValue() == null) {
                createModificationAttributeMissing("variationValue");
            }
        }
    }
}
