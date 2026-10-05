/*
 * Copyright (c) 2023-2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.dto.scaling;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.data.scaling.ScalingVariationData;
import org.gridsuite.modification.modifications.scaling.LoadScaling;

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
@ToString(callSuper = true)
@Schema(description = "Load scaling creation")
@JsonTypeName("LOAD_SCALING")
public class LoadScalingInfos extends ScalingInfos {

    @Override
    public AbstractModification toModification(ModificationContext modificationContext) {
        check();

        Map<UUID, FilterWithDistributionKeys> resolvedFilters = resolveFilters(modificationContext);
        List<ScalingVariationData> scalingVariations = getVariations().stream()
                .map(svi -> svi.toData(resolvedFilters))
                .toList();

        return LoadScaling.builder()
                .scalingVariations(scalingVariations)
                .variationType(getVariationType())
                .build();
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.loadScaling")
                .add();
    }

    @Override
    public void check() {
        super.check();

        for (ScalingVariationInfos variation : getVariations()) {
            if (variation.getReactiveVariationMode() == null) {
                createModificationAttributeMissing("reactiveVariationMode");
            }
        }
    }
}
