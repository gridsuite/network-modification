/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.filter.wip.FilterLoader;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.data.ScalingVariationData;
import org.gridsuite.modification.modifications.scaling.GeneratorScaling;

import java.util.List;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@JsonTypeName("GENERATOR_SCALING")
@Schema(description = "Generator scaling creation")
public class GeneratorScalingInfos extends ScalingInfos {

    @Override
    public AbstractModification toModification(FilterLoader filterLoader) {
        List<ScalingVariationData> scalingVariations = getVariations().stream()
                .map(svi -> svi.toData(filterLoader))
                .toList();

        return GeneratorScaling.builder()
                .scalingVariations(scalingVariations)
                .variationType(getVariationType())
                .build();
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.generatorScaling")
                .add();
    }
}
