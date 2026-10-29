/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.context.FilterUtils;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.NetworkReduction;

import java.util.List;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Network reduction")
@JsonTypeName("NETWORK_REDUCTION")
public class NetworkReductionInfos extends ModificationInfos {
    @Schema(description = "List of filters")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<FilterInfos> filters;

    @Override
    public AbstractModification toModification(ModificationContext modificationContext) {
        return NetworkReduction.builder()
                .filters(FilterUtils.loadFilterWithNames(getFilters(), modificationContext.filterLoader()))
                .build();
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.networkReduction")
                .add();
    }
}
