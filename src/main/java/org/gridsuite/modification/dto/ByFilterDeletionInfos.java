/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.ByFilterDeletion;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Antoine Bouhours <antoine.bouhours at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "By filter deletion")
@JsonTypeName("BY_FILTER_DELETION")
@ModificationErrorTypeName("BY_FILTER_DELETION_ERROR")
public class ByFilterDeletionInfos extends ModificationInfos {
    @Schema(description = "Equipment type")
    private IdentifiableType equipmentType;

    @Schema(description = "List of filters")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<FilterInfos> filters;

    @Override
    public AbstractModification toModification() {
        return new ByFilterDeletion(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.byFilter.deletion").add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("equipmentType", getEquipmentType().name());
        return mapMessageValues;
    }
}
