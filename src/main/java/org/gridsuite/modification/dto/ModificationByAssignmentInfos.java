/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.modifications.byfilter.ModificationByAssignment;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Stream;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@JsonTypeName("MODIFICATION_BY_ASSIGNMENT")
@ModificationErrorTypeName("MODIFICATION_BY_ASSIGNMENT_ERROR")
@ToString(callSuper = true)
@Schema(description = "Modification by assignment")
public class ModificationByAssignmentInfos extends ModificationInfos {
    @Schema(description = "Equipment type")
    private IdentifiableType equipmentType;

    @Schema(description = "list of modifications")
    private List<? extends AssignmentInfos<?>> assignmentInfosList;

    @Override
    public ModificationByAssignment toModification() {
        return ModificationByAssignment.builder()
                .equipmentType(getEquipmentType())
                .assignmentInfosList(getAssignmentInfosList())
                .build();
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.modificationByAssignment").add();
    }

    @Override
    public List<UUID> getReferencedFilterUuids() {
        return assignmentInfosList == null
                ? List.of()
                : assignmentInfosList.stream()
                .flatMap(a -> a.getFilters() == null ? Stream.empty() : a.getFilters().stream())
                .map(FilterInfos::getId)
                .filter(Objects::nonNull)
                .toList();
    }

    @Override
    public void embedReferencedData(Map<UUID, AbstractFilter> filters, Map<UUID, LoadFlowParametersInfos> lfParams) {
        if (assignmentInfosList == null) {
            return;
        }
        assignmentInfosList.stream()
                .flatMap(a -> a.getFilters() == null ? Stream.empty() : a.getFilters().stream())
                .forEach(ref -> {
                    if (ref.getId() != null) {
                        ref.setFilterContent(filters.get(ref.getId()));
                    }
                });
    }
}
