/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.context.FilterUtils;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.modifications.byfilter.ByFormulaModification;
import org.gridsuite.modification.modifications.data.assignment.FormulaAssignmentData;

import java.util.List;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
@JsonTypeName("BY_FORMULA_MODIFICATION")
@Schema(description = "Modification by formula")
public class ByFormulaModificationInfos extends ModificationInfos {
    @Schema(description = "Identifiable type")
    private IdentifiableType identifiableType;

    @Schema(description = "list of formulas")
    private List<FormulaInfos> formulaInfosList;

    @Override
    public ByFormulaModification toModification(ModificationContext modificationContext) {
        return ByFormulaModification.builder()
                .identifiableType(getIdentifiableType())
                .formulaAssignments(getFormulaInfosList().stream().map(formulaInfos ->
                        (FormulaAssignmentData) FormulaAssignmentData.builder()
                            .editedField(formulaInfos.getEditedField())
                            .operator(formulaInfos.getOperator())
                            .fieldOrValue1(formulaInfos.getFieldOrValue1())
                            .fieldOrValue2(formulaInfos.getFieldOrValue2())
                            .filters(FilterUtils.loadFilterWithNames(formulaInfos.getFilters(), modificationContext.filterLoader()))
                            .build())
                        .toList())
                .build();
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.byFormulaModification").add();
    }
}
