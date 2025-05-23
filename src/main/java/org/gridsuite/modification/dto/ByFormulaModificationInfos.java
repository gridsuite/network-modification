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
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.modifications.byfilter.ByFormulaModification;

import java.util.List;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@JsonTypeName("BY_FORMULA_MODIFICATION")
@ModificationErrorTypeName("BY_FORMULA_MODIFICATION_ERROR")
@ToString(callSuper = true)
@Schema(description = "Modification by formula")
public class ByFormulaModificationInfos extends ModificationInfos {
    @Schema(description = "Identifiable type")
    private IdentifiableType identifiableType;

    @Schema(description = "list of formulas")
    private List<FormulaInfos> formulaInfosList;

    @Override
    public ByFormulaModification toModification() {
        return new ByFormulaModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.byFormulaModification").add();
    }
}
