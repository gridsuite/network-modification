/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.TwoWindingsTransformerModification;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Two windings transformer modification")
@ModificationErrorTypeName("MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR")
public class TwoWindingsTransformerModificationModel extends BranchModificationModel implements ModificationModel {

    @Schema(description = "Magnetizing conductance")
    private AttributeModification<Double> g;

    @Schema(description = "Magnetizing susceptance")
    private AttributeModification<Double> b;

    @Schema(description = "Side 1 rated voltage")
    private AttributeModification<Double> ratedU1;

    @Schema(description = "Side 2 rated voltage")
    private AttributeModification<Double> ratedU2;

    @Schema(description = "Rated conductance in Siemens")
    private AttributeModification<Double> ratedS;

    @Schema(description = "Ratio tap changer")
    @Builder.Default
    private RatioTapChangerModificationModel ratioTapChanger = new RatioTapChangerModificationModel();

    @Schema(description = "Phase tap changer")
    @Builder.Default
    private PhaseTapChangerModificationModel phaseTapChanger = new PhaseTapChangerModificationModel();

    @Schema(description = "Ratio tap changer to be estimated status")
    private AttributeModification<Boolean> ratioTapChangerToBeEstimated;

    @Schema(description = "Phase tap changer to be estimated status")
    private AttributeModification<Boolean> phaseTapChangerToBeEstimated;

    @Override
    public AbstractModification toModification() {
        return new TwoWindingsTransformerModification(this);
    }

    @Override
    public ModificationType getType() {
        return ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION;
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.twoWindingsTransformerModification.modification")
                .withUntypedValue("twoWindingsTransformerId", getEquipmentId())
                .add();
    }
}
