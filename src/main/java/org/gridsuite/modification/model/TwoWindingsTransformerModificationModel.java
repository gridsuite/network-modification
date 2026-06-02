/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import lombok.*;
import lombok.experimental.SuperBuilder;

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
@JsonTypeName("TWO_WINDINGS_TRANSFORMER_MODIFICATION")
@ModificationErrorTypeName("MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR")
public class TwoWindingsTransformerModificationModel extends BranchModificationModel {

    private AttributeModification<Double> g;

    private AttributeModification<Double> b;

    private AttributeModification<Double> ratedU1;

    private AttributeModification<Double> ratedU2;

    private AttributeModification<Double> ratedS;

    @Builder.Default
    private RatioTapChangerModificationModel ratioTapChanger = new RatioTapChangerModificationModel();

    @Builder.Default
    private PhaseTapChangerModificationModel phaseTapChanger = new PhaseTapChangerModificationModel();

    private AttributeModification<Boolean> ratioTapChangerToBeEstimated;

    private AttributeModification<Boolean> phaseTapChangerToBeEstimated;

    @Override
    public AbstractModification toModification() {
        return new TwoWindingsTransformerModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.twoWindingsTransformerModification.modification")
                .withUntypedValue("twoWindingsTransformerId", getEquipmentId())
                .add();
    }
}
