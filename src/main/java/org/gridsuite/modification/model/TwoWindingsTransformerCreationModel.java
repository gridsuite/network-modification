/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.TwoWindingsTransformerCreation;

/**
 * @author Abdelsalem Hedhili <abdelsalem.hedhili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Two windings transformer creation")
@ModificationErrorTypeName("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR")
public class TwoWindingsTransformerCreationModel extends BranchCreationModel implements ModificationModel {

    @Schema(description = "Magnetizing conductance")
    private double g;

    @Schema(description = "Magnetizing susceptance")
    private double b;

    @Schema(description = "side 1 rated voltage")
    private double ratedU1;

    @Schema(description = "side 2 rated voltage")
    private double ratedU2;

    @Schema(description = "Rated conductance in Siemens")
    private Double ratedS;

    @Schema(description = "Ratio tap changer")
    private RatioTapChangerCreationModel ratioTapChanger;

    @Schema(description = "Phase tap changer")
    private PhaseTapChangerCreationModel phaseTapChanger;

    @Override
    public AbstractModification toModification() {
        return new TwoWindingsTransformerCreation(this);
    }

    @Override
    public ModificationType getType() {
        return ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION;
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.twoWindingsTransformerCreation")
                .withUntypedValue("twoWindingsTransformerId", getEquipmentId())
                .add();
    }

}
