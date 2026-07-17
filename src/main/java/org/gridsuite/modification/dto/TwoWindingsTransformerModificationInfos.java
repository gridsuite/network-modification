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
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
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
@JsonTypeName("TWO_WINDINGS_TRANSFORMER_MODIFICATION")
@ModificationErrorTypeName("MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR")
public class TwoWindingsTransformerModificationInfos extends BranchModificationInfos {

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
    private RatioTapChangerModificationInfos ratioTapChanger = new RatioTapChangerModificationInfos();

    @Schema(description = "Phase tap changer")
    @Builder.Default
    private PhaseTapChangerModificationInfos phaseTapChanger = new PhaseTapChangerModificationInfos();

    @Schema(description = "Ratio tap changer to be estimated status")
    private AttributeModification<Boolean> ratioTapChangerToBeEstimated;

    @Schema(description = "Phase tap changer to be estimated status")
    private AttributeModification<Boolean> phaseTapChangerToBeEstimated;

    @Override
    public AbstractModification toModification() {
        return TwoWindingsTransformerModification.builder()
                .equipmentId(getEquipmentId())
                .properties(getProperties())
                .equipmentName(getEquipmentName())
                .r(getR())
                .x(getX())
                .operationalLimitsGroupsModificationType(getOperationalLimitsGroupsModificationType())
                .enableOLGModification(getEnableOLGModification())
                .operationalLimitsGroups(getOperationalLimitsGroups())
                .selectedOperationalLimitsGroupId1(getSelectedOperationalLimitsGroupId1())
                .selectedOperationalLimitsGroupId2(getSelectedOperationalLimitsGroupId2())
                .voltageLevelId1(getVoltageLevelId1())
                .voltageLevelId2(getVoltageLevelId2())
                .busOrBusbarSectionId1(getBusOrBusbarSectionId1())
                .busOrBusbarSectionId2(getBusOrBusbarSectionId2())
                .connectionName1(getConnectionName1())
                .connectionName2(getConnectionName2())
                .connectionDirection1(getConnectionDirection1())
                .connectionDirection2(getConnectionDirection2())
                .connectionPosition1(getConnectionPosition1())
                .connectionPosition2(getConnectionPosition2())
                .terminal1Connected(getTerminal1Connected())
                .terminal2Connected(getTerminal2Connected())
                .p1MeasurementValue(getP1MeasurementValue())
                .p1MeasurementValidity(getP1MeasurementValidity())
                .p2MeasurementValue(getP2MeasurementValue())
                .p2MeasurementValidity(getP2MeasurementValidity())
                .q1MeasurementValue(getQ1MeasurementValue())
                .q1MeasurementValidity(getQ1MeasurementValidity())
                .q2MeasurementValue(getQ2MeasurementValue())
                .q2MeasurementValidity(getQ2MeasurementValidity())
                .g(getG())
                .b(getB())
                .ratedU1(getRatedU1())
                .ratedU2(getRatedU2())
                .ratedS(getRatedS())
                .ratioTapChanger(getRatioTapChanger())
                .phaseTapChanger(getPhaseTapChanger())
                .ratioTapChangerToBeEstimated(getRatioTapChangerToBeEstimated())
                .phaseTapChangerToBeEstimated(getPhaseTapChangerToBeEstimated())
                .build();
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.twoWindingsTransformerModification.modification")
                .withUntypedValue("twoWindingsTransformerId", getEquipmentId())
                .add();
    }
}
