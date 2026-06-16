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
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.LineModification;
import java.util.List;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Line modification")
@JsonTypeName("LINE_MODIFICATION")
@ModificationErrorTypeName("MODIFY_LINE_ERROR")
public class LineModificationInfos extends BranchModificationInfos {

    @Schema(description = "Shunt conductance Side 1")
    private AttributeModification<Double> g1;

    @Schema(description = "Shunt susceptance Side 1")
    private AttributeModification<Double> b1;

    @Schema(description = "Shunt conductance Side 2")
    private AttributeModification<Double> g2;

    @Schema(description = "Shunt susceptance Side 2")
    private AttributeModification<Double> b2;

    @Schema(description = "segments used from catalog to generate limits")
    private List<LineSegmentInfos> lineSegments;

    @Schema(description = "apply limits from catalog segments")
    private boolean applySegmentsLimits;

    @Override
    public AbstractModification toModification() {
        return LineModification.builder()
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
            .g1(g1)
            .b1(b1)
            .g2(g2)
            .b2(b2)
            .lineSegments(lineSegments)
            .applySegmentsLimits(applySegmentsLimits)
            .build();
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.line.modification")
                .withUntypedValue("lineId", getEquipmentId())
                .add();
    }
}
