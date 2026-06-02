/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
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
@JsonTypeName("LINE_MODIFICATION")
@ModificationErrorTypeName("MODIFY_LINE_ERROR")
public class LineModificationModel extends BranchModificationModel {

    private AttributeModification<Double> g1;

    private AttributeModification<Double> b1;

    private AttributeModification<Double> g2;

    private AttributeModification<Double> b2;

    private List<LineSegmentModel> lineSegments;

    private boolean applySegmentsLimits;

    @Override
    public AbstractModification toModification() {
        return new LineModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.line.modification")
                .withUntypedValue("lineId", getEquipmentId())
                .add();
    }
}
