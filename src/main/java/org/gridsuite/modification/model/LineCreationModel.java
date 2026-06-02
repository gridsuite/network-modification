/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.modifications.LineCreation;

import java.util.List;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("LINE_CREATION")
@ModificationErrorTypeName("CREATE_LINE_ERROR")
public class LineCreationModel extends BranchCreationModel {

    private Double g1;

    private Double b1;

    private Double g2;

    private Double b2;

    private List<LineSegmentModel> lineSegments;

    @Override
    public AbstractModification toModification() {
        return new LineCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.lineCreation")
                .withUntypedValue("lineId", getEquipmentId())
                .add();
    }
}
