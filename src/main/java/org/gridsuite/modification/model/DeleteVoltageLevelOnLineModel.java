/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.DeleteVoltageLevelOnLine;

import java.util.HashMap;
import java.util.Map;


/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("DELETE_VOLTAGE_LEVEL_ON_LINE")
@ModificationErrorTypeName("DELETE_VOLTAGE_LEVEL_ON_LINE_ERROR")
public class DeleteVoltageLevelOnLineModel extends ModificationModel {

    private String lineToAttachTo1Id;

    private String lineToAttachTo2Id;

    private String replacingLine1Id;

    private String replacingLine1Name;

    @Override
    public AbstractModification toModification() {
        return new DeleteVoltageLevelOnLine(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.deleteVoltageLevelOnLine").add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("lineToAttachTo1Id", getLineToAttachTo1Id());
        mapMessageValues.put("lineToAttachTo2Id", getLineToAttachTo2Id());
        return mapMessageValues;
    }

}
