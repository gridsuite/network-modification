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
import org.gridsuite.modification.modifications.LineSplitWithVoltageLevel;

import java.util.Map;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("LINE_SPLIT_WITH_VOLTAGE_LEVEL")
@ModificationErrorTypeName("LINE_SPLIT_ERROR")
public class LineSplitWithVoltageLevelModel extends ModificationModel {

    private String lineToSplitId;

    private double percent;

    private VoltageLevelCreationModel mayNewVoltageLevelInfos;

    private String existingVoltageLevelId;

    private String bbsOrBusId;

    private String newLine1Id;

    private String newLine1Name;

    private String newLine2Id;

    private String newLine2Name;

    @Override
    public AbstractModification toModification() {
        return new LineSplitWithVoltageLevel(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.lineSplitWithVoltageLevel")
                .add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        return Map.of("lineToSplitId", getLineToSplitId());
    }
}
