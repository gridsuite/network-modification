/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.modifications.CreateVoltageLevelSection;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("CREATE_VOLTAGE_LEVEL_SECTION")
@ModificationErrorTypeName("CREATE_VOLTAGE_LEVEL_SECTION_ERROR")
public class CreateVoltageLevelSectionModel extends ModificationModel {
    private String voltageLevelId;

    private int busbarIndex;

    private boolean isAfterBusbarSectionId;

    private String leftSwitchKind;

    private String rightSwitchKind;

    private boolean isAllBusbars;

    private String busbarSectionId;

    private boolean isSwitchOpen;

    @Override
    public AbstractModification toModification() {
        return new CreateVoltageLevelSection(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevel.section.created")
                .withUntypedValue("voltageLevelId", getVoltageLevelId())
                .add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("voltageLevelId", getVoltageLevelId());
        return mapMessageValues;
    }
}
