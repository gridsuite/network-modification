/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
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
@Schema(description = "Create Voltage Level Section")
@JsonTypeName("CREATE_VOLTAGE_LEVEL_SECTION")
@ModificationErrorTypeName("CREATE_VOLTAGE_LEVEL_SECTION_ERROR")
public class CreateVoltageLevelSectionInfos extends ModificationInfos {
    @Schema(description = "VoltageLevelId")
    private String voltageLevelId;

    @Schema(description = "Busbar Count")
    private int busbarCount;

    @Schema(description = "Section Count")
    private int sectionCount;

    @Schema(description = "After The Reference BusbarSectionId")
    private boolean isAfterBusbarSectionId;

    @Schema(description = "Left Switch Kind")
    private String leftSwitchKind;

    @Schema(description = "Right Switch Kind")
    private String rightSwitchKind;

    @Schema(description = "All Busbars")
    private boolean isAllBusbars;

    @Schema(description = "Reference BusbarSectionId")
    private String busbarSectionId;

    @Schema(description = "Switch Open")
    private boolean isSwitchOpen;

    @Override
    public AbstractModification toModification() {
        return new CreateVoltageLevelSection(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevel.sections.creation")
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
