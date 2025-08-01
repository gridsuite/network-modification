/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.SwitchKind;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.CreateVoltageLevelTopology;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "voltage level topology creation")
@JsonTypeName("CREATE_VOLTAGE_LEVEL_TOPOLOGY")
public class CreateVoltageLevelTopologyInfos extends ModificationInfos {

    @Schema(description = "voltageLevelId")
    private String voltageLevelId;

    @Schema(description = "switchKinds")
    private List<SwitchKind> switchKinds;

    @Schema(description = "sectionCount")
    private Integer sectionCount;

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.createVoltageLevelTopology")
            .withUntypedValue("voltageLevelId", getVoltageLevelId())
            .add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("voltageLevelId", getVoltageLevelId());
        return mapMessageValues;
    }

    @Override
    public AbstractModification toModification() {
        return new CreateVoltageLevelTopology(this);
    }
}
