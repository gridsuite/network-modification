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
import org.gridsuite.modification.modifications.CreateCouplingDevice;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Coupling device creation")
@JsonTypeName("CREATE_COUPLING_DEVICE")
@ModificationErrorTypeName("CREATE_COUPLING_DEVICE_ERROR")
public class CreateCouplingDeviceInfos extends ModificationInfos {

    @Schema(description = "VoltageLevelId")
    private String voltageLevelId;

    @Schema(description = "BusOrBbsId1")
    private String busOrBbsId1;

    @Schema(description = "BusOrBbsId2")
    private String busOrBbsId2;

    @Override
    public AbstractModification toModification() {
        return new CreateCouplingDevice(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.createCouplingDevice").add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("voltageLevelId", getVoltageLevelId());
        return mapMessageValues;
    }
}
