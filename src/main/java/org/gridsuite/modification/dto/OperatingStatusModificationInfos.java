/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.OperatingStatusModification;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Operating status modification")
@JsonTypeName("OPERATING_STATUS_MODIFICATION")
public class OperatingStatusModificationInfos extends EquipmentModificationInfos {
    @Schema(description = "Action type")
    private ActionType action;

    @Schema(description = "Energized end one or two voltage level ID")
    private String energizedVoltageLevelId;

    public enum ActionType {
        LOCKOUT,
        TRIP,
        SWITCH_ON,
        ENERGISE_END_ONE,
        ENERGISE_END_TWO
    }

    @Override
    public AbstractModification toModification() {
        return new OperatingStatusModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        String messageKey = switch (action) {
            case LOCKOUT -> "network.modification.OPERATING_STATUS_MODIFICATION_LOCKOUT";
            case TRIP -> "network.modification.OPERATING_STATUS_MODIFICATION_TRIP";
            case ENERGISE_END_ONE -> "network.modification.OPERATING_STATUS_MODIFICATION_ENERGISE_END_ONE";
            case ENERGISE_END_TWO -> "network.modification.OPERATING_STATUS_MODIFICATION_ENERGISE_END_TWO";
            case SWITCH_ON -> "network.modification.OPERATING_STATUS_MODIFICATION_SWITCH_ON";
        };
        return reportNode.newReportNode()
                .withMessageTemplate(messageKey)
                .withUntypedValue("equipmentId", this.getEquipmentId())
                .add();
    }

    @Override
    public void check() {
        super.check();
        if (action == null) {
            throw new NetworkModificationRunException("Empty operating action type");
        }
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("action", getAction().name());
        mapMessageValues.put("equipmentId", getEquipmentId());
        if (getEnergizedVoltageLevelId() != null) {
            mapMessageValues.put("energizedVoltageLevelId", getEnergizedVoltageLevelId());
        }
        return mapMessageValues;

    }
}
