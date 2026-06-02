/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.EquipmentAttributeModification;
import lombok.NonNull;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.gridsuite.modification.NetworkModificationException.Type.EQUIPMENT_ATTRIBUTE_NAME_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.EQUIPMENT_ATTRIBUTE_VALUE_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("EQUIPMENT_ATTRIBUTE_MODIFICATION")
@ModificationErrorTypeName("MODIFICATION_ERROR")
public class EquipmentAttributeModificationModel extends EquipmentModificationModel {
    private String equipmentAttributeName;

    private Object equipmentAttributeValue;

    @NonNull
    private IdentifiableType equipmentType;

    @Override
    public AbstractModification toModification() {
        return new EquipmentAttributeModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.equipmentAttributeModification")
                .withUntypedValue("EquipmentType", equipmentType.name())
                .withUntypedValue("EquipmentId", getEquipmentId())
                .add();
    }

    @Override
    public void check() {
        super.check();
        if (equipmentType == IdentifiableType.SWITCH) {
            checkSwitchStatusModificationInfos();
        }
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("equipmentAttributeName", getEquipmentAttributeName());
        mapMessageValues.put("equipmentId", getEquipmentId());
        mapMessageValues.put("equipmentAttributeValue", getEquipmentAttributeValue() != null
                    ? getEquipmentAttributeValue().toString()
                    : null);
        return mapMessageValues;
    }

    private void checkSwitchStatusModificationInfos() {
        if (!equipmentAttributeName.equals("open")) {
            throw new NetworkModificationException(EQUIPMENT_ATTRIBUTE_NAME_ERROR, "For switch status, the attribute name is only 'open'");
        }
        Set<Boolean> possibleValues = Set.of(true, false);
        if (!possibleValues.contains(equipmentAttributeValue)) {
            throw new NetworkModificationException(EQUIPMENT_ATTRIBUTE_VALUE_ERROR, "For switch status, the attribute values are only " + possibleValues);
        }
    }
}
