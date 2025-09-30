/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.*;
import org.gridsuite.modification.NetworkModificationException;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public final class FieldUtils {

    private FieldUtils() {

    }

    @Nullable
    public static String getFieldValue(Identifiable<?> equipment, String equipmentField) {
        return switch (equipment.getType()) {
            case GENERATOR -> GeneratorField.getReferenceValue((Generator) equipment, equipmentField);
            case BATTERY -> BatteryField.getReferenceValue((Battery) equipment, equipmentField);
            case SHUNT_COMPENSATOR ->
                    ShuntCompensatorField.getReferenceValue((ShuntCompensator) equipment, equipmentField);
            case VOLTAGE_LEVEL -> VoltageLevelField.getReferenceValue((VoltageLevel) equipment, equipmentField);
            case LOAD -> LoadField.getReferenceValue((Load) equipment, equipmentField);
            case TWO_WINDINGS_TRANSFORMER ->
                    TwoWindingsTransformerField.getReferenceValue((TwoWindingsTransformer) equipment, equipmentField);
            case LINE -> LineField.getReferenceValue((Line) equipment, equipmentField);
            default -> throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                            "Unsupported getting value for equipment type : " + equipment.getType().name());
        };
    }

    public static void setFieldValue(Identifiable<?> equipment, String equipmentField, String newValue) {
        switch (equipment.getType()) {
            case GENERATOR -> GeneratorField.setNewValue((Generator) equipment, equipmentField, newValue);
            case BATTERY -> BatteryField.setNewValue((Battery) equipment, equipmentField, newValue);
            case SHUNT_COMPENSATOR -> ShuntCompensatorField.setNewValue((ShuntCompensator) equipment, equipmentField, newValue);
            case VOLTAGE_LEVEL -> VoltageLevelField.setNewValue((VoltageLevel) equipment, equipmentField, newValue);
            case LOAD -> LoadField.setNewValue((Load) equipment, equipmentField, newValue);
            case TWO_WINDINGS_TRANSFORMER -> TwoWindingsTransformerField.setNewValue((TwoWindingsTransformer) equipment, equipmentField, newValue);
            case LINE -> LineField.setNewValue((Line) equipment, equipmentField, newValue);
            default -> throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                            "Unsupported setting value for equipment type : " + equipment.getType().name());
        }
    }

    public static void setOperationalLimitsGroupPropertyValue(Branch<?> branch, String propertyField, String newValue, TwoSides side) {
        List<OperationalLimitsGroup> operationalLimitsGroupList = new ArrayList<>();
        switch (side) {
            case ONE -> operationalLimitsGroupList = branch.getOperationalLimitsGroups1().stream()
                    .filter(operationalLimitsGroup -> operationalLimitsGroup.getProperty(propertyField) != null &&
                            operationalLimitsGroup.getProperty(propertyField).equals(newValue))
                    .toList();
            case TWO -> operationalLimitsGroupList = branch.getOperationalLimitsGroups2().stream()
                    .filter(operationalLimitsGroup -> operationalLimitsGroup.getProperty(propertyField) != null &&
                            operationalLimitsGroup.getProperty(propertyField).equals(newValue))
                    .toList();
        }
        if (operationalLimitsGroupList.isEmpty()) {
            throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                    String.format("Error when modifying operational limits group, branch %s has no property %s", branch.getId(), propertyField));
        } else if (operationalLimitsGroupList.size() > 1) {
            throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                    String.format("Error when modifying operational limits group, branch %s has several limits group with property %s", branch.getId(), propertyField));
        }
        switch (side) {
            case ONE -> branch.setSelectedOperationalLimitsGroup1(operationalLimitsGroupList.getFirst().getId());
            case TWO -> branch.setSelectedOperationalLimitsGroup2(operationalLimitsGroupList.getFirst().getId());
        }
    }
}
