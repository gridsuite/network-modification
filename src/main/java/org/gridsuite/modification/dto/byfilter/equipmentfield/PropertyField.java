/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.OperationalLimitsGroup;
import com.powsybl.iidm.network.TwoSides;
import org.gridsuite.modification.NetworkModificationException;

import java.util.List;

import static org.gridsuite.modification.dto.byfilter.equipmentfield.FieldUtils.setOperationalLimitsGroup;

/**
 * @author Etienne LESOT <etienne.lesot at rte-france.com>
 */
public enum PropertyField {
    FREE_PROPERTIES,
    OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES,
    OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES;

    public static String getReferenceValue(Identifiable<?> equipment, String editedField, String propertyName) {
        PropertyField field = PropertyField.valueOf(editedField);
        if (field == FREE_PROPERTIES) {
            return equipment.getProperty(propertyName);
        } else {
            return switch (equipment.getType()) {
                case LINE, TWO_WINDINGS_TRANSFORMER -> getReferenceValue((Branch<?>) equipment, field);
                default -> throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                        "Unsupported getting value for equipment type : " + equipment.getType().name());
            };
        }
    }

    public static String getReferenceValue(Branch<?> branch, PropertyField field) {
        return switch (field) {
            case OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES -> branch.getSelectedOperationalLimitsGroupId1().orElse(null);
            case OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES -> branch.getSelectedOperationalLimitsGroupId2().orElse(null);
            default -> throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                    "Unsupported getting value for equipment type : " + " [" + field + "," + branch.getType() + "]");
        };
    }

    public static String getNewValue(Identifiable<?> equipment, String editedField, String propertyName, String propertyValue) {
        PropertyField field = PropertyField.valueOf(editedField);
        if (field == FREE_PROPERTIES) {
            return propertyValue;
        } else {
            return switch (equipment.getType()) {
                case LINE, TWO_WINDINGS_TRANSFORMER -> getNewValue((Branch<?>) equipment, field, propertyName, propertyValue);
                default -> throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                        "Unsupported getting value for equipment type : " + equipment.getType().name());
            };
        }
    }

    public static String getNewValue(Branch<?> equipment, PropertyField field, String propertyName, String propertyValue) {
        return switch (field) {
            case OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES -> getNewValue(equipment, TwoSides.ONE, propertyName, propertyValue);
            case OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES -> getNewValue(equipment, TwoSides.TWO, propertyName, propertyValue);
            default -> throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                    "Unsupported getting value for equipment type : " + " [" + field + "," + equipment.getType() + "]");
        };
    }

    public static String getNewValue(Branch<?> branch, TwoSides side, String propertyName, String propertyValue) {
        List<OperationalLimitsGroup> operationalLimitsGroupList = (
            switch (side) {
                case ONE -> branch.getOperationalLimitsGroups1();
                case TWO -> branch.getOperationalLimitsGroups2();
            }
        ).stream()
                .filter(operationalLimitsGroup -> operationalLimitsGroup.getProperty(propertyName) != null &&
                                                  operationalLimitsGroup.getProperty(propertyName).equals(propertyValue))
                .toList();
        return operationalLimitsGroupList.getFirst().getId();
    }

    public static void setNewValue(Identifiable<?> equipment, String editedField, String propertyName, String newValue) {
        PropertyField field = PropertyField.valueOf(editedField);
        if (field == PropertyField.FREE_PROPERTIES) {
            equipment.setProperty(propertyName, newValue);
        } else {
            switch (equipment.getType()) {
                case LINE, TWO_WINDINGS_TRANSFORMER -> setNewValue((Branch<?>) equipment, field, newValue);
            }
        }
    }

    private static void setNewValue(Branch<?> branch, PropertyField field, String newValue) {
        switch (field) {
            case OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES -> setOperationalLimitsGroup(branch, newValue, TwoSides.ONE);
            case OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES -> setOperationalLimitsGroup(branch, newValue, TwoSides.TWO);
        }
    }
}
