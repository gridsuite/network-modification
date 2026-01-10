/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.OperationalLimitsGroup;
import com.powsybl.iidm.network.TwoSides;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.jspecify.annotations.NonNull;

import java.util.List;

/**
 * @author Etienne LESOT <etienne.lesot at rte-france.com>
 */
public enum PropertyField {
    FREE_PROPERTIES,
    OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES,
    OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES;

    public static final String VALUE_KEY_ID = "id";
    public static final String VALUE_KEY_PROPERTY_NAME = "propertyName";
    public static final String VALUE_KEY_PROPERTY_VALUE = "propertyValue";
    public static final String VALUE_KEY_SIDE = "side";

    public static final String REPORT_KEY_OPERATIONAL_LIMITS_GROUP_PROPERTY_VALUE_NOT_FOUND_ERROR = "network.modification.operationalLimitsGroupPropertyValueNotFoundError";
    public static final String REPORT_KEY_OPERATIONAL_LIMITS_GROUP_PROPERTY_VALUE_MULTIPLE_ERROR = "network.modification.operationalLimitsGroupPropertyValueMultipleError";

    public static boolean isEquipmentEditable(Identifiable<?> equipment, String editedField, String propertyName, String propertyValue, List<ReportNode> equipmentsReport) {
        PropertyField field = PropertyField.valueOf(editedField);
        if (field == FREE_PROPERTIES) {
            return true;
        } else {
            return switch (equipment.getType()) {
                case LINE, TWO_WINDINGS_TRANSFORMER -> switch (field) {
                    case OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES ->
                            isEditableOperationalLimitsGroupPropertyValue((Branch<?>) equipment, propertyName, propertyValue, TwoSides.ONE, equipmentsReport);
                    case OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES ->
                            isEditableOperationalLimitsGroupPropertyValue((Branch<?>) equipment, propertyName, propertyValue, TwoSides.TWO, equipmentsReport);
                    default -> true;
                };
                default -> true;
            };
        }
    }

    static boolean isEditableOperationalLimitsGroupPropertyValue(Branch<?> branch, String propertyName, String propertyValue, TwoSides side, List<ReportNode> equipmentsReport) {
        List<OperationalLimitsGroup> operationalLimitsGroupList = (
            switch (side) {
                case ONE -> branch.getOperationalLimitsGroups1();
                case TWO -> branch.getOperationalLimitsGroups2();
            }
        ).stream()
                .filter(operationalLimitsGroup -> operationalLimitsGroup.getProperty(propertyName) != null &&
                                                  operationalLimitsGroup.getProperty(propertyName).equals(propertyValue))
                .toList();

        if (operationalLimitsGroupList.isEmpty()) {
            equipmentsReport.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_OPERATIONAL_LIMITS_GROUP_PROPERTY_VALUE_NOT_FOUND_ERROR)
                    .withUntypedValue(VALUE_KEY_ID, branch.getId())
                    .withUntypedValue(VALUE_KEY_SIDE, side.ordinal() + 1)
                    .withUntypedValue(VALUE_KEY_PROPERTY_NAME, propertyName)
                    .withUntypedValue(VALUE_KEY_PROPERTY_VALUE, propertyValue)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            return false;
        } else if (operationalLimitsGroupList.size() > 1) {
            equipmentsReport.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_OPERATIONAL_LIMITS_GROUP_PROPERTY_VALUE_MULTIPLE_ERROR)
                    .withUntypedValue(VALUE_KEY_ID, branch.getId())
                    .withUntypedValue(VALUE_KEY_SIDE, side.ordinal() + 1)
                    .withUntypedValue(VALUE_KEY_PROPERTY_NAME, propertyName)
                    .withUntypedValue(VALUE_KEY_PROPERTY_VALUE, propertyValue)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            return false;
        }

        return true;
    }

    public static String getReferenceValue(Identifiable<?> equipment, String editedField, String propertyName) {
        PropertyField field = PropertyField.valueOf(editedField);
        if (field == FREE_PROPERTIES) {
            return equipment.getProperty(propertyName);
        } else {
            return switch (equipment.getType()) {
                case LINE, TWO_WINDINGS_TRANSFORMER -> getReferenceValue((Branch<?>) equipment, field);
                default -> throw unsupportedGettingField(equipment, field);
            };
        }
    }

    public static String getReferenceValue(Branch<?> branch, PropertyField field) {
        return switch (field) {
            case OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES -> branch.getSelectedOperationalLimitsGroupId1().orElse(null);
            case OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES -> branch.getSelectedOperationalLimitsGroupId2().orElse(null);
            default -> throw unsupportedGettingField(branch, field);
        };
    }

    public static String getNewValue(Identifiable<?> equipment, String editedField, String propertyName, String propertyValue) {
        PropertyField field = PropertyField.valueOf(editedField);
        if (field == FREE_PROPERTIES) {
            return propertyValue;
        } else {
            return switch (equipment.getType()) {
                case LINE, TWO_WINDINGS_TRANSFORMER -> getNewValue((Branch<?>) equipment, field, propertyName, propertyValue);
                default -> throw unsupportedGettingField(equipment, field);
            };
        }
    }

    public static String getNewValue(Branch<?> equipment, PropertyField field, String propertyName, String propertyValue) {
        return switch (field) {
            case OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES -> getNewValue(equipment, TwoSides.ONE, propertyName, propertyValue);
            case OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES -> getNewValue(equipment, TwoSides.TWO, propertyName, propertyValue);
            default -> throw unsupportedGettingField(equipment, field);
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
                default -> throw unsupportedGettingField(equipment, field);
            }
        }
    }

    private static void setNewValue(Branch<?> branch, PropertyField field, String newValue) {
        switch (field) {
            case OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES -> branch.setSelectedOperationalLimitsGroup1(newValue);
            case OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES -> branch.setSelectedOperationalLimitsGroup2(newValue);
            default -> throw unsupportedGettingField(branch, field);
        }
    }

    private static @NonNull NetworkModificationException unsupportedGettingField(Identifiable<?> equipment, PropertyField field) {
        return new NetworkModificationException("Unsupported getting value for equipment type : " + " [" + field + "," + equipment.getType() + "]");
    }
}
