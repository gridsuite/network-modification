/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.NetworkModificationException;

import javax.annotation.Nullable;
import java.util.List;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public final class FieldUtils {
    public static final String VALUE_KEY_ID = "id";
    public static final String VALUE_KEY_PROPERTY_NAME = "propertyName";
    public static final String VALUE_KEY_PROPERTY_VALUE = "propertyValue";
    public static final String VALUE_KEY_SIDE = "side";

    public static final String REPORT_KEY_OPERATIONAL_LIMITS_GROUP_PROPERTY_VALUE_NOT_FOUND_ERROR = "network.modification.operationalLimitsGroupPropertyValueNotFoundError";
    public static final String REPORT_KEY_OPERATIONAL_LIMITS_GROUP_PROPERTY_VALUE_MULTIPLE_ERROR = "network.modification.operationalLimitsGroupPropertyValueMultipleError";

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

    public static boolean isEditableOperationalLimitsGroupPropertyValue(Branch<?> branch, String propertyName, String propertyValue, TwoSides side, List<ReportNode> equipmentsReport) {
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
                .withAllResourceBundlesFromClasspath()
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
                .withAllResourceBundlesFromClasspath()
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

    public static void setOperationalLimitsGroup(Branch<?> branch, String newValue, TwoSides side) {
        switch (side) {
            case ONE -> branch.setSelectedOperationalLimitsGroup1(newValue);
            case TWO -> branch.setSelectedOperationalLimitsGroup2(newValue);
        }
    }
}
