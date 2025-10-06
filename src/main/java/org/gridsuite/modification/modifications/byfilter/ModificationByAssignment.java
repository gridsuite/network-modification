/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.byfilter.AbstractAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.DataType;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.PropertyAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField;

import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFICATION_BY_ASSIGNMENT_ERROR;
import static org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField.getReferenceValue;
import static org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField.setNewValue;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class ModificationByAssignment extends AbstractModificationByAssignment {

    public static final String VALUE_KEY_ID = "id";
    public static final String VALUE_KEY_PROPERTY_NAME = "propertyName";
    public static final String VALUE_KEY_PROPERTY_VALUE = "propertyValue";
    public static final String VALUE_KEY_SIDE = "side";

    public static final String REPORT_KEY_OPERATIONAL_LIMITS_GROUP_PROPERTY_VALUE_NOT_FOUND_ERROR = "network.modification.operationalLimitsGroupPropertyValueNotFoundError";
    public static final String REPORT_KEY_OPERATIONAL_LIMITS_GROUP_PROPERTY_VALUE_MULTIPLE_ERROR = "network.modification.operationalLimitsGroupPropertyValueMultipleError";

    private final ModificationByAssignmentInfos modificationInfos;

    public ModificationByAssignment(ModificationByAssignmentInfos modificationInfos) {
        super();
        this.modificationInfos = modificationInfos;
    }

    @Override
    public String getModificationTypeLabel() {
        return "assignment";
    }

    @Override
    public ModificationInfos getModificationInfos() {
        return modificationInfos;
    }

    @Override
    public IdentifiableType getEquipmentType() {
        return modificationInfos.getEquipmentType();
    }

    @Override
    public List<AbstractAssignmentInfos> getAssignmentInfosList() {
        return Collections.unmodifiableList(modificationInfos.getAssignmentInfosList());
    }

    @Override
    public NetworkModificationException.Type getExceptionType() {
        return MODIFICATION_BY_ASSIGNMENT_ERROR;
    }

    @Override
    protected boolean isEquipmentEditable(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos, List<ReportNode> equipmentsReport) {
        AssignmentInfos<?> assignmentInfos = (AssignmentInfos<?>) abstractAssignmentInfos;
        if (assignmentInfos.getDataType() == DataType.PROPERTY) {
            PropertyField field = PropertyField.valueOf(abstractAssignmentInfos.getEditedField());
            String propertyName = ((PropertyAssignmentInfos) abstractAssignmentInfos).getPropertyName();
            String value = ((PropertyAssignmentInfos) abstractAssignmentInfos).getValue();
            return switch (equipment.getType()) {
                case LINE, TWO_WINDINGS_TRANSFORMER -> switch (field) {
                    case OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES -> isEditableOperationalLimitsGroupPropertyValue((Branch<?>) equipment, propertyName, value, TwoSides.ONE, equipmentsReport);
                    case OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES -> isEditableOperationalLimitsGroupPropertyValue((Branch<?>) equipment, propertyName, value, TwoSides.TWO, equipmentsReport);
                    default -> true;
                };
                default -> true;
            };
        } else {
            return super.isEquipmentEditable(equipment, abstractAssignmentInfos, equipmentsReport);
        }
    }

    private boolean isEditableOperationalLimitsGroupPropertyValue(Branch<?> branch, String propertyName, String propertyValue, TwoSides side, List<ReportNode> equipmentsReport) {
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

    @Override
    protected boolean preCheckValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos, List<ReportNode> reports, List<String> notEditableEquipments) {
        return true;
    }

    @Override
    protected String getOldValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        AssignmentInfos<?> assignmentInfos = (AssignmentInfos<?>) abstractAssignmentInfos;
        if (assignmentInfos.getDataType() == DataType.PROPERTY) {
            String propertyName = ((PropertyAssignmentInfos) assignmentInfos).getPropertyName();
            String editedField = assignmentInfos.getEditedField();
            return getReferenceValue(equipment, editedField, propertyName);
        } else {
            return super.getOldValue(equipment, abstractAssignmentInfos);
        }
    }

    @Override
    protected String getNewValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        AssignmentInfos<?> assignmentInfos = (AssignmentInfos<?>) abstractAssignmentInfos;
        if (assignmentInfos.getValue() == null) {
            return null;
        }
        if (assignmentInfos.getDataType() == DataType.PROPERTY) {
            String propertyName = ((PropertyAssignmentInfos) assignmentInfos).getPropertyName();
            String propertyValue = ((PropertyAssignmentInfos) assignmentInfos).getValue();
            String editedField = assignmentInfos.getEditedField();
            return PropertyField.getNewValue(equipment, editedField, propertyName, propertyValue);
        } else {
            return assignmentInfos.getValue().toString();
        }
    }

    @Override
    protected String applyValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        AssignmentInfos<?> assignmentInfos = (AssignmentInfos<?>) abstractAssignmentInfos;
        if (assignmentInfos.getDataType() == DataType.PROPERTY) {
            String newValue = getNewValue(equipment, abstractAssignmentInfos);
            String propertyName = ((PropertyAssignmentInfos) assignmentInfos).getPropertyName();
            String editedField = assignmentInfos.getEditedField();
            setNewValue(equipment, editedField, propertyName, newValue);
            return newValue;
        } else {
            return super.applyValue(equipment, abstractAssignmentInfos);
        }
    }

    @Override
    public String getName() {
        return "ModificationByAssignment";
    }
}
