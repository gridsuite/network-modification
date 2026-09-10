/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.byfilter;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.*;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField;
import org.gridsuite.modification.error.NetworkModificationExceptionType;
import org.gridsuite.modification.modifications.data.assignment.AbstractAssignmentData;
import org.gridsuite.modification.modifications.data.assignment.DataType;
import org.gridsuite.modification.modifications.data.assignment.PropertyAssignmentData;
import org.gridsuite.modification.modifications.data.assignment.ValueAssignmentData;

import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField.getReferenceValue;
import static org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField.setNewValue;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.MODIFICATION_BY_ASSIGNMENT_ERROR;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ModificationByAssignment extends AbstractModificationByAssignment {

    private IdentifiableType equipmentType;
    private List<? extends ValueAssignmentData<?>> valueAssignments;

    @Builder
    public ModificationByAssignment(IdentifiableType equipmentType, List<? extends ValueAssignmentData<?>> valueAssignments) {
        this.equipmentType = equipmentType;
        this.valueAssignments = valueAssignments;
    }

    @Override
    public String getModificationTypeLabel() {
        return "assignment";
    }

    @Override
    public NetworkModificationExceptionType getExceptionType() {
        return MODIFICATION_BY_ASSIGNMENT_ERROR;
    }

    @Override
    @JsonIgnore
    public List<AbstractAssignmentData> getAssignments() {
        return Collections.unmodifiableList(valueAssignments);
    }

    @Override
    protected boolean isEquipmentEditable(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData, ReportNode reportNode) {
        ValueAssignmentData<?> valueAssignment = (ValueAssignmentData<?>) abstractAssignmentData;
        if (valueAssignment.getDataType() == DataType.PROPERTY) {
            String editedField = abstractAssignmentData.getEditedField();
            String propertyName = ((PropertyAssignmentData) abstractAssignmentData).getPropertyName();
            String propertyValue = ((PropertyAssignmentData) abstractAssignmentData).getValue();
            return PropertyField.isEquipmentEditable(equipment, editedField, propertyName, propertyValue, reportNode);
        } else {
            return super.isEquipmentEditable(equipment, abstractAssignmentData, reportNode);
        }
    }

    @Override
    protected boolean preCheckValue(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData, ReportNode reportNode) {
        if (equipment.getType() == IdentifiableType.GENERATOR) {
            return checkGeneratorsPowerValues(equipment, abstractAssignmentData, reportNode);
        }
        return true;
    }

    @Override
    protected String getOldValue(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData) {
        ValueAssignmentData<?> assignmentData = (ValueAssignmentData<?>) abstractAssignmentData;
        if (assignmentData.getDataType() == DataType.PROPERTY) {
            String propertyName = ((PropertyAssignmentData) assignmentData).getPropertyName();
            String editedField = assignmentData.getEditedField();
            return getReferenceValue(equipment, editedField, propertyName);
        } else {
            return super.getOldValue(equipment, abstractAssignmentData);
        }
    }

    @Override
    protected String getNewValue(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData) {
        ValueAssignmentData<?> assignmentData = (ValueAssignmentData<?>) abstractAssignmentData;
        if (assignmentData.getValue() == null) {
            return null;
        }
        if (assignmentData.getDataType() == DataType.PROPERTY) {
            String propertyName = ((PropertyAssignmentData) assignmentData).getPropertyName();
            String propertyValue = ((PropertyAssignmentData) assignmentData).getValue();
            String editedField = assignmentData.getEditedField();
            return PropertyField.getNewValue(equipment, editedField, propertyName, propertyValue);
        } else {
            return assignmentData.getValue().toString();
        }
    }

    @Override
    protected String applyValue(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData) {
        ValueAssignmentData<?> assignmentData = (ValueAssignmentData<?>) abstractAssignmentData;
        if (assignmentData.getDataType() == DataType.PROPERTY) {
            String newValue = getNewValue(equipment, abstractAssignmentData);
            String propertyName = ((PropertyAssignmentData) assignmentData).getPropertyName();
            String editedField = assignmentData.getEditedField();
            setNewValue(equipment, editedField, propertyName, newValue);
            return newValue;
        } else {
            return super.applyValue(equipment, abstractAssignmentData);
        }
    }

    @Override
    public String getName() {
        return ModificationType.MODIFICATION_BY_ASSIGNMENT.name();
    }
}
