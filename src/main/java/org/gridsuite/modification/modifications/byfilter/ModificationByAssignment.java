/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.byfilter;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.*;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.byfilter.AbstractAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.AbstractValueAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DataType;
import org.gridsuite.modification.dto.byfilter.assignment.PropertyAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField;
import org.gridsuite.modification.error.NetworkModificationExceptionType;

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
    private List<? extends AbstractValueAssignmentInfos<?>> assignmentInfosList;

    @Builder
    public ModificationByAssignment(IdentifiableType equipmentType, List<? extends AbstractValueAssignmentInfos<?>> assignmentInfosList) {
        super();
        this.equipmentType = equipmentType;
        this.assignmentInfosList = assignmentInfosList;
    }

    @Override
    @JsonIgnore
    public String getModificationTypeLabel() {
        return "assignment";
    }

    @Override
    public IdentifiableType getEquipmentType() {
        return equipmentType;
    }

    @Override
    public List<? extends AbstractValueAssignmentInfos<?>> getAssignmentInfosList() {
        return Collections.unmodifiableList(assignmentInfosList);
    }

    @JsonIgnore
    public NetworkModificationExceptionType getExceptionType() {
        return MODIFICATION_BY_ASSIGNMENT_ERROR;
    }

    @Override
    protected boolean isEquipmentEditable(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos, List<ReportNode> equipmentsReport) {
        AbstractValueAssignmentInfos<?> assignmentInfos = (AbstractValueAssignmentInfos<?>) abstractAssignmentInfos;
        if (assignmentInfos.getDataType() == DataType.PROPERTY) {
            String editedField = abstractAssignmentInfos.getEditedField();
            String propertyName = ((PropertyAssignmentInfos) abstractAssignmentInfos).getPropertyName();
            String propertyValue = ((PropertyAssignmentInfos) abstractAssignmentInfos).getValue();
            return PropertyField.isEquipmentEditable(equipment, editedField, propertyName, propertyValue, equipmentsReport);
        } else {
            return super.isEquipmentEditable(equipment, abstractAssignmentInfos, equipmentsReport);
        }
    }

    @Override
    protected boolean preCheckValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos, List<ReportNode> reports, List<String> notEditableEquipments) {
        if (equipment.getType() == IdentifiableType.GENERATOR) {
            return checkGeneratorsPowerValues(equipment, abstractAssignmentInfos, reports);
        }
        return true;
    }

    @Override
    protected String getOldValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        AbstractValueAssignmentInfos<?> assignmentInfos = (AbstractValueAssignmentInfos<?>) abstractAssignmentInfos;
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
        AbstractValueAssignmentInfos<?> assignmentInfos = (AbstractValueAssignmentInfos<?>) abstractAssignmentInfos;
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
        AbstractValueAssignmentInfos<?> assignmentInfos = (AbstractValueAssignmentInfos<?>) abstractAssignmentInfos;
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
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public String getName() {
        return ModificationType.MODIFICATION_BY_ASSIGNMENT.name();
    }
}
