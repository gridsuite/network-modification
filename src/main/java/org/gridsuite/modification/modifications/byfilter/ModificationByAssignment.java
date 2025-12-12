/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.byfilter.AbstractAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.DataType;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.PropertyAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField;

import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField.getReferenceValue;
import static org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField.setNewValue;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class ModificationByAssignment extends AbstractModificationByAssignment {

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
    protected boolean isEquipmentEditable(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos, List<ReportNode> equipmentsReport) {
        AssignmentInfos<?> assignmentInfos = (AssignmentInfos<?>) abstractAssignmentInfos;
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
