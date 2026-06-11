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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.ModificationByAssignmentModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.byfilter.AbstractAssignmentModel;
import org.gridsuite.modification.model.byfilter.DataType;
import org.gridsuite.modification.model.byfilter.assignment.AssignmentModel;
import org.gridsuite.modification.model.byfilter.assignment.PropertyAssignmentModel;
import org.gridsuite.modification.model.byfilter.equipmentfield.PropertyField;

import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFICATION_BY_ASSIGNMENT_ERROR;
import static org.gridsuite.modification.model.byfilter.equipmentfield.PropertyField.getReferenceValue;
import static org.gridsuite.modification.model.byfilter.equipmentfield.PropertyField.setNewValue;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class ModificationByAssignment extends AbstractModificationByAssignment {

    private final ModificationByAssignmentModel modificationModel;

    public ModificationByAssignment(ModificationByAssignmentModel modificationModel) {
        super();
        this.modificationModel = modificationModel;
    }

    @Override
    public String getModificationTypeLabel() {
        return "assignment";
    }

    @Override
    public ModificationModel getModificationModel() {
        return modificationModel;
    }

    @Override
    public IdentifiableType getEquipmentType() {
        return modificationModel.getEquipmentType();
    }

    @Override
    public List<AbstractAssignmentModel> getAssignmentModelList() {
        return Collections.unmodifiableList(modificationModel.getAssignmentInfosList());
    }

    @Override
    public NetworkModificationException.Type getExceptionType() {
        return MODIFICATION_BY_ASSIGNMENT_ERROR;
    }

    @Override
    protected boolean isEquipmentEditable(Identifiable<?> equipment, AbstractAssignmentModel abstractAssignmentModel, List<ReportNode> equipmentsReport) {
        AssignmentModel<?> assignmentModel = (AssignmentModel<?>) abstractAssignmentModel;
        if (assignmentModel.getDataType() == DataType.PROPERTY) {
            String editedField = abstractAssignmentModel.getEditedField();
            String propertyName = ((PropertyAssignmentModel) abstractAssignmentModel).getPropertyName();
            String propertyValue = ((PropertyAssignmentModel) abstractAssignmentModel).getValue();
            return PropertyField.isEquipmentEditable(equipment, editedField, propertyName, propertyValue, equipmentsReport);
        } else {
            return super.isEquipmentEditable(equipment, abstractAssignmentModel, equipmentsReport);
        }
    }

    @Override
    protected boolean preCheckValue(Identifiable<?> equipment, AbstractAssignmentModel abstractAssignmentModel, List<ReportNode> reports, List<String> notEditableEquipments) {
        if (equipment.getType() == IdentifiableType.GENERATOR) {
            return checkGeneratorsPowerValues(equipment, abstractAssignmentModel, reports);
        }
        return true;
    }

    @Override
    protected String getOldValue(Identifiable<?> equipment, AbstractAssignmentModel abstractAssignmentModel) {
        AssignmentModel<?> assignmentModel = (AssignmentModel<?>) abstractAssignmentModel;
        if (assignmentModel.getDataType() == DataType.PROPERTY) {
            String propertyName = ((PropertyAssignmentModel) assignmentModel).getPropertyName();
            String editedField = assignmentModel.getEditedField();
            return getReferenceValue(equipment, editedField, propertyName);
        } else {
            return super.getOldValue(equipment, abstractAssignmentModel);
        }
    }

    @Override
    protected String getNewValue(Identifiable<?> equipment, AbstractAssignmentModel abstractAssignmentModel) {
        AssignmentModel<?> assignmentModel = (AssignmentModel<?>) abstractAssignmentModel;
        if (assignmentModel.getValue() == null) {
            return null;
        }
        if (assignmentModel.getDataType() == DataType.PROPERTY) {
            String propertyName = ((PropertyAssignmentModel) assignmentModel).getPropertyName();
            String propertyValue = ((PropertyAssignmentModel) assignmentModel).getValue();
            String editedField = assignmentModel.getEditedField();
            return PropertyField.getNewValue(equipment, editedField, propertyName, propertyValue);
        } else {
            return assignmentModel.getValue().toString();
        }
    }

    @Override
    protected String applyValue(Identifiable<?> equipment, AbstractAssignmentModel abstractAssignmentModel) {
        AssignmentModel<?> assignmentModel = (AssignmentModel<?>) abstractAssignmentModel;
        if (assignmentModel.getDataType() == DataType.PROPERTY) {
            String newValue = getNewValue(equipment, abstractAssignmentModel);
            String propertyName = ((PropertyAssignmentModel) assignmentModel).getPropertyName();
            String editedField = assignmentModel.getEditedField();
            setNewValue(equipment, editedField, propertyName, newValue);
            return newValue;
        } else {
            return super.applyValue(equipment, abstractAssignmentModel);
        }
    }

    @Override
    public String getName() {
        return "ModificationByAssignment";
    }
}
