/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.byfilter;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import lombok.*;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.dto.byfilter.equipmentfield.FieldUtils;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.error.NetworkModificationExceptionType;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.data.assignment.AbstractAssignmentData;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.gridsuite.modification.dto.byfilter.equipmentfield.FieldUtils.getFieldValue;
import static org.gridsuite.modification.dto.byfilter.equipmentfield.FieldUtils.setFieldValue;
import static org.gridsuite.modification.dto.byfilter.equipmentfield.GeneratorField.*;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@EqualsAndHashCode(callSuper = true)
public abstract class AbstractModificationByAssignment extends AbstractModification {
    public static final String VALUE_KEY_FIELD_NAME = "fieldName";
    public static final String VALUE_KEY_FIELD_VALUE = "fieldValue";
    public static final String VALUE_KEY_MIN_VALUE = "minValue";
    public static final String VALUE_KEY_MAX_VALUE = "maxValue";
    public static final String VALUE_KEY_TARGET_VALUE = "targetValue";
    public static final String VALUE_KEY_EQUIPMENT_NAME = "equipmentName";
    public static final String VALUE_KEY_EQUIPMENT_TYPE = "equipmentType";
    public static final String VALUE_KEY_EQUIPMENT_COUNT = "equipmentCount";
    public static final String VALUE_KEY_NB_CHANGED = "nbChanged";
    public static final String VALUE_KEY_NB_UNCHANGED = "nbUnchanged";
    public static final String VALUE_KEY_OLD_VALUE = "oldValue";
    public static final String VALUE_KEY_NEW_VALUE = "newValue";
    public static final String VALUE_KEY_MODIFICATION_TYPE_LABEL = "modificationTypeLabel";
    public static final String VALUE_KEY_ERROR_MESSAGE = "errorMessage";
    public static final String VALUE_KEY_ARROW_NAME = "arrow";
    public static final String VALUE_KEY_ARROW_VALUE = "→";
    public static final String VALUE_KEY_FILTER_COUNT = "filterCount";
    public static final String REPORT_KEY_ASSIGNING_VALUES = "network.modification.assignValues";
    public static final String REPORT_KEY_FILTER_EVALUATION = "network.modification.filterEvaluation";
    public static final String REPORT_KEY_FILTER_EVALUATION_RESULT = "network.modification.filterEvaluationResult";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_ERROR_ZERO = "network.modification.equipmentModifiedError.zero";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_ERROR_MISSING = "network.modification.equipmentModifiedError.missing";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_SOME = "network.modification.byFilterModificationSome";
    public static final String REPORT_KEY_EDITED_FIELD_FILTER = "network.modification.editedFieldFilter";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_REPORT = "network.modification.equipmentModifiedReport";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_REPORT_EXCEPTION = "network.modification.equipmentModifiedReportException";
    public static final String REPORT_KEY_APPLIED_BY_FILTER_MODIFICATIONS = "network.modification.appliedByFilterModifications";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_ALL = "network.modification.byFilterModificationAll";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_NONE = "network.modification.byFilterModificationNone";

    @JsonIgnore
    @EqualsAndHashCode.Exclude
    protected long equipmentModifiedCount = 0;

    @JsonIgnore
    @EqualsAndHashCode.Exclude
    protected long equipmentCount = 0;

    @JsonIgnore
    public abstract String getModificationTypeLabel();

    @JsonIgnore
    public abstract NetworkModificationExceptionType getExceptionType();

    public abstract IdentifiableType getEquipmentType();

    public abstract List<AbstractAssignmentData> getAssignments();

    protected abstract boolean preCheckValue(Identifiable<?> equipment,
                                             AbstractAssignmentData abstractAssignmentData,
                                             ReportNode reportNode);

    protected abstract String getNewValue(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData);

    protected boolean checkGeneratorsPowerValues(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData, ReportNode reportNode) {
        if (equipment.getType() == IdentifiableType.GENERATOR) {
            Generator generator = (Generator) equipment;
            if (abstractAssignmentData.getEditedField().equals(PLANNED_ACTIVE_POWER_SET_POINT.name())) {
                return validateActivePowerValue(generator, FIELD_PLANNED_ACTIVE_POWER_SET_POINT, reportNode, Double.parseDouble(getNewValue(equipment, abstractAssignmentData)));
            } else if (abstractAssignmentData.getEditedField().equals(MINIMUM_ACTIVE_POWER.name())) {
                return validateMinimumActivePower(generator, reportNode, Double.parseDouble(getNewValue(equipment, abstractAssignmentData)));
            } else if (abstractAssignmentData.getEditedField().equals(MAXIMUM_ACTIVE_POWER.name())) {
                return validateMaximumActivePower(generator, reportNode, Double.parseDouble(getNewValue(equipment, abstractAssignmentData)));
            } else if (abstractAssignmentData.getEditedField().equals(ACTIVE_POWER_SET_POINT.name())) {
                double newValue = Double.parseDouble(getNewValue(equipment, abstractAssignmentData));
                if (newValue != 0) { // 0 is an exception to the rule
                    return validateActivePowerValue(generator, FIELD_ACTIVE_POWER_TARGET, reportNode, newValue);
                }
            }
        }
        return true;
    }

    protected String getOldValue(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData) {
        return getFieldValue(equipment, abstractAssignmentData.getEditedField());
    }

    protected String applyValue(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData) {
        // get new value
        String newValue = getNewValue(equipment, abstractAssignmentData);

        // set new value for the equipment
        setFieldValue(equipment, abstractAssignmentData.getEditedField(), newValue);
        return newValue;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (CollectionUtils.isEmpty(getAssignments())) {
            throw new NetworkModificationException(getExceptionType(), String.format("At least one %s is required", getModificationTypeLabel()));
        }

        if (getAssignments().stream().anyMatch(modificationByFilterInfos -> CollectionUtils.isEmpty(modificationByFilterInfos.getFilters()))) {
            throw new NetworkModificationException(getExceptionType(), String.format("Every %s must have at least one filter", getModificationTypeLabel()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ReportNode subReporter = subReportNode.newReportNode()
                .withMessageTemplate(REPORT_KEY_APPLIED_BY_FILTER_MODIFICATIONS)
                .withUntypedValue(VALUE_KEY_MODIFICATION_TYPE_LABEL, StringUtils.capitalize(getModificationTypeLabel()))
                .withUntypedValue(VALUE_KEY_EQUIPMENT_TYPE, getEquipmentType().name())
                .add();
        for (int i = 0; i < getAssignments().size(); i++) {
            ReportNode assignmentContainer = subReporter.newReportNode()
                    .withMessageTemplate(REPORT_KEY_EDITED_FIELD_FILTER)
                    .withUntypedValue(VALUE_KEY_FIELD_NAME, getAssignments().get(i).getEditedFieldLabel())
                    .add();
            List<Identifiable<?>> equipments = new ArrayList<>();
            for (int j = 0; j < getAssignments().get(i).getFilters().size(); j++) {
                ReportNode filterReport = assignmentContainer.newReportNode()
                        .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION)
                        .withUntypedValue(VALUE_KEY_FILTER_COUNT, j + 1)
                        .add();
                equipments.addAll(getAssignments().get(i).getFilters().get(j).evaluate(network, filterReport));
            }
            assignmentContainer.newReportNode()
                    .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION_RESULT)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_COUNT, equipments.size())
                    .add();
            ReportNode assigningValuesContainer = assignmentContainer.newReportNode()
                    .withMessageTemplate(REPORT_KEY_ASSIGNING_VALUES)
                    .add();
            equipmentCount += equipments.size();
            equipmentModifiedCount += applyOnAssignmentEquipments(equipments, assigningValuesContainer, getAssignments().get(i));
        }
        createCountReports(subReportNode, equipmentCount, equipmentModifiedCount);
    }

    private int applyOnAssignmentEquipments(List<Identifiable<?>> equipments,
                                            ReportNode assignmentReportNode,
                                            AbstractAssignmentData abstractAssignmentData) {
        final AtomicInteger modifiedInAssignmentCount = new AtomicInteger(0);
        equipments.stream()
                // Why not in the same pre-condition ??
                .filter(equipment -> isEquipmentEditable(equipment, abstractAssignmentData, assignmentReportNode) &&
                        preCheckValue(equipment, abstractAssignmentData, assignmentReportNode))
                .forEach(equipment -> {
                    boolean applied = applyModification(equipment, abstractAssignmentData, assignmentReportNode);
                    if (applied) {
                        modifiedInAssignmentCount.incrementAndGet();
                    }
                });
        createCountReports(assignmentReportNode, equipments.size(), modifiedInAssignmentCount.get());
        return modifiedInAssignmentCount.get();
    }

    protected boolean isEquipmentEditable(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData, ReportNode reportNode) {
        if (abstractAssignmentData.getEditedField() == null) {
            return false;
        }
        return FieldUtils.isEquipmentEditable(equipment, abstractAssignmentData.getEditedField(), reportNode);
    }

    private boolean applyModification(Identifiable<?> equipment,
                                      AbstractAssignmentData abstractAssignmentData,
                                      ReportNode reportNode) {
        try {
            final String oldValue = getOldValue(equipment, abstractAssignmentData);
            final String newValue = applyValue(equipment, abstractAssignmentData);
            reportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_EQUIPMENT_MODIFIED_REPORT)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_TYPE, equipment.getType().name())
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                    .withUntypedValue(VALUE_KEY_FIELD_NAME, abstractAssignmentData.getEditedFieldLabel())
                    .withUntypedValue(VALUE_KEY_OLD_VALUE, oldValue == null ? NO_VALUE : oldValue)
                    .withUntypedValue(VALUE_KEY_NEW_VALUE, newValue == null ? NO_VALUE : newValue)
                    .withUntypedValue(VALUE_KEY_ARROW_NAME, VALUE_KEY_ARROW_VALUE) // Workaround to use non-ISO-8859-1 characters in the internationalization file
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .add();
            return true;
        } catch (Exception e) {
            reportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_EQUIPMENT_MODIFIED_REPORT_EXCEPTION)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                    .withUntypedValue(VALUE_KEY_ERROR_MESSAGE, e.getMessage())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .add();
            return false;
        }
    }

    private void createCountReports(ReportNode subReportNode, long allEquipmentsCount, long modifiedEquipmentCount) {
        if (allEquipmentsCount == modifiedEquipmentCount && modifiedEquipmentCount != 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_ALL)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_COUNT, allEquipmentsCount)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        } else {
            if (modifiedEquipmentCount == 0) {
                subReportNode.newReportNode()
                        .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_NONE)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else {
                subReportNode.newReportNode()
                        .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_SOME)
                        .withUntypedValue(VALUE_KEY_NB_CHANGED, modifiedEquipmentCount)
                        .withUntypedValue(VALUE_KEY_NB_UNCHANGED, allEquipmentsCount - modifiedEquipmentCount)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            }
        }
    }
}
