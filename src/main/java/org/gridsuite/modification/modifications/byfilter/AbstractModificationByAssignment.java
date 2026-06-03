/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.FilterEquipments;
import org.gridsuite.modification.model.FilterModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.byfilter.AbstractAssignmentModel;
import org.gridsuite.modification.model.byfilter.equipmentfield.FieldUtils;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.model.byfilter.equipmentfield.FieldUtils.getFieldValue;
import static org.gridsuite.modification.model.byfilter.equipmentfield.FieldUtils.setFieldValue;
import static org.gridsuite.modification.model.byfilter.equipmentfield.GeneratorField.*;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public abstract class AbstractModificationByAssignment extends AbstractModification {
    public static final String VALUE_KEY_FILTER_NAME = "filterName";
    public static final String VALUE_KEY_FIELD_NAME = "fieldName";
    public static final String VALUE_KEY_NAME = "name";
    public static final String VALUE_KEY_FIELD_VALUE = "fieldValue";
    public static final String VALUE_KEY_MIN_VALUE = "minValue";
    public static final String VALUE_KEY_MAX_VALUE = "maxValue";
    public static final String VALUE_KEY_TARGET_VALUE = "targetValue";
    public static final String VALUE_KEY_EQUIPMENT_NAME = "equipmentName";
    public static final String VALUE_KEY_EQUIPMENT_TYPE = "equipmentType";
    public static final String VALUE_KEY_EQUIPMENT_COUNT = "equipmentCount";
    public static final String VALUE_KEY_EQUIPMENT_IDS = "equipmentIds";
    public static final String VALUE_KEY_NB_CHANGED = "nbChanged";
    public static final String VALUE_KEY_NB_UNCHANGED = "nbUnchanged";
    public static final String VALUE_KEY_OLD_VALUE = "oldValue";
    public static final String VALUE_KEY_NEW_VALUE = "newValue";
    public static final String VALUE_KEY_MODIFICATION_TYPE_LABEL = "modificationTypeLabel";
    public static final String VALUE_KEY_FILTERS_EACH_ASSIGNMENT = "filtersEachAssignment";
    public static final String VALUE_KEY_ERROR_MESSAGE = "errorMessage";
    public static final String VALUE_KEY_ARROW_NAME = "arrow";
    public static final String VALUE_KEY_ARROW_VALUE = "→";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_ERROR_ZERO = "network.modification.equipmentModifiedError.zero";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_ERROR_MISSING = "network.modification.equipmentModifiedError.missing";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_SOME = "network.modification.byFilterModificationSome";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_FAILED = "network.modification.byFilterModificationFailed";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_SUCCESS = "network.modification.byFilterModificationSuccess";
    public static final String REPORT_KEY_NUMBER_OF_VALID_EQUIPMENT = "network.modification.numberOfValidEquipment";
    public static final String REPORT_KEY_NOT_EDITED_EQUIPMENTS_FILTER = "network.modification.notEditedEquipmentsFilter";
    public static final String REPORT_KEY_EDITED_FIELD_FILTER = "network.modification.editedFieldFilter";
    public static final String REPORT_KEY_FILTER_EQUIPMENTS_NOT_FOUND = "network.modification.filterEquipmentsNotFound";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_REPORT = "network.modification.equipmentModifiedReport";
    public static final String REPORT_KEY_EQUIPMENT_MODIFIED_REPORT_EXCEPTION = "network.modification.equipmentModifiedReportException";
    public static final String REPORT_KEY_APPLIED_BY_FILTER_MODIFICATIONS = "network.modification.appliedByFilterModifications";
    public static final String REPORT_KEY_APPLIED_ASSIGNMENT = "network.modification.appliedAssignment";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_ALL = "network.modification.byFilterModificationAll";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_NONE = "network.modification.byFilterModificationNone";
    public static final String REPORT_KEY_BY_FILTER_MODIFICATION_NOT_FOUND = "network.modification.byFilterModificationNotFound";

    protected IFilterService filterService;
    protected int equipmentNotModifiedCount;
    protected long equipmentCount;
    protected long equipmentNotFoundCount;

    protected AbstractModificationByAssignment() {
        equipmentNotModifiedCount = 0;
        equipmentCount = 0;
        equipmentNotFoundCount = 0;
    }

    public abstract String getModificationTypeLabel();

    private String getEditedFieldLabel(AbstractAssignmentModel modificationByFilterModel) {
        return modificationByFilterModel.getEditedFieldLabel();
    }

    public abstract ModificationModel getModificationModel();

    public abstract IdentifiableType getEquipmentType();

    public abstract NetworkModificationException.Type getExceptionType();

    public abstract List<AbstractAssignmentModel> getAssignmentModelList();

    protected abstract boolean preCheckValue(Identifiable<?> equipment,
                                             AbstractAssignmentModel abstractAssignmentModel,
                                             List<ReportNode> reports, List<String> notEditableEquipments);

    protected abstract String getNewValue(Identifiable<?> equipment, AbstractAssignmentModel abstractAssignmentModel);

    protected boolean checkGeneratorsPowerValues(Identifiable<?> equipment, AbstractAssignmentModel abstractAssignmentModel, List<ReportNode> reports) {
        if (equipment.getType() == IdentifiableType.GENERATOR) {
            Generator generator = (Generator) equipment;
            if (abstractAssignmentModel.getEditedField().equals(PLANNED_ACTIVE_POWER_SET_POINT.name())) {
                return validateActivePowerValue(generator, FIELD_PLANNED_ACTIVE_POWER_SET_POINT, reports, Double.parseDouble(getNewValue(equipment, abstractAssignmentModel)));
            } else if (abstractAssignmentModel.getEditedField().equals(MINIMUM_ACTIVE_POWER.name())) {
                return validateMinimumActivePower(generator, reports, Double.parseDouble(getNewValue(equipment, abstractAssignmentModel)));
            } else if (abstractAssignmentModel.getEditedField().equals(MAXIMUM_ACTIVE_POWER.name())) {
                return validateMaximumActivePower(generator, reports, Double.parseDouble(getNewValue(equipment, abstractAssignmentModel)));
            } else if (abstractAssignmentModel.getEditedField().equals(ACTIVE_POWER_SET_POINT.name())) {
                double newValue = Double.parseDouble(getNewValue(equipment, abstractAssignmentModel));
                if (newValue != 0) { // 0 is an exception to the rule
                    return validateActivePowerValue(generator, FIELD_ACTIVE_POWER_TARGET, reports, newValue);
                }
            }
        }
        return true;
    }

    protected String getOldValue(Identifiable<?> equipment, AbstractAssignmentModel abstractAssignmentModel) {
        return getFieldValue(equipment, abstractAssignmentModel.getEditedField());
    }

    protected String applyValue(Identifiable<?> equipment, AbstractAssignmentModel abstractAssignmentModel) {
        // get new value
        String newValue = getNewValue(equipment, abstractAssignmentModel);

        // set new value for the equipment
        setFieldValue(equipment, abstractAssignmentModel.getEditedField(), newValue);
        return newValue;
    }

    @Override
    public void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService) {
        this.filterService = filterService;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (getModificationModel() == null) {
            throw new NetworkModificationException(getExceptionType(), "Missing required attributes to modify the equipment");
        }

        if (CollectionUtils.isEmpty(getAssignmentModelList())) {
            throw new NetworkModificationException(getExceptionType(), String.format("At least one %s is required", getModificationTypeLabel()));
        }

        if (getAssignmentModelList().stream().anyMatch(modificationByFilterModel -> CollectionUtils.isEmpty(modificationByFilterModel.getFilters()))) {
            throw new NetworkModificationException(getExceptionType(), String.format("Every %s must have at least one filter", getModificationTypeLabel()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // collect all filters from all variations
        Map<UUID, String> filters = getFilters();

        Map<UUID, FilterEquipments> filterUuidEquipmentsMap = ModificationUtils.getUuidFilterEquipmentsMap(filterService, network, subReportNode, filters, getModificationModel().getErrorType());

        if (filterUuidEquipmentsMap != null) {
            ReportNode subReporter = subReportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_APPLIED_BY_FILTER_MODIFICATIONS)
                    .withUntypedValue(VALUE_KEY_MODIFICATION_TYPE_LABEL, StringUtils.capitalize(getModificationTypeLabel()))
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_TYPE, getEquipmentType().name())
                    .add();
            // perform modifications
            getAssignmentModelList().forEach(abstractAssignmentModel -> {
                List<ReportNode> reports = new ArrayList<>();
                ReportNode eachAssignmentReporter = subReporter.newReportNode()
                        .withMessageTemplate(REPORT_KEY_APPLIED_ASSIGNMENT)
                        .withUntypedValue(VALUE_KEY_MODIFICATION_TYPE_LABEL, StringUtils.capitalize(getModificationTypeLabel()))
                        .withUntypedValue(VALUE_KEY_FILTERS_EACH_ASSIGNMENT, abstractAssignmentModel.getFilters().stream().map(FilterModel::getName)
                                .collect(Collectors.joining(", ")))
                        .add();
                abstractAssignmentModel.getFilters().forEach(filterModel -> applyOnFilterEquipments(network, filterUuidEquipmentsMap, reports, abstractAssignmentModel, filterModel));
                reports.forEach(report -> insertReportNode(eachAssignmentReporter, report));
            });
            // reporting
            if (equipmentNotModifiedCount == 0 && equipmentNotFoundCount == 0) {
                subReportNode.newReportNode()
                        .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_ALL)
                        .withUntypedValue(VALUE_KEY_EQUIPMENT_COUNT, equipmentCount)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();

            } else {
                if (equipmentNotModifiedCount == equipmentCount) {
                    createReport(subReportNode, REPORT_KEY_BY_FILTER_MODIFICATION_NONE, Map.of(), TypedValue.ERROR_SEVERITY);
                } else {
                    subReportNode.newReportNode()
                            .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_SOME)
                            .withUntypedValue(VALUE_KEY_NB_CHANGED, equipmentCount - equipmentNotModifiedCount)
                            .withUntypedValue(VALUE_KEY_NB_UNCHANGED, equipmentNotModifiedCount + equipmentNotFoundCount)
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .add();
                }
            }
        }
    }

    protected boolean isEquipmentEditable(Identifiable<?> equipment,
                                          AbstractAssignmentModel abstractAssignmentModel,
                                          List<ReportNode> equipmentsReport) {
        if (abstractAssignmentModel.getEditedField() == null) {
            return false;
        }
        return FieldUtils.isEquipmentEditable(equipment, abstractAssignmentModel.getEditedField(), equipmentsReport);
    }

    private void createAssignmentReports(List<ReportNode> reports, AbstractAssignmentModel abstractAssignmentModel,
                                         FilterModel filterModel, FilterEquipments filterEquipments, List<String> notEditableEquipments) {
        if (notEditableEquipments.size() == filterEquipments.getIdentifiableAttributes().size()) {
            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_FAILED)
                    .withUntypedValue(VALUE_KEY_FILTER_NAME, filterModel.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_SUCCESS)
                    .withUntypedValue(VALUE_KEY_MODIFICATION_TYPE_LABEL, getModificationTypeLabel())
                    .withUntypedValue(VALUE_KEY_FILTER_NAME, filterModel.getName())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_NUMBER_OF_VALID_EQUIPMENT)
                    .withUntypedValue(VALUE_KEY_NB_CHANGED, filterEquipments.getIdentifiableAttributes().size() - notEditableEquipments.size())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            if (!CollectionUtils.isEmpty(notEditableEquipments)) {
                reports.add(ReportNode.newRootReportNode()
                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                        .withMessageTemplate(REPORT_KEY_NOT_EDITED_EQUIPMENTS_FILTER)
                        .withUntypedValue(VALUE_KEY_NB_UNCHANGED, notEditableEquipments.size())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            }
        }

        reports.add(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate(REPORT_KEY_EDITED_FIELD_FILTER)
                .withUntypedValue(VALUE_KEY_FIELD_NAME, getEditedFieldLabel(abstractAssignmentModel))
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
            String equipmentIds = String.join(", ", filterEquipments.getNotFoundEquipments());
            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_FILTER_EQUIPMENTS_NOT_FOUND)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_IDS, equipmentIds)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        }
    }

    private void applyModification(Identifiable<?> equipment,
                                   AbstractAssignmentModel abstractAssignmentModel,
                                   List<ReportNode> reports,
                                   List<String> notEditableEquipments) {

        // check pre-conditions
        if (!preCheckValue(equipment, abstractAssignmentModel, reports, notEditableEquipments)) {
            return;
        }

        // perform to apply new value
        try {
            final String oldValue = getOldValue(equipment, abstractAssignmentModel);
            final String newValue = applyValue(equipment, abstractAssignmentModel);
            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_EQUIPMENT_MODIFIED_REPORT)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_TYPE, equipment.getType().name())
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                    .withUntypedValue(VALUE_KEY_FIELD_NAME, getEditedFieldLabel(abstractAssignmentModel))
                    .withUntypedValue(VALUE_KEY_OLD_VALUE, oldValue == null ? NO_VALUE : oldValue)
                    .withUntypedValue(VALUE_KEY_NEW_VALUE, newValue == null ? NO_VALUE : newValue)
                    .withUntypedValue(VALUE_KEY_ARROW_NAME, VALUE_KEY_ARROW_VALUE) // Workaround to use non-ISO-8859-1 characters in the internationalization file
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build());
        } catch (Exception e) {
            notEditableEquipments.add(equipment.getId());
            equipmentNotModifiedCount += 1;
            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_EQUIPMENT_MODIFIED_REPORT_EXCEPTION)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                    .withUntypedValue(VALUE_KEY_ERROR_MESSAGE, e.getMessage())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        }
    }

    private Map<UUID, String> getFilters() {
        return getAssignmentModelList().stream()
                .flatMap(v -> v.getFilters().stream())
                .filter(distinctByKey(FilterModel::getId))
                .collect(Collectors.toMap(FilterModel::getId, FilterModel::getName));
    }

    private void applyOnFilterEquipments(Network network,
                                         Map<UUID, FilterEquipments> filterUuidEquipmentsMap,
                                         List<ReportNode> reports,
                                         AbstractAssignmentModel abstractAssignmentModel,
                                         FilterModel filterModel) {
        FilterEquipments filterEquipments = filterUuidEquipmentsMap.get(filterModel.getId());

        if (filterEquipments == null) {
            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.filterNotFound")
                    .withUntypedValue(VALUE_KEY_NAME, filterModel.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            return;
        }

        if (CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes())) {
            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_BY_FILTER_MODIFICATION_NOT_FOUND)
                    .withUntypedValue(VALUE_KEY_FILTER_NAME, filterModel.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            equipmentCount += filterEquipments.getIdentifiableAttributes().size();
            if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
                equipmentNotFoundCount += filterEquipments.getNotFoundEquipments().size();
            }
            List<String> notEditableEquipments = new ArrayList<>();
            List<ReportNode> equipmentsReport = new ArrayList<>();
            filterEquipments.getIdentifiableAttributes()
                    .stream()
                    .map(attributes -> network.getIdentifiable(attributes.getId()))
                    .filter(equipment -> {
                        boolean isEditableEquipment = isEquipmentEditable(equipment, abstractAssignmentModel, equipmentsReport);
                        if (!isEditableEquipment) {
                            notEditableEquipments.add(equipment.getId());
                            equipmentNotModifiedCount += 1;
                        }
                        return isEditableEquipment;
                    })
                    .forEach(equipment -> applyModification(equipment, abstractAssignmentModel, equipmentsReport, notEditableEquipments));

            createAssignmentReports(reports, abstractAssignmentModel, filterModel, filterEquipments, notEditableEquipments);

            reports.addAll(equipmentsReport);
        }
    }

}
