/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import jakarta.validation.constraints.NotNull;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.List;

import static org.gridsuite.modification.modifications.GeneratorModification.ERROR_MESSAGE;
import static org.gridsuite.modification.modifications.TwoWindingsTransformerModification.*;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.parseDoubleOrNaNIfNull;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum TwoWindingsTransformerField {
    R,
    X,
    G,
    B,
    RATED_U1,
    RATED_U2,
    RATED_S,
    TARGET_V,
    RATIO_LOW_TAP_POSITION,
    RATIO_TAP_POSITION,
    RATIO_TARGET_DEADBAND,
    REGULATION_VALUE,
    PHASE_LOW_TAP_POSITION,
    PHASE_TAP_POSITION,
    PHASE_TARGET_DEADBAND,
    SELECTED_OPERATIONAL_LIMITS_GROUP_1,
    SELECTED_OPERATIONAL_LIMITS_GROUP_2;

    public static final String VALUE_KEY_FIELD_NAME = "fieldName";
    public static final String VALUE_KEY_EQUIPMENT_NAME = "equipmentName";

    public static final String REPORT_KEY_RATIO_TAP_CHANGER_EQUIPMENT_MODIFIED_ERROR = "network.modification.ratioTapChangerEquipmentModifiedError";
    public static final String REPORT_KEY_PHASE_TAP_CHANGER_EQUIPMENT_MODIFIED_ERROR = "network.modification.phaseTapChangerEquipmentModifiedError";

    public static boolean isEquipmentEditable(TwoWindingsTransformer twoWindingsTransformer, String editedField, List<ReportNode> equipmentsReport) {
        TwoWindingsTransformerField field = TwoWindingsTransformerField.valueOf(editedField);

        return switch (field) {
            case TARGET_V, RATIO_LOW_TAP_POSITION, RATIO_TAP_POSITION, RATIO_TARGET_DEADBAND -> {
                boolean isEditable = twoWindingsTransformer.getRatioTapChanger() != null;
                if (!isEditable) {
                    equipmentsReport.add(ReportNode.newRootReportNode()
                            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                            .withMessageTemplate(REPORT_KEY_RATIO_TAP_CHANGER_EQUIPMENT_MODIFIED_ERROR)
                            .withUntypedValue(VALUE_KEY_FIELD_NAME, field.name())
                            .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, twoWindingsTransformer.getId())
                            .withSeverity(TypedValue.DETAIL_SEVERITY)
                            .build());
                }
                yield isEditable;
            }
            case REGULATION_VALUE, PHASE_LOW_TAP_POSITION, PHASE_TAP_POSITION, PHASE_TARGET_DEADBAND -> {
                boolean isEditable = twoWindingsTransformer.getPhaseTapChanger() != null;
                if (!isEditable) {
                    equipmentsReport.add(ReportNode.newRootReportNode()
                            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                            .withMessageTemplate(REPORT_KEY_PHASE_TAP_CHANGER_EQUIPMENT_MODIFIED_ERROR)
                            .withUntypedValue(VALUE_KEY_FIELD_NAME, field.name())
                            .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, twoWindingsTransformer.getId())
                            .withSeverity(TypedValue.DETAIL_SEVERITY)
                            .build());
                }
                yield isEditable;
            }
            default -> true;
        };

    }

    public static String getReferenceValue(TwoWindingsTransformer transformer, String twoWindingsTransformerField) {
        TwoWindingsTransformerField field = TwoWindingsTransformerField.valueOf(twoWindingsTransformerField);
        final PhaseTapChanger phaseTapChanger = transformer.getPhaseTapChanger();
        final RatioTapChanger ratioTapChanger = transformer.getRatioTapChanger();
        return switch (field) {
            case R -> String.valueOf(transformer.getR());
            case X -> String.valueOf(transformer.getX());
            case G -> String.valueOf(transformer.getG());
            case B -> String.valueOf(transformer.getB());
            case RATED_U1 -> String.valueOf(transformer.getRatedU1());
            case RATED_U2 -> String.valueOf(transformer.getRatedU2());
            case RATED_S -> String.valueOf(transformer.getRatedS());
            case TARGET_V -> ratioTapChanger != null ? String.valueOf(ratioTapChanger.getTargetV()) : null;
            case RATIO_LOW_TAP_POSITION ->
                ratioTapChanger != null ? String.valueOf(ratioTapChanger.getLowTapPosition()) : null;
            case RATIO_TAP_POSITION ->
                ratioTapChanger != null ? String.valueOf(ratioTapChanger.getTapPosition()) : null;
            case RATIO_TARGET_DEADBAND ->
                ratioTapChanger != null ? String.valueOf(ratioTapChanger.getTargetDeadband()) : null;
            case REGULATION_VALUE ->
                phaseTapChanger != null ? String.valueOf(phaseTapChanger.getRegulationValue()) : null;
            case PHASE_LOW_TAP_POSITION ->
                phaseTapChanger != null ? String.valueOf(phaseTapChanger.getLowTapPosition()) : null;
            case PHASE_TAP_POSITION ->
                phaseTapChanger != null ? String.valueOf(phaseTapChanger.getTapPosition()) : null;
            case PHASE_TARGET_DEADBAND ->
                phaseTapChanger != null ? String.valueOf(phaseTapChanger.getTargetDeadband()) : null;
            case SELECTED_OPERATIONAL_LIMITS_GROUP_1 ->
                String.valueOf(transformer.getSelectedOperationalLimitsGroupId1().orElse(null));
            case SELECTED_OPERATIONAL_LIMITS_GROUP_2 ->
                String.valueOf(transformer.getSelectedOperationalLimitsGroupId2().orElse(null));
        };
    }

    public static void setNewValue(TwoWindingsTransformer transformer, String twoWindingsTransformerField, @NotNull String newValue) {
        TwoWindingsTransformerField field = TwoWindingsTransformerField.valueOf(twoWindingsTransformerField);
        final String errorMessage = String.format(ERROR_MESSAGE, transformer.getId());

        switch (field) {
            case R, X, G, B, RATED_U1, RATED_U2, RATED_S, TARGET_V, RATIO_LOW_TAP_POSITION, RATIO_TAP_POSITION,
                 RATIO_TARGET_DEADBAND, REGULATION_VALUE, PHASE_LOW_TAP_POSITION, PHASE_TAP_POSITION,
                 PHASE_TARGET_DEADBAND -> setNewDoubleValue(transformer, field, newValue, errorMessage);
            case SELECTED_OPERATIONAL_LIMITS_GROUP_1, SELECTED_OPERATIONAL_LIMITS_GROUP_2 ->
                setNewStringValue(transformer, field, newValue, errorMessage);
        }
    }

    private static void setNewDoubleValue(TwoWindingsTransformer transformer, TwoWindingsTransformerField field, String newValue, String errorMessage) {
        final PhaseTapChanger phaseTapChanger = transformer.getPhaseTapChanger();
        final RatioTapChanger ratioTapChanger = transformer.getRatioTapChanger();
        final Double doubleValue = parseDoubleOrNaNIfNull(newValue);
        final AttributeModification<Double> attributeModification = new AttributeModification<>(doubleValue, OperationType.SET);
        switch (field) {
            case R -> {
                checkIsNotNegativeValue(errorMessage, doubleValue, "Resistance R");
                modifyR(transformer, attributeModification, null);
            }
            case X -> modifyX(transformer, attributeModification, null);
            case G -> {
                checkIsNotNegativeValue(errorMessage, doubleValue, "Conductance G");
                modifyG(transformer, attributeModification, null);
            }
            case B -> modifyB(transformer, attributeModification, null);
            case RATED_U1 -> {
                checkIsNotNegativeValue(errorMessage, doubleValue, "Rated Voltage on side 1");
                modifyRatedU1(transformer, attributeModification, null);
            }
            case RATED_U2 -> {
                checkIsNotNegativeValue(errorMessage, doubleValue, "Rated Voltage on side 2");
                modifyRatedU2(transformer, attributeModification, null);
            }
            case RATED_S -> {
                checkIsNotNegativeValue(errorMessage, doubleValue, "Rated nominal power");
                modifyRatedS(transformer, attributeModification, null);
            }
            case TARGET_V -> {
                checkIsNotNegativeValue(errorMessage, doubleValue, "Target Voltage");
                modifyTargets(ratioTapChanger, null, true, attributeModification, null, null);
            }
            case RATIO_LOW_TAP_POSITION -> processTapChangerPositionsAndSteps(ratioTapChanger, null, true,
                new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, null, null);
            case RATIO_TAP_POSITION -> processTapChangerPositionsAndSteps(ratioTapChanger, null, true,
                null, new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, null);
            case RATIO_TARGET_DEADBAND -> modifyTargets(ratioTapChanger, null, true, null, attributeModification, null);
            case REGULATION_VALUE -> processPhaseTapRegulation(
                phaseTapChanger, null, true, null, attributeModification, null, null, null);
            case PHASE_LOW_TAP_POSITION -> processTapChangerPositionsAndSteps(phaseTapChanger, null, true,
                new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, null, null);
            case PHASE_TAP_POSITION -> processTapChangerPositionsAndSteps(phaseTapChanger, null, true,
                null, new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, null);
            case PHASE_TARGET_DEADBAND -> processPhaseTapRegulation(
                phaseTapChanger, null, true, null, null, attributeModification, null, null
            );
            default -> throw new IllegalArgumentException(String.format("field %s is not a double modification", field));
        }
    }

    private static void setNewStringValue(TwoWindingsTransformer transformer, TwoWindingsTransformerField field, String newValue, String errorMessage) {
        final AttributeModification<String> attributeModification = new AttributeModification<>(newValue, OperationType.SET);
        switch (field) {
            case SELECTED_OPERATIONAL_LIMITS_GROUP_1 -> {
                ModificationUtils.checkLimitsGroupExist(errorMessage, newValue,
                    transformer.getOperationalLimitsGroups1()
                        .stream()
                        .map(OperationalLimitsGroup::getId)
                        .toList(), 1);
                modifySelectedOperationalLimitsGroup(transformer, attributeModification, TwoSides.ONE, null);
            }
            case SELECTED_OPERATIONAL_LIMITS_GROUP_2 -> {
                ModificationUtils.checkLimitsGroupExist(errorMessage, newValue,
                    transformer.getOperationalLimitsGroups2()
                        .stream()
                        .map(OperationalLimitsGroup::getId)
                        .toList(), 2);
                modifySelectedOperationalLimitsGroup(transformer, attributeModification, TwoSides.TWO, null);
            }
            default -> throw new IllegalArgumentException(String.format("field %s is not a string modification", field));
        }
    }

}
