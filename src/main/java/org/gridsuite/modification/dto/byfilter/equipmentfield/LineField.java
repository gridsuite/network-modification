/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.OperationalLimitsGroup;
import com.powsybl.iidm.network.TwoSides;
import groovyjarjarantlr4.v4.runtime.misc.NotNull;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.utils.ModificationUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_LINE_ERROR;
import static org.gridsuite.modification.modifications.LineModification.*;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public enum LineField {
    R,
    X,
    G1,
    G2,
    B1,
    B2,
    SELECTED_OPERATIONAL_LIMITS_GROUP_1,
    SELECTED_OPERATIONAL_LIMITS_GROUP_2;

    public static String getReferenceValue(Line line, String lineField) {
        LineField field = LineField.valueOf(lineField);
        return switch (field) {
            case R -> String.valueOf(line.getR());
            case X -> String.valueOf(line.getX());
            case G1 -> String.valueOf(line.getG1());
            case G2 -> String.valueOf(line.getG2());
            case B1 -> String.valueOf(line.getB1());
            case B2 -> String.valueOf(line.getB2());
            case SELECTED_OPERATIONAL_LIMITS_GROUP_1 -> String.valueOf(line.getSelectedOperationalLimitsGroupId1().orElse(null));
            case SELECTED_OPERATIONAL_LIMITS_GROUP_2 -> String.valueOf(line.getSelectedOperationalLimitsGroupId2().orElse(null));
        };
    }

    public static void setNewValue(Line line, String lineField, @NotNull String newValue) {
        LineField field = LineField.valueOf(lineField);
        String errorMessage = String.format(ERROR_MESSAGE, line.getId());
        switch (field) {
            case R, X, G1, G2, B1, B2 -> setNewDoubleValue(line, field, newValue, errorMessage);
            case SELECTED_OPERATIONAL_LIMITS_GROUP_1, SELECTED_OPERATIONAL_LIMITS_GROUP_2 -> setNewStringValue(line, field, newValue, errorMessage);
        }
    }

    private static void setNewDoubleValue(Line line, LineField field, String newValue, String errorMessage) {
        Double doubleValue = newValue != null ? Double.parseDouble(newValue) : Double.NaN;
        final AttributeModification<Double> attributeModification = new AttributeModification<>(doubleValue, OperationType.SET);
        switch (field) {
            case R -> {
                ModificationUtils.checkIsNotNegativeValue(errorMessage, doubleValue, MODIFY_LINE_ERROR, "Resistance R");
                modifyR(line, attributeModification, null);
            }
            case X -> modifyX(line, attributeModification, null);
            case G1 -> {
                ModificationUtils.checkIsNotNegativeValue(errorMessage, doubleValue, MODIFY_LINE_ERROR, "Conductance G on side 1");
                modifyG1(line, attributeModification, null);
            }
            case G2 -> {
                ModificationUtils.checkIsNotNegativeValue(errorMessage, doubleValue, MODIFY_LINE_ERROR, "Conductance G on side 2");
                modifyG2(line, attributeModification, null);
            }
            case B1 -> modifyB1(line, attributeModification, null);
            case B2 -> modifyB2(line, attributeModification, null);
            default -> throw new IllegalArgumentException(String.format("field %s is not a double modification", field));
        }
    }

    private static void setNewStringValue(Line line, LineField field, String newValue, String errorMessage) {
        final AttributeModification<String> attributeModification = new AttributeModification<>(newValue, OperationType.SET);
        switch (field) {
            case SELECTED_OPERATIONAL_LIMITS_GROUP_1 -> {
                ModificationUtils.checkLimitsGroupExist(errorMessage, newValue, MODIFY_LINE_ERROR,
                    line.getOperationalLimitsGroups1()
                    .stream()
                    .map(OperationalLimitsGroup::getId)
                    .toList(), 1);
                modifySelectedOperationalLimitsGroup(line, attributeModification, TwoSides.ONE, null);
            }
            case SELECTED_OPERATIONAL_LIMITS_GROUP_2 -> {
                ModificationUtils.checkLimitsGroupExist(errorMessage, newValue, MODIFY_LINE_ERROR,
                    line.getOperationalLimitsGroups2()
                        .stream()
                        .map(OperationalLimitsGroup::getId)
                        .toList(), 2);
                modifySelectedOperationalLimitsGroup(line, attributeModification, TwoSides.TWO, null);
            }
            default -> throw new IllegalArgumentException(String.format("field %s is not a string modification", field));
        }
    }
}
