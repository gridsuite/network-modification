/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Line;
import jakarta.validation.constraints.NotNull;
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
    B2;

    public static String getReferenceValue(Line line, String lineField) {
        LineField field = LineField.valueOf(lineField);
        return switch (field) {
            case R -> String.valueOf(line.getR());
            case X -> String.valueOf(line.getX());
            case G1 -> String.valueOf(line.getG1());
            case G2 -> String.valueOf(line.getG2());
            case B1 -> String.valueOf(line.getB1());
            case B2 -> String.valueOf(line.getB2());
        };
    }

    public static void setNewValue(Line line, String lineField, @NotNull String newValue) {
        LineField field = LineField.valueOf(lineField);
        String errorMessage = String.format(ERROR_MESSAGE, line.getId());
        Double doubleValue = Double.parseDouble(newValue);
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
        }
    }
}
