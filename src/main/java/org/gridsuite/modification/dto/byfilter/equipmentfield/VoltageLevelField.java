/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.OperationType;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_VOLTAGE_LEVEL_ERROR;
import static org.gridsuite.modification.modifications.GeneratorModification.ERROR_MESSAGE;
import static org.gridsuite.modification.modifications.VoltageLevelModification.*;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum VoltageLevelField {
    NOMINAL_VOLTAGE,
    LOW_VOLTAGE_LIMIT,
    HIGH_VOLTAGE_LIMIT,
    LOW_SHORT_CIRCUIT_CURRENT_LIMIT,
    HIGH_SHORT_CIRCUIT_CURRENT_LIMIT;

    public static String getReferenceValue(VoltageLevel voltageLevel, String voltageLevelField) {
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        VoltageLevelField field = VoltageLevelField.valueOf(voltageLevelField);
        return switch (field) {
            case NOMINAL_VOLTAGE -> String.valueOf(voltageLevel.getNominalV());
            case LOW_VOLTAGE_LIMIT -> String.valueOf(voltageLevel.getLowVoltageLimit());
            case HIGH_VOLTAGE_LIMIT -> String.valueOf(voltageLevel.getHighVoltageLimit());
            case LOW_SHORT_CIRCUIT_CURRENT_LIMIT -> identifiableShortCircuit != null ? String.valueOf(identifiableShortCircuit.getIpMin()) : null;
            case HIGH_SHORT_CIRCUIT_CURRENT_LIMIT -> identifiableShortCircuit != null ? String.valueOf(identifiableShortCircuit.getIpMax()) : null;
        };
    }

    public static void setNewValue(VoltageLevel voltageLevel, String voltageLevelField, String newValue) {
        VoltageLevelField field = VoltageLevelField.valueOf(voltageLevelField);
        final String errorMessage = String.format(ERROR_MESSAGE, voltageLevel.getId());
        switch (field) {
            case NOMINAL_VOLTAGE -> {
                Double nominalVoltage = Double.valueOf(newValue);
                checkIsNotNegativeValue(errorMessage, nominalVoltage, MODIFY_VOLTAGE_LEVEL_ERROR, "Nominal voltage");
                modifyNominalV(voltageLevel, new AttributeModification<>(nominalVoltage, OperationType.SET), null);
            }
            case LOW_VOLTAGE_LIMIT -> {
                Double lowVoltageLimit = newValue != null ? Double.parseDouble(newValue) : Double.NaN;
                checkIsNotNegativeValue(errorMessage, lowVoltageLimit, MODIFY_VOLTAGE_LEVEL_ERROR, "Low voltage limit");
                modifLowVoltageLimit(voltageLevel, new AttributeModification<>(lowVoltageLimit, OperationType.SET), null);
            }
            case HIGH_VOLTAGE_LIMIT -> {
                Double highVoltageLimit = newValue != null ? Double.parseDouble(newValue) : Double.NaN;
                checkIsNotNegativeValue(errorMessage, highVoltageLimit, MODIFY_VOLTAGE_LEVEL_ERROR, "High voltage limit");
                modifyHighVoltageLimit(voltageLevel, new AttributeModification<>(highVoltageLimit, OperationType.SET), null);
            }
            case LOW_SHORT_CIRCUIT_CURRENT_LIMIT -> modifyVoltageLevelShortCircuit(
                    new AttributeModification<>(newValue != null ? Double.parseDouble(newValue) : Double.NaN, OperationType.SET),
                    null, null, voltageLevel);
            case HIGH_SHORT_CIRCUIT_CURRENT_LIMIT -> modifyVoltageLevelShortCircuit(
                    null, new AttributeModification<>(newValue != null ? Double.parseDouble(newValue) : Double.NaN, OperationType.SET),
                    null, voltageLevel);
        }
    }
}
