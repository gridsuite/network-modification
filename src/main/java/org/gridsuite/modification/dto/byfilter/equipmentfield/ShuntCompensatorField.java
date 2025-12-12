/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import com.powsybl.iidm.network.VoltageLevel;
import jakarta.validation.constraints.NotNull;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.dto.ShuntCompensatorType;

import static org.gridsuite.modification.modifications.ShuntCompensatorModification.*;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum ShuntCompensatorField {
    MAXIMUM_SECTION_COUNT,
    SECTION_COUNT,
    MAX_SUSCEPTANCE,
    MAX_Q_AT_NOMINAL_V;

    public static String getReferenceValue(ShuntCompensator shuntCompensator, String shuntCompensatorField) {
        VoltageLevel voltageLevel = shuntCompensator.getTerminal().getVoltageLevel();
        ShuntCompensatorField field = ShuntCompensatorField.valueOf(shuntCompensatorField);
        double bPerSection = shuntCompensator.getB() / shuntCompensator.getSectionCount();
        return switch (field) {
            case MAXIMUM_SECTION_COUNT -> String.valueOf(shuntCompensator.getMaximumSectionCount());
            case SECTION_COUNT -> String.valueOf(shuntCompensator.getSectionCount());
            case MAX_SUSCEPTANCE -> String.valueOf(bPerSection * shuntCompensator.getMaximumSectionCount());
            case MAX_Q_AT_NOMINAL_V -> String.valueOf(Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * bPerSection) * shuntCompensator.getMaximumSectionCount());
        };
    }

    public static void setNewValue(ShuntCompensator shuntCompensator, String shuntCompensatorField, @NotNull String newValue) {
        if (shuntCompensator.getModelType() != ShuntCompensatorModelType.LINEAR) {
            throw new NetworkModificationRunException(String.format("Shunt compensator with %s model is not supported", shuntCompensator.getModelType()));
        }
        ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        ShuntCompensatorField field = ShuntCompensatorField.valueOf(shuntCompensatorField);
        VoltageLevel voltageLevel = shuntCompensator.getTerminal().getVoltageLevel();
        var shuntCompensatorType = ShuntCompensatorType.REACTOR;
        if (model != null && model.getBPerSection() > 0) {
            shuntCompensatorType = ShuntCompensatorType.CAPACITOR;
        }
        switch (field) {
            case MAXIMUM_SECTION_COUNT -> modifyMaximumSectionCount(new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET),
                    null, null, null, shuntCompensator, model);
            case SECTION_COUNT -> modifySectionCount(new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, shuntCompensator);
            case MAX_SUSCEPTANCE -> modifyMaxSusceptance(new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET),
                    shuntCompensator.getMaximumSectionCount(), null, model);
            case MAX_Q_AT_NOMINAL_V -> modifyMaximumQAtNominalVoltage(new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET),
                    voltageLevel, shuntCompensator.getMaximumSectionCount(), null, model, shuntCompensatorType);
        }
    }
}
