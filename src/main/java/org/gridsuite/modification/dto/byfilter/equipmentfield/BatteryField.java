/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import jakarta.validation.constraints.NotNull;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.utils.ModificationUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_BATTERY_ERROR;
import static org.gridsuite.modification.modifications.BatteryModification.*;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public enum BatteryField {
    MINIMUM_ACTIVE_POWER,
    MAXIMUM_ACTIVE_POWER,
    ACTIVE_POWER_SET_POINT,
    REACTIVE_POWER_SET_POINT,
    DROOP;

    public static String getReferenceValue(Battery battery, String batteryField) {
        ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
        BatteryField field = BatteryField.valueOf(batteryField);
        return switch (field) {
            case MINIMUM_ACTIVE_POWER -> String.valueOf(battery.getMinP());
            case MAXIMUM_ACTIVE_POWER -> String.valueOf(battery.getMaxP());
            case ACTIVE_POWER_SET_POINT -> String.valueOf(battery.getTargetP());
            case REACTIVE_POWER_SET_POINT -> String.valueOf(battery.getTargetQ());
            case DROOP -> activePowerControl != null ? String.valueOf(activePowerControl.getDroop()) : null;
        };
    }

    public static void setNewValue(Battery battery, String batteryField, @NotNull String newValue) {
        BatteryField field = BatteryField.valueOf(batteryField);
        String errorMessage = String.format(ERROR_MESSAGE, battery.getId());
        final AttributeModification<Double> attributeModification = new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET);
        switch (field) {
            case MINIMUM_ACTIVE_POWER ->
                    modifyBatteryActiveLimitsAttributes(null, attributeModification, battery, null);
            case MAXIMUM_ACTIVE_POWER ->
                    modifyBatteryActiveLimitsAttributes(attributeModification, null, battery, null);
            case ACTIVE_POWER_SET_POINT -> {
                ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                        attributeModification, null, null, battery.getMinP(),
                        battery.getMaxP(), battery.getTargetP(), MODIFY_BATTERY_ERROR, errorMessage
                );
                modifyBatterySetpointsAttributes(attributeModification, null, null, null, battery, null);
            }
            case REACTIVE_POWER_SET_POINT -> modifyBatterySetpointsAttributes(
                    null, attributeModification, null, null, battery, null);
            case DROOP -> {
                Float droopValue = Float.parseFloat(newValue);
                ModificationUtils.checkIsPercentage(errorMessage, droopValue, MODIFY_BATTERY_ERROR, "Droop");
                ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
                ActivePowerControlAdder<Battery> activePowerControlAdder = battery.newExtension(ActivePowerControlAdder.class);
                ModificationUtils.getInstance().modifyActivePowerControlAttributes(
                        activePowerControl, activePowerControlAdder, null,
                        new AttributeModification<>(droopValue, OperationType.SET), null,
                    null, MODIFY_BATTERY_ERROR, errorMessage);
            }
        }
    }
}
