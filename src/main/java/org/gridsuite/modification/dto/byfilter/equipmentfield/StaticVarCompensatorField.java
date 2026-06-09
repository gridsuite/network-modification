/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import com.powsybl.iidm.network.extensions.MeasurementsAdder;
import jakarta.validation.constraints.NotNull;

import static org.gridsuite.modification.modifications.AbstractInjectionModification.getExistingMeasurement;
import static org.gridsuite.modification.modifications.AbstractInjectionModification.upsertMeasurement;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public enum StaticVarCompensatorField {
    REACTIVE_POWER_MEASUREMENT_VALUE,
    REACTIVE_POWER_MEASUREMENT_VALIDITY;

    public static String getReferenceValue(StaticVarCompensator svc, String svcField) {
        StaticVarCompensatorField field = StaticVarCompensatorField.valueOf(svcField);
        Measurements<?> measurements = (Measurements<?>) svc.getExtension(Measurements.class);
        if (measurements == null) {
            return null;
        }
        Measurement measurement = getExistingMeasurement(measurements, Measurement.Type.REACTIVE_POWER);
        if (measurement == null) {
            return null;
        }
        return switch (field) {
            case REACTIVE_POWER_MEASUREMENT_VALUE -> String.valueOf(measurement.getValue());
            case REACTIVE_POWER_MEASUREMENT_VALIDITY -> String.valueOf(measurement.isValid());
        };
    }

    public static void setNewValue(StaticVarCompensator svc, String svcField, @NotNull String newValue) {
        StaticVarCompensatorField field = StaticVarCompensatorField.valueOf(svcField);
        Measurements<?> measurements = (Measurements<?>) svc.getExtension(Measurements.class);
        if (measurements == null) {
            MeasurementsAdder<?> measurementsAdder = svc.newExtension(MeasurementsAdder.class);
            measurements = measurementsAdder.add();
        }
        switch (field) {
            case REACTIVE_POWER_MEASUREMENT_VALUE -> upsertMeasurement(measurements, Measurement.Type.REACTIVE_POWER, Double.parseDouble(newValue), null, null);
            case REACTIVE_POWER_MEASUREMENT_VALIDITY -> upsertMeasurement(measurements, Measurement.Type.REACTIVE_POWER, null, Boolean.parseBoolean(newValue), null);
        }
    }
}
