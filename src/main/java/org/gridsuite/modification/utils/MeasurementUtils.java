/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;

import java.util.List;
import java.util.UUID;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public final class MeasurementUtils {

    private static final String VALUE = "value";
    private static final String VALIDITY = "validity";

    private MeasurementUtils() {
    }

    public static Measurement getExistingMeasurement(Measurements<?> measurements, Measurement.Type type) {
        return measurements.getMeasurements(type).stream().findFirst().orElse(null);
    }

    /** Upsert a measurement without producing report entries (used by by-filter assignment). */
    public static void upsertMeasurement(Measurements<?> measurements, Measurement.Type type, Double value, Boolean requestedValidity) {
        upsertMeasurement(measurements, type, value, requestedValidity, null);
    }

    /** Upsert a measurement and append change entries to {@code reports} (used by injection modification). */
    public static void upsertMeasurement(Measurements<?> measurements, Measurement.Type type, Double value, Boolean requestedValidity, List<ReportNode> reports) {
        if (value == null && requestedValidity == null) {
            return;
        }
        String measurementType = (type == Measurement.Type.ACTIVE_POWER ? "Active power" : "Reactive power") + " measurement ";
        Measurement measurement = getExistingMeasurement(measurements, type);
        if (measurement != null) {
            if (value != null) {
                double oldValue = measurement.getValue();
                measurement.setValue(value);
                if (reports != null) {
                    reports.add(ModificationUtils.buildModificationReport(oldValue, value, measurementType + VALUE, TypedValue.INFO_SEVERITY));
                }
            }
            if (requestedValidity != null) {
                boolean oldValidity = measurement.isValid();
                ModificationUtils.updateMeasurementValidity(measurement, requestedValidity);
                if (reports != null) {
                    reports.add(ModificationUtils.buildModificationReport(oldValidity, requestedValidity, measurementType + VALIDITY, TypedValue.INFO_SEVERITY));
                }
            }
        } else {
            var measurementAdder = measurements.newMeasurement().setId(UUID.randomUUID().toString()).setType(type);
            if (value != null) {
                measurementAdder.setValue(value);
                if (reports != null) {
                    reports.add(ModificationUtils.buildModificationReport(null, value, measurementType + VALUE, TypedValue.INFO_SEVERITY));
                }
            }
            if (requestedValidity != null) {
                measurementAdder.setValid(requestedValidity);
                if (reports != null) {
                    reports.add(ModificationUtils.buildModificationReport(null, requestedValidity, measurementType + VALIDITY, TypedValue.INFO_SEVERITY));
                }
            }
            measurementAdder.add();
        }
    }
}
