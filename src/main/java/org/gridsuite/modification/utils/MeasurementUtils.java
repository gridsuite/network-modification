/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.ThreeSides;
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

    private static String getMeasurementTypeLabel(Measurement.Type type) {
        return switch (type) {
            case ACTIVE_POWER -> "Active power";
            case REACTIVE_POWER -> "Reactive power";
            case VOLTAGE -> "Voltage";
            default -> type.name();
        };
    }

    public static Measurement getExistingMeasurement(Measurements<?> measurements, Measurement.Type type) {
        return getExistingSideMeasurement(measurements, type, null);
    }

    public static Measurement getExistingSideMeasurement(Measurements<?> measurements, Measurement.Type type, ThreeSides side) {
        if (side == null) {
            return measurements.getMeasurements(type).stream().findFirst().orElse(null);
        }
        return measurements.getMeasurements(type).stream().filter(m -> m.getSide() == side).findFirst().orElse(null);
    }

    /** Upsert a measurement without producing report entries (used by by-filter assignment). */
    public static void upsertMeasurement(Measurements<?> measurements, Measurement.Type type, Double requestedValue, Boolean requestedValidity) {
        upsertMeasurement(measurements, type, requestedValue, requestedValidity, null);
    }

    /** Upsert a measurement and append change entries to {@code reports} (used by injection modification). */
    public static void upsertMeasurement(Measurements<?> measurements, Measurement.Type type, Double requestedValue, Boolean requestedValidity, List<ReportNode> reports) {
        upsertSideMeasurement(measurements, type, null, requestedValue, requestedValidity, reports);
    }

    /** Upsert a measurement and append change entries to {@code reports} (used by branch modification). */
    public static void upsertSideMeasurement(Measurements<?> measurements, Measurement.Type type, ThreeSides side, Double requestedValue, Boolean requestedValidity, List<ReportNode> reports) {
        if (requestedValue == null && requestedValidity == null) {
            return;
        }
        String logFieldPrefix = getMeasurementTypeLabel(type) + " measurement ";
        Measurement measurement = getExistingSideMeasurement(measurements, type, side);
        if (measurement != null) { // update measurement
            if (requestedValue != null) {
                double oldValue = measurement.getValue();
                measurement.setValue(requestedValue);
                if (reports != null) {
                    reports.add(ModificationUtils.buildModificationReport(oldValue, requestedValue, logFieldPrefix + VALUE, TypedValue.INFO_SEVERITY));
                }
            }
            if (requestedValidity != null) {
                boolean oldValidity = measurement.isValid();
                updateMeasurementValidity(measurement, requestedValidity);
                if (reports != null) {
                    reports.add(ModificationUtils.buildModificationReport(oldValidity, requestedValidity, logFieldPrefix + VALIDITY, TypedValue.INFO_SEVERITY));
                }
            }
        } else { // add new measurement
            var measurementAdder = measurements.newMeasurement().setId(UUID.randomUUID().toString()).setType(type);
            if (side != null) {
                measurementAdder.setSide(side);
            }
            if (requestedValue != null) {
                measurementAdder.setValue(requestedValue);
                if (reports != null) {
                    reports.add(ModificationUtils.buildModificationReport(null, requestedValue, logFieldPrefix + VALUE, TypedValue.INFO_SEVERITY));
                }
            }
            if (requestedValidity != null) {
                measurementAdder.setValid(requestedValidity);
                if (reports != null) {
                    reports.add(ModificationUtils.buildModificationReport(null, requestedValidity, logFieldPrefix + VALIDITY, TypedValue.INFO_SEVERITY));
                }
            }
            measurementAdder.add();
        }
    }

    public static void updateMeasurementValidity(Measurement measurement, boolean requestedValidity) {
        measurement.setValid(requestedValidity);
        if (measurement.getProperty(VALIDITY) != null) {
            if (requestedValidity) {
                switch (measurement.getProperty(VALIDITY)) {
                    //validity = 1 →  TM non-valid & not masked
                    //validity = 3 →  TM non-valid & masked
                    case "1": measurement.putProperty(VALIDITY, "0");
                        break;
                    case "3": measurement.putProperty(VALIDITY, "2");
                        break;
                    default: break;
                }
            } else {
                switch (measurement.getProperty(VALIDITY)) {
                    //validity = 0 →  TM valid & not masked
                    //validity = 2 →  TM valid & masked
                    case "0": measurement.putProperty(VALIDITY, "1");
                        break;
                    case "2": measurement.putProperty(VALIDITY, "3");
                        break;
                    default: break;
                }
            }
        }
    }
}
