/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import com.powsybl.iidm.network.extensions.DiscreteMeasurement;
import com.powsybl.iidm.network.extensions.DiscreteMeasurements;
import com.powsybl.iidm.network.extensions.DiscreteMeasurementsAdder;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * Helpers to upsert the tap position discrete measurements of a two windings transformer.
 */
public final class DiscreteMeasurementUtils {

    private static final String RATIO_TAP_POSITION_MEASUREMENT = "Ratio tap changer position measurement";
    private static final String PHASE_TAP_POSITION_MEASUREMENT = "Phase tap changer position measurement";

    private DiscreteMeasurementUtils() {
    }

    public static DiscreteMeasurement getExistingTapPositionMeasurement(DiscreteMeasurements<?> discreteMeasurements,
                                                                        DiscreteMeasurement.TapChanger tapChanger) {
        return discreteMeasurements.getDiscreteMeasurements(DiscreteMeasurement.Type.TAP_POSITION).stream()
                .filter(m -> m.getTapChanger() == tapChanger)
                .findFirst()
                .orElse(null);
    }

    /**
     * Upsert the requested tap position measurements of the transformer, each one being marked as valid.
     * A null requested value is a no-op. The discrete measurements extension is only created when needed.
     *
     * @return the change reports, empty when nothing has been requested
     */
    public static List<ReportNode> applyTapPositionMeasurements(TwoWindingsTransformer twt, Integer ratioTapPosition, Integer phaseTapPosition) {
        List<ReportNode> reports = new ArrayList<>();
        if (ratioTapPosition == null && phaseTapPosition == null) {
            return reports;
        }
        DiscreteMeasurements<?> discreteMeasurements = (DiscreteMeasurements<?>) twt.getExtension(DiscreteMeasurements.class);
        if (discreteMeasurements == null) {
            DiscreteMeasurementsAdder<?> adder = twt.newExtension(DiscreteMeasurementsAdder.class);
            discreteMeasurements = adder.add();
        }
        upsertTapPositionMeasurement(discreteMeasurements, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, ratioTapPosition,
                RATIO_TAP_POSITION_MEASUREMENT, reports);
        upsertTapPositionMeasurement(discreteMeasurements, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, phaseTapPosition,
                PHASE_TAP_POSITION_MEASUREMENT, reports);
        return reports;
    }

    private static void upsertTapPositionMeasurement(DiscreteMeasurements<?> discreteMeasurements, DiscreteMeasurement.TapChanger tapChanger,
                                                     Integer requestedValue, String logFieldPrefix, List<ReportNode> reports) {
        if (requestedValue == null) {
            return;
        }
        DiscreteMeasurement measurement = getExistingTapPositionMeasurement(discreteMeasurements, tapChanger);
        if (measurement != null) {
            Object oldValue = getOldValue(measurement);
            boolean oldValidity = measurement.isValid();
            measurement.setValue(requestedValue);
            measurement.setValid(true); // Always true because front doesn't ask for validity
            reports.add(ModificationUtils.buildModificationReport(oldValue, requestedValue, logFieldPrefix + " value", TypedValue.INFO_SEVERITY));
            if (!oldValidity) {
                reports.add(ModificationUtils.buildModificationReport(false, true, logFieldPrefix + " validity", TypedValue.INFO_SEVERITY));
            }
        } else {
            discreteMeasurements.newDiscreteMeasurement()
                    .setId(UUID.randomUUID().toString())
                    .setType(DiscreteMeasurement.Type.TAP_POSITION)
                    .setTapChanger(tapChanger)
                    .setValue(requestedValue)
                    .setValid(true)
                    .add();
            reports.add(ModificationUtils.buildModificationReport(null, requestedValue, logFieldPrefix + " value", TypedValue.INFO_SEVERITY));
        }
    }

    /** Safe read of the previous value, which may be undefined (invalid measurement) or not an integer. */
    private static Object getOldValue(DiscreteMeasurement measurement) {
        DiscreteMeasurement.ValueType valueType = measurement.getValueType();
        if (valueType == null) {
            return null;
        }
        return switch (valueType) {
            case INT -> measurement.getValueAsInt();
            case BOOLEAN -> measurement.getValueAsBoolean();
            case STRING -> measurement.getValueAsString();
        };
    }
}
