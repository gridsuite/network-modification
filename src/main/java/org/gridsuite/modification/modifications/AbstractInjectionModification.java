/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Injection;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import com.powsybl.iidm.network.extensions.MeasurementsAdder;
import org.gridsuite.modification.dto.InjectionModificationInfos;
import org.gridsuite.modification.utils.MeasurementUtils;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public abstract class AbstractInjectionModification extends AbstractModification {

    protected final InjectionModificationInfos modificationInfos;

    protected AbstractInjectionModification(InjectionModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    protected ReportNode updateMeasurements(Injection<?> injection, InjectionModificationInfos injectionModificationInfos, ReportNode subReportNode) {
        Double pValue = injectionModificationInfos.getPMeasurementValue() != null ? injectionModificationInfos.getPMeasurementValue().getValue() : null;
        Double qValue = injectionModificationInfos.getQMeasurementValue() != null ? injectionModificationInfos.getQMeasurementValue().getValue() : null;
        Boolean pValidity = injectionModificationInfos.getPMeasurementValidity() != null ? injectionModificationInfos.getPMeasurementValidity().getValue() : null;
        Boolean qValidity = injectionModificationInfos.getQMeasurementValidity() != null ? injectionModificationInfos.getQMeasurementValidity().getValue() : null;

        if (pValue == null && pValidity == null && qValue == null && qValidity == null) {
            // no measurement modification requested
            return null;
        }
        Measurements<?> measurements = (Measurements<?>) injection.getExtension(Measurements.class);
        if (measurements == null) {
            MeasurementsAdder<?> measurementsAdder = injection.newExtension(MeasurementsAdder.class);
            measurements = measurementsAdder.add();
        }
        // measurements update
        List<ReportNode> reports = new ArrayList<>();
        MeasurementUtils.upsertMeasurement(measurements, Measurement.Type.ACTIVE_POWER, pValue, pValidity, reports);
        MeasurementUtils.upsertMeasurement(measurements, Measurement.Type.REACTIVE_POWER, qValue, qValidity, reports);
        // report changes
        ReportNode estimSubReportNode = null;
        if (!reports.isEmpty()) {
            estimSubReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.stateEstimationData").add();
            ModificationUtils.getInstance().reportModifications(estimSubReportNode, reports, "network.modification.measurements");
        }
        return estimSubReportNode;
    }

}
