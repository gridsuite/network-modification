/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Injection;
import org.gridsuite.modification.dto.InjectionModificationInfos;
import org.gridsuite.modification.utils.MeasurementUtils;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public abstract class AbstractInjectionModification extends AbstractModification {

    protected final InjectionModificationInfos modificationInfos;

    protected AbstractInjectionModification(InjectionModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    protected ReportNode updateMeasurements(Injection<?> injection, ReportNode subReportNode) {
        return MeasurementUtils.applyAndBuildModificationReport(injection, modificationInfos, subReportNode);
    }
}
