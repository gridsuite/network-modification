/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Injection;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.utils.MeasurementUtils;

import java.util.List;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Getter
@Setter
@NoArgsConstructor
public abstract class AbstractInjectionModification extends AbstractEquipmentModification implements InjectionModification {

    protected AttributeModification<String> voltageLevelId;
    protected AttributeModification<String> busOrBusbarSectionId;
    protected AttributeModification<String> connectionName;
    protected AttributeModification<ConnectablePosition.Direction> connectionDirection;
    protected AttributeModification<Integer> connectionPosition;
    protected AttributeModification<Boolean> terminalConnected;
    protected AttributeModification<Double> pMeasurementValue;
    protected AttributeModification<Boolean> pMeasurementValidity;
    protected AttributeModification<Double> qMeasurementValue;
    protected AttributeModification<Boolean> qMeasurementValidity;

    protected AbstractInjectionModification(String equipmentId, List<FreePropertyInfos> properties,
                                         AttributeModification<String> equipmentName,
                                         AttributeModification<String> voltageLevelId,
                                         AttributeModification<String> busOrBusbarSectionId,
                                         AttributeModification<String> connectionName,
                                         AttributeModification<ConnectablePosition.Direction> connectionDirection,
                                         AttributeModification<Integer> connectionPosition,
                                         AttributeModification<Boolean> terminalConnected,
                                         AttributeModification<Double> pMeasurementValue,
                                         AttributeModification<Boolean> pMeasurementValidity,
                                         AttributeModification<Double> qMeasurementValue,
                                         AttributeModification<Boolean> qMeasurementValidity) {
        super(equipmentId, properties, equipmentName);
        this.voltageLevelId = voltageLevelId;
        this.busOrBusbarSectionId = busOrBusbarSectionId;
        this.connectionName = connectionName;
        this.connectionDirection = connectionDirection;
        this.connectionPosition = connectionPosition;
        this.terminalConnected = terminalConnected;
        this.pMeasurementValue = pMeasurementValue;
        this.pMeasurementValidity = pMeasurementValidity;
        this.qMeasurementValue = qMeasurementValue;
        this.qMeasurementValidity = qMeasurementValidity;
    }

    protected ReportNode updateMeasurements(Injection<?> injection, ReportNode subReportNode) {
        return MeasurementUtils.applyAndBuildModificationReport(injection, this, subReportNode);
    }
}
