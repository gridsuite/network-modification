/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

/**
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
public class InjectionModificationModel extends BasicEquipmentModificationModel {
    private AttributeModification<String> voltageLevelId;

    private AttributeModification<String> busOrBusbarSectionId;

    private AttributeModification<String> connectionName;

    private AttributeModification<ConnectablePosition.Direction> connectionDirection;

    private AttributeModification<Integer> connectionPosition;

    private AttributeModification<Boolean> terminalConnected;

    @JsonProperty("pMeasurementValue")
    private AttributeModification<Double> pMeasurementValue;

    @JsonProperty("pMeasurementValidity")
    private AttributeModification<Boolean> pMeasurementValidity;

    @JsonProperty("qMeasurementValue")
    private AttributeModification<Double> qMeasurementValue;

    @JsonProperty("qMeasurementValidity")
    private AttributeModification<Boolean> qMeasurementValidity;
}
