/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;

import java.util.List;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("CONVERTER_STATION_MODIFICATION")
@ModificationErrorTypeName("MODIFY_CONVERTER_STATION_ERROR")
public class ConverterStationModificationModel extends InjectionModificationModel {
    private AttributeModification<Float> lossFactor;

    private AttributeModification<Double> reactivePowerSetpoint;

    private AttributeModification<Boolean> voltageRegulationOn;

    private AttributeModification<Double> voltageSetpoint;

    private AttributeModification<Boolean> reactiveCapabilityCurve;

    private AttributeModification<Double> minQ;

    private AttributeModification<Double> maxQ;

    private List<ReactiveCapabilityCurvePointsModel> reactiveCapabilityCurvePoints;

}
