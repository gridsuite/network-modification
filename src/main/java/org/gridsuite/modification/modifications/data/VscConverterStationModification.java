/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.data;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;

import java.util.List;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Getter
@Setter
@SuperBuilder
public class VscConverterStationModification extends AbstractInjectionModification {

    private AttributeModification<Float> lossFactor;
    private AttributeModification<Double> reactivePowerSetpoint;
    private AttributeModification<Boolean> voltageRegulationOn;
    private AttributeModification<Double> voltageSetpoint;
    private AttributeModification<Boolean> reactiveCapabilityCurve;
    private AttributeModification<Double> minQ;
    private AttributeModification<Double> maxQ;
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;
}
