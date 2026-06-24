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
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.dto.ReactiveLimitsHolderInfos;

import java.util.List;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Getter
@Setter
@SuperBuilder
public class VscConverterStationCreation extends AbstractInjectionCreation implements ReactiveLimitsHolderInfos {

    private Float lossFactor;
    private Double reactivePowerSetpoint;
    private Boolean voltageRegulationOn;
    private Double voltageSetpoint;
    private Boolean reactiveCapabilityCurve;
    private Double minQ;
    private Double maxQ;
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;
}
