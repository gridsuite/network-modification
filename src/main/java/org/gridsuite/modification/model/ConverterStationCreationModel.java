/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
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
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("CONVERTER_STATION_CREATION")
@ModificationErrorTypeName("CREATE_CONVERTER_STATION_ERROR")
public class ConverterStationCreationModel extends InjectionCreationModel implements ReactiveLimitsHolderModel {
    private Float lossFactor;

    private Double reactivePowerSetpoint;

    private Boolean voltageRegulationOn;

    private Double voltageSetpoint;

    private Boolean reactiveCapabilityCurve;

    private Double minQ;

    private Double maxQ;

    private List<ReactiveCapabilityCurvePointsModel> reactiveCapabilityCurvePoints;

}
