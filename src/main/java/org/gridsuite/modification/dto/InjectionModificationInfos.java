/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import io.swagger.v3.oas.annotations.media.Schema;
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
@Schema(description = "Injection modification")
public class InjectionModificationInfos extends BasicEquipmentModificationInfos {
    @Schema(description = "Voltage level id modification")
    private AttributeModification<String> voltageLevelId;

    @Schema(description = "Bus id modification")
    private AttributeModification<String> busOrBusbarSectionId;

    @Schema(description = "Connection Name")
    private AttributeModification<String> connectionName;

    @Schema(description = "Connection Direction")
    private AttributeModification<ConnectablePosition.Direction> connectionDirection;

    @Schema(description = "Connection Position")
    private AttributeModification<Integer> connectionPosition;

    @Schema(description = "Connected")
    private AttributeModification<Boolean> terminalConnected;

    @JsonProperty("pMeasurementValue")
    @Schema(description = "P measurement value")
    private AttributeModification<Double> pMeasurementValue;

    @JsonProperty("pMeasurementValidity")
    @Schema(description = "P measurement validity")
    private AttributeModification<Boolean> pMeasurementValidity;

    @JsonProperty("qMeasurementValue")
    @Schema(description = "Q measurement value")
    private AttributeModification<Double> qMeasurementValue;

    @JsonProperty("qMeasurementValidity")
    @Schema(description = "Q measurement validity")
    private AttributeModification<Boolean> qMeasurementValidity;
}
