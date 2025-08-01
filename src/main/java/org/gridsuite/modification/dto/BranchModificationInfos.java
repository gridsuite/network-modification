/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Branch creation")
public class BranchModificationInfos extends BasicEquipmentModificationInfos {

    @Schema(description = "Series resistance")
    private AttributeModification<Double> r;

    @Schema(description = "Series reactance")
    private AttributeModification<Double> x;

    @Schema(description = "Current limits Side 1")
    private CurrentLimitsModificationInfos currentLimits1;

    @Schema(description = "Current limits Side 2")
    private CurrentLimitsModificationInfos currentLimits2;

    @Schema(description = "Operational limits group Side 1")
    private List<OperationalLimitsGroupModificationInfos> operationalLimitsGroup1;

    @Schema(description = "Operational limits group Side 2")
    private List<OperationalLimitsGroupModificationInfos> operationalLimitsGroup2;

    @Schema(description = "Voltage level id modification 1")
    private AttributeModification<String> voltageLevelId1;

    @Schema(description = "Voltage level id modification 2")
    private AttributeModification<String> voltageLevelId2;

    @Schema(description = "Bus id modification 1")
    private AttributeModification<String> busOrBusbarSectionId1;

    @Schema(description = "Bus id modification 2")
    private AttributeModification<String> busOrBusbarSectionId2;

    @Schema(description = "Connection Name 1")
    private AttributeModification<String> connectionName1;

    @Schema(description = "Connection Name 2")
    private AttributeModification<String> connectionName2;

    @Schema(description = "Connection Direction 1")
    private AttributeModification<ConnectablePosition.Direction> connectionDirection1;

    @Schema(description = "Connection Direction 2")
    private AttributeModification<ConnectablePosition.Direction> connectionDirection2;

    @Schema(description = "Connection Position 1")
    private AttributeModification<Integer> connectionPosition1;

    @Schema(description = "Connection Position 2")
    private AttributeModification<Integer> connectionPosition2;

    @Schema(description = "Connected 1")
    private AttributeModification<Boolean> terminal1Connected;

    @Schema(description = "Connected 2")
    private AttributeModification<Boolean> terminal2Connected;

    @Schema(description = "P1 measurement value")
    private AttributeModification<Double> p1MeasurementValue;

    @Schema(description = "P1 measurement validity")
    private AttributeModification<Boolean> p1MeasurementValidity;

    @Schema(description = "P2 measurement value")
    private AttributeModification<Double> p2MeasurementValue;

    @Schema(description = "P2 measurement validity")
    private AttributeModification<Boolean> p2MeasurementValidity;

    @Schema(description = "Q1 measurement value")
    private AttributeModification<Double> q1MeasurementValue;

    @Schema(description = "Q1 measurement validity")
    private AttributeModification<Boolean> q1MeasurementValidity;

    @Schema(description = "Q2 measurement value")
    private AttributeModification<Double> q2MeasurementValue;

    @Schema(description = "Q2 measurement validity")
    private AttributeModification<Boolean> q2MeasurementValidity;
}
