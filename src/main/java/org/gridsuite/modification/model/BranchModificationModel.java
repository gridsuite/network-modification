/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
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
public class BranchModificationModel extends BasicEquipmentModificationModel {

    private AttributeModification<Double> r;

    private AttributeModification<Double> x;

    private OperationalLimitsGroupsModificationType operationalLimitsGroupsModificationType;

    private Boolean enableOLGModification;

    private List<OperationalLimitsGroupModificationModel> operationalLimitsGroups;

    private AttributeModification<String> selectedOperationalLimitsGroupId1;

    private AttributeModification<String> selectedOperationalLimitsGroupId2;

    private AttributeModification<String> voltageLevelId1;

    private AttributeModification<String> voltageLevelId2;

    private AttributeModification<String> busOrBusbarSectionId1;

    private AttributeModification<String> busOrBusbarSectionId2;

    private AttributeModification<String> connectionName1;

    private AttributeModification<String> connectionName2;

    private AttributeModification<ConnectablePosition.Direction> connectionDirection1;

    private AttributeModification<ConnectablePosition.Direction> connectionDirection2;

    private AttributeModification<Integer> connectionPosition1;

    private AttributeModification<Integer> connectionPosition2;

    private AttributeModification<Boolean> terminal1Connected;

    private AttributeModification<Boolean> terminal2Connected;

    private AttributeModification<Double> p1MeasurementValue;

    private AttributeModification<Boolean> p1MeasurementValidity;

    private AttributeModification<Double> p2MeasurementValue;

    private AttributeModification<Boolean> p2MeasurementValidity;

    private AttributeModification<Double> q1MeasurementValue;

    private AttributeModification<Boolean> q1MeasurementValidity;

    private AttributeModification<Double> q2MeasurementValue;

    private AttributeModification<Boolean> q2MeasurementValidity;
}
