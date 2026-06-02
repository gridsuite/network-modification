/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
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
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
public class BranchCreationModel extends EquipmentCreationModel {

    private double r;

    private double x;

    private String voltageLevelId1;

    private String voltageLevelId2;

    private String busOrBusbarSectionId1;

    private String busOrBusbarSectionId2;

    private List<OperationalLimitsGroupModel> operationalLimitsGroups;

    private String selectedOperationalLimitsGroupId1;

    private String selectedOperationalLimitsGroupId2;

    private String connectionName1;

    private ConnectablePosition.Direction connectionDirection1;

    private String connectionName2;

    private ConnectablePosition.Direction connectionDirection2;

    private Integer connectionPosition1;

    private Integer connectionPosition2;

    private boolean connected1;

    private boolean connected2;
}
