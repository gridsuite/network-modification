/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.Setter;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.OperationalLimitsGroupInfos;

import java.util.List;

/**
 * @author Joris MANCINI <joris.mancini_externe at rte-france.com>
 */
@Setter
@Getter
public abstract class AbstractBranchCreation extends AbstractEquipmentCreation {

    protected double r;
    protected double x;
    protected String voltageLevelId1;
    protected String voltageLevelId2;
    protected String busOrBusbarSectionId1;
    protected String busOrBusbarSectionId2;
    protected List<OperationalLimitsGroupInfos> operationalLimitsGroups;
    protected String selectedOperationalLimitsGroupId1;
    protected String selectedOperationalLimitsGroupId2;
    protected String connectionName1;
    protected ConnectablePosition.Direction connectionDirection1;
    protected String connectionName2;
    protected ConnectablePosition.Direction connectionDirection2;
    protected Integer connectionPosition1;
    protected Integer connectionPosition2;
    protected boolean connected1;
    protected boolean connected2;

    public AbstractBranchCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName,
                                  String busOrBusbarSectionId2, double r, double x, String voltageLevelId1,
                                  String voltageLevelId2, String busOrBusbarSectionId1,
                                  List<OperationalLimitsGroupInfos> operationalLimitsGroups,
                                  String selectedOperationalLimitsGroupId1, String selectedOperationalLimitsGroupId2,
                                  String connectionName1, ConnectablePosition.Direction connectionDirection1,
                                  String connectionName2, ConnectablePosition.Direction connectionDirection2,
                                  Integer connectionPosition1, Integer connectionPosition2, boolean connected1, boolean connected2) {
        super(equipmentId, properties, equipmentName);
        this.busOrBusbarSectionId2 = busOrBusbarSectionId2;
        this.r = r;
        this.x = x;
        this.voltageLevelId1 = voltageLevelId1;
        this.voltageLevelId2 = voltageLevelId2;
        this.busOrBusbarSectionId1 = busOrBusbarSectionId1;
        this.operationalLimitsGroups = operationalLimitsGroups;
        this.selectedOperationalLimitsGroupId1 = selectedOperationalLimitsGroupId1;
        this.selectedOperationalLimitsGroupId2 = selectedOperationalLimitsGroupId2;
        this.connectionName1 = connectionName1;
        this.connectionDirection1 = connectionDirection1;
        this.connectionName2 = connectionName2;
        this.connectionDirection2 = connectionDirection2;
        this.connectionPosition1 = connectionPosition1;
        this.connectionPosition2 = connectionPosition2;
        this.connected1 = connected1;
        this.connected2 = connected2;
    }
}
