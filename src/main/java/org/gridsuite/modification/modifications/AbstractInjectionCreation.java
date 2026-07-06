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

import java.util.List;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Setter
@Getter
public abstract class AbstractInjectionCreation extends AbstractEquipmentCreation implements InjectionCreation {

    protected String voltageLevelId;
    protected String busOrBusbarSectionId;
    protected String connectionName;
    protected ConnectablePosition.Direction connectionDirection;
    protected Integer connectionPosition;
    protected boolean terminalConnected;

    protected AbstractInjectionCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName,
                                     String voltageLevelId, String busOrBusbarSectionId, String connectionName,
                                     ConnectablePosition.Direction connectionDirection, Integer connectionPosition,
                                     boolean terminalConnected) {
        super(equipmentId, properties, equipmentName);
        this.voltageLevelId = voltageLevelId;
        this.busOrBusbarSectionId = busOrBusbarSectionId;
        this.connectionName = connectionName;
        this.connectionDirection = connectionDirection;
        this.connectionPosition = connectionPosition;
        this.terminalConnected = terminalConnected;
    }
}
