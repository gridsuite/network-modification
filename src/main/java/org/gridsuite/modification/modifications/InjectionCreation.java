/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.dto.FreePropertyInfos;

import java.util.List;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public interface InjectionCreation {

    String getEquipmentId();

    List<FreePropertyInfos> getProperties();

    String getEquipmentName();

    String getVoltageLevelId();

    String getBusOrBusbarSectionId();

    String getConnectionName();

    ConnectablePosition.Direction getConnectionDirection();

    Integer getConnectionPosition();

    boolean isTerminalConnected();
}
