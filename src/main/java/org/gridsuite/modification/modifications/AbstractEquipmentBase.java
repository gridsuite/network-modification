/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.dto.FreePropertyInfos;

import java.util.List;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Setter
@Getter
@NoArgsConstructor
public abstract class AbstractEquipmentBase extends AbstractModification {

    protected String equipmentId;
    protected List<FreePropertyInfos> properties;

    protected AbstractEquipmentBase(String equipmentId, List<FreePropertyInfos> properties) {
        this.equipmentId = equipmentId;
        this.properties = properties;
    }
}
