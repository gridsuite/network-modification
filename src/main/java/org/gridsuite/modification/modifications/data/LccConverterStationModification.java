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
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.LccShuntCompensatorModificationInfos;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Getter
@Setter
@SuperBuilder
public class LccConverterStationModification extends AbstractInjectionModification {

    private AttributeModification<Float> lossFactor;
    private AttributeModification<Float> powerFactor;
    private List<LccShuntCompensatorModificationInfos> shuntCompensatorsOnSide;

    public boolean hasModifications() {
        return getEquipmentName() != null || lossFactor != null || powerFactor != null || !CollectionUtils.isEmpty(shuntCompensatorsOnSide);
    }
}
