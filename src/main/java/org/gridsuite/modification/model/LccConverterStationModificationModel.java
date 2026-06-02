/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;

import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("LCC_CONVERTER_STATION_MODIFICATION")
@ModificationErrorTypeName("LCC_MODIFY_CONVERTER_STATION_ERROR")
public class LccConverterStationModificationModel extends InjectionModificationModel {
    private AttributeModification<Float> lossFactor;

    private AttributeModification<Float> powerFactor;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<LccShuntCompensatorModificationModel> shuntCompensatorsOnSide;

    public boolean hasModifications() {
        return getEquipmentName() != null || lossFactor != null || powerFactor != null || shuntCompensatorsOnSide != null && !shuntCompensatorsOnSide.isEmpty();
    }
}
