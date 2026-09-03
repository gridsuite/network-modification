/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto.tabular;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.modifications.AbstractEquipmentBase;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.tabular.TabularCreation;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@EqualsAndHashCode(callSuper = true)
@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Tabular creation")
@JsonTypeName("TABULAR_CREATION")
public class TabularCreationInfos extends TabularBaseInfos {

    @Override
    public AbstractModification toModification() {
        return TabularCreation.builder()
                .modificationType(getModificationType())
                .modifications(getModifications().stream()
                    .map(ModificationInfos::toModification)
                    .map(m -> (AbstractEquipmentBase) m)
                    .toList())
                .build();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("tabularCreationType", getModificationType().name());
        return mapMessageValues;
    }
}
