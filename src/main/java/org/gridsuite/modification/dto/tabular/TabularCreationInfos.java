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
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.tabular.TabularCreationModel;

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
@ModificationErrorTypeName("TABULAR_CREATION_ERROR")
public class TabularCreationInfos extends AbstractTabularBaseInfos implements ModificationInfos {
    @Override
    public ModificationType getType() {
        return ModificationType.TABULAR_CREATION;
    }

    @Override
    public ModificationModel toModel() {
        return TabularCreationModel.builder()
            .modifications(modifications.stream().map(ModificationInfos::toModel).toList())
            .properties(properties)
            .csvFilename(csvFilename)
            .modificationType(modificationType)
            .build();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("tabularCreationType", getModificationType().name());
        return mapMessageValues;
    }
}
