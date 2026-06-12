/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.model.tabular.TabularModificationModel;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@EqualsAndHashCode(callSuper = true)
@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Tabular modification")
@JsonTypeName("TABULAR_MODIFICATION")
@ModificationErrorTypeName("TABULAR_MODIFICATION_ERROR")
public class TabularModificationInfos extends AbstractTabularBaseInfos implements ModificationInfos {
    @Override
    public ModificationType getType() {
        return ModificationType.TABULAR_MODIFICATION;
    }

    @Override
    public ModificationModel toModel() {
        return TabularModificationModel.builder()
            .modifications(modifications.stream().map(ModificationInfos::toModel).toList())
            .properties(properties)
            .csvFilename(csvFilename)
            .modificationType(modificationType)
            .build();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("tabularModificationType", getModificationType().name());
        return mapMessageValues;
    }
}
