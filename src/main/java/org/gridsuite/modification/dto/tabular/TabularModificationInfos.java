/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto.tabular;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.modifications.AbstractEquipmentBase;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.tabular.TabularModification;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@JsonTypeName("TABULAR_MODIFICATION")
@Schema(description = "Tabular modification")
public class TabularModificationInfos extends TabularBaseInfos {
    @Override
    public AbstractModification toModification(ModificationContext context) {
        return TabularModification.builder()
                .modificationType(getModificationType())
                .modifications(getModifications().stream()
                        .map(modificationInfos -> modificationInfos.toModification(context))
                        .map(m -> (AbstractEquipmentBase) m)
                        .toList())
                .build();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("tabularModificationType", getModificationType().name());
        return mapMessageValues;
    }
}
