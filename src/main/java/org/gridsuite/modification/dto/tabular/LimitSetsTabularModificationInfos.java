/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
import org.gridsuite.modification.model.tabular.LimitSetsTabularModificationModel;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
@EqualsAndHashCode(callSuper = true)
@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Limit sets tabular modification")
@JsonTypeName("LIMIT_SETS_TABULAR_MODIFICATION")
@ModificationErrorTypeName("LIMIT_SETS_TABULAR_MODIFICATION_ERROR")
public class LimitSetsTabularModificationInfos extends AbstractTabularBaseInfos implements ModificationInfos {
    @Override
    public ModificationType getType() {
        return ModificationType.LIMIT_SETS_TABULAR_MODIFICATION;
    }

    @Override
    public ModificationModel toModel() {
        return LimitSetsTabularModificationModel.builder()
            .modifications(modifications.stream().map(ModificationInfos::toModel).toList())
            .properties(properties)
            .csvFilename(csvFilename)
            .modificationType(modificationType)
            .build();
    }
}
