/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.TabularModification;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Limit sets tabular modification")
@JsonTypeName("LIMIT_SETS_TABULAR_MODIFICATION")
@ModificationErrorTypeName("LIMIT_SETS_TABULAR_MODIFICATION_ERROR")
public class LimitSetsTabularModificationInfos extends TabularModificationInfos{

    @Override
    public AbstractModification toModification() {
        return new TabularModification(this);
    }
}
