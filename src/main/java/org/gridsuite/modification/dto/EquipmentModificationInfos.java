/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import org.springframework.lang.NonNull;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;
import java.util.Map;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Equipment modification")
public class EquipmentModificationInfos extends ModificationInfos {
    @Schema(description = "Equipment ID")
    @NonNull
    private String equipmentId;

    @Schema(description = "free properties")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<FreePropertyInfos> properties;

    @Override
    public Map<String, String> getMapMessageValues() {
        return Map.of("equipmentId", getEquipmentId());
    }
}
