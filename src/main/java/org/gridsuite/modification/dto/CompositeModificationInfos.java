/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.model.CompositeModificationModel;
import org.gridsuite.modification.model.ModificationModel;

import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Composite modification")
@JsonTypeName("COMPOSITE_MODIFICATION")
@ModificationErrorTypeName("COMPOSITE_MODIFICATION_ERROR")
public class CompositeModificationInfos implements ModificationInfos {
    @Schema(description = "Modification id")
    private UUID uuid;

    @Schema(description = "Modification date")
    private Instant date;

    @Schema(description = "Modification flag")
    @Builder.Default
    private Boolean stashed = false;

    @Schema(description = "Modification activated (defaults to true at creation when not provided)")
    private Boolean activated;

    @Schema(description = "User description")
    private String description;

    @Schema(description = "composite modification name")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String name;

    @Schema(description = "composite modification list")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ModificationInfos> modificationsInfos;

    // While composite submodifications are lazy loaded we need an indicator to know if we allow depth sensitive operation
    // added only to the DTO so it can be computed while retrieving composite metadata at runtime
    @Schema(description = "composite modification max depth")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Integer maxDepth;

    @Override
    public ModificationType getType() {
        return ModificationType.COMPOSITE_MODIFICATION;
    }

    @Override
    public ModificationModel toModel() {
        return CompositeModificationModel.builder()
            .name(name)
            .modificationsInfos(modificationsInfos.stream().map(ModificationInfos::toModel).toList())
            .maxDepth(maxDepth)
            .build();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("name", getName());
        return mapMessageValues;
    }
}
