/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.ModificationModel;

import java.time.Instant;
import java.util.UUID;

@AllArgsConstructor
@Builder
@Getter
@JsonInclude(JsonInclude.Include.NON_NULL)
@NoArgsConstructor
@Setter
@JsonTypeName("MODIFICATION_METADATA")
public class ModificationMetadataInfos implements ModificationInfos {

    private UUID uuid;
    private Instant date;
    private Boolean stashed;
    @JsonProperty(access = JsonProperty.Access.READ_WRITE)
    private String messageType;
    @JsonProperty(access = JsonProperty.Access.READ_WRITE)
    private String messageValues;
    private Integer maxDepth;
    private Boolean activated;
    private String description;

    @Override
    public ModificationType getType() {
        return ModificationType.MODIFICATION_METADATA;
    }

    @Override
    public ModificationModel toModel() {
        throw new UnsupportedOperationException("ModificationMetadataInfos cannot be converted to a ModificationModel");
    }
}
