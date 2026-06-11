/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
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
public class ModificationMetadataInfos implements ModificationInfos {

    private UUID uuid;
    private ModificationType type;
    private Instant date;
    private Boolean stashed;
    private String messageType;
    private String messageValues;
    private Boolean activated;
    private String description;

    @Override
    public ModificationModel toModel() {
        ModificationModel model = new ModificationModel();
        model.setType(type);
        return model;
    }
}
