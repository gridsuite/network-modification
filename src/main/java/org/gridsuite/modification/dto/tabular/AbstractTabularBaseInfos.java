/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto.tabular;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.model.tabular.TabularPropertyModel;
import org.springframework.lang.NonNull;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

/**
 * @author David Braquart <david.braquart_externe at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode
@Data
@Schema(description = "Tabular abstract modification")
public abstract class AbstractTabularBaseInfos implements ModificationInfos {
    @Schema(description = "Modification id")
    protected UUID uuid;

    @Schema(description = "Modification date")
    protected Instant date;

    @Schema(description = "Modification flag")
    @Builder.Default
    protected Boolean stashed = false;

    @Schema(description = "Message type")
    protected String messageType;

    @Schema(description = "Message values")
    protected String messageValues;

    @Schema(description = "Modification activated (defaults to true at creation when not provided)")
    protected Boolean activated;

    @Schema(description = "User description")
    protected String description;

    @Schema(description = "additional properties")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    protected List<TabularPropertyModel> properties;

    @Schema(description = "csv file name")
    protected String csvFilename;

    @Schema(description = "Modification type")
    @NonNull
    protected ModificationType modificationType;

    @Schema(description = "modifications")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    protected List<ModificationInfos> modifications;
}
