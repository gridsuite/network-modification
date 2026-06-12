/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.model.LineSplitWithVoltageLevelModel;
import java.time.Instant;
import java.util.Map;
import java.util.UUID;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Line split with voltage level")
@JsonTypeName("LINE_SPLIT_WITH_VOLTAGE_LEVEL")
@ModificationErrorTypeName("LINE_SPLIT_ERROR")
public class LineSplitWithVoltageLevelInfos extends LineSplitWithVoltageLevelModel implements ModificationInfos {
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

    @Override
    public Map<String, String> getMapMessageValues() {
        return Map.of("lineToSplitId", getLineToSplitId());
    }
}
