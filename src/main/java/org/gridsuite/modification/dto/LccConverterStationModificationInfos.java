/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.model.LccConverterStationModificationModel;
import java.time.Instant;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Lcc converter station modification")
@JsonTypeName("LCC_CONVERTER_STATION_MODIFICATION")
@ModificationErrorTypeName("LCC_MODIFY_CONVERTER_STATION_ERROR")
public class LccConverterStationModificationInfos extends LccConverterStationModificationModel implements ModificationInfos {
    @Schema(description = "Modification id")
    private UUID uuid;

    @Schema(description = "Modification type")
    @Setter(AccessLevel.NONE)
    private final AtomicReference<ModificationType> type = new AtomicReference<>(null); // Only accessor (automatically initialized)

    @Schema(description = "Modification date")
    private Instant date;

    @Schema(description = "Modification flag")
    @Builder.Default
    private Boolean stashed = false;

    @Schema(description = "Message type")
    private String messageType;

    @Schema(description = "Message values")
    private String messageValues;

    @Schema(description = "Modification activated (defaults to true at creation when not provided)")
    private Boolean activated;

    @Schema(description = "User description")
    private String description;
}
