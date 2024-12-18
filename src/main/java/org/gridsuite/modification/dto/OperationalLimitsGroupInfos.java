package org.gridsuite.modification.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString
@Schema(description = "Operational limits group")
public class OperationalLimitsGroupInfos {
    @Schema(description = "Operational limit group id")
    private String id;

    @Schema(description = "Current limits")
    private CurrentLimitsInfos currentLimits;
}
