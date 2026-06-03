package org.gridsuite.modification.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;

import java.util.List;
import java.util.UUID;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString
@Schema(description = "Scaling creation")
public class ScalingVariationInfos {
    @Schema(description = "id")
    private UUID id;

    @Schema(description = "filters")
    private List<FilterInfos> filters;

    @Schema(description = "variation mode")
    private VariationMode variationMode;

    @Schema(description = "variation value")
    private Double variationValue;

    @Schema(description = "reactiveVariationMode")
    private ReactiveVariationMode reactiveVariationMode;

}
