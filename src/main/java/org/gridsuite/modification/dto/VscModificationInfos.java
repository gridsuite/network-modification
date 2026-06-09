package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.model.VscModificationModel;
import java.time.Instant;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "VSC modification")
@JsonTypeName("VSC_MODIFICATION")
@ModificationErrorTypeName("MODIFY_VSC_ERROR")
public class VscModificationInfos extends VscModificationModel implements ModificationInfos {
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
