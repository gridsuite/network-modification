package org.gridsuite.modification.dto;

import java.time.Instant;
import java.util.UUID;

public interface ModificationDto {

    UUID getUuid();

    Instant getDate();

    boolean getStashed();
}
