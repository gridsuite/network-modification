package org.gridsuite.modification.dto;

import org.gridsuite.modification.ModificationType;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;

public interface ModificationInfos {

    UUID getUuid();

    Instant getDate();

    Boolean getActivated();

    Boolean getStashed();

    ModificationType getType();

    String getDescription();

    Map<String, String> getMapMessageValues();

    default String getMessageType() {
        return getType().name();
    }

    default String getMessageValues() {
        return getMapMessageValues().toString();
    }
}
