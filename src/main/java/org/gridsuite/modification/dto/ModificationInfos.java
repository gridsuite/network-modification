/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import java.time.Instant;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public interface ModificationInfos {
    UUID getUuid();

    void setUuid(UUID uuid);

    Instant getDate();

    void setDate(Instant date);

    Boolean getStashed();

    void setStashed(Boolean stashed);

    String getMessageType();

    void setMessageType(String messageType);

    String getMessageValues();

    void setMessageValues(String messageValues);

    Boolean getActivated();

    void setActivated(Boolean activated);

    String getDescription();

    void setDescription(String description);
}
