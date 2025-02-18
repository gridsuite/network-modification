/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;

@Getter
public final class FeederData {
    private final String connectionName;
    private final Integer order;
    private final ConnectablePosition.Direction direction;

    public FeederData(String connectionName, Integer order, ConnectablePosition.Direction direction) {
        this.connectionName = connectionName;
        this.order = order;
        this.direction = direction;
    }

    public String getConnectionName() {
        return connectionName;
    }

    public Integer getOrder() {
        return order;
    }

    public ConnectablePosition.Direction getDirection() {
        return direction;
    }
}
