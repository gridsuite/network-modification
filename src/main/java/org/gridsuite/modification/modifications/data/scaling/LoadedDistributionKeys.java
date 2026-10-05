/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.modifications.data.scaling;

import java.util.Map;

/**
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
public record LoadedDistributionKeys(Map<String, Double> distributionKeys, DistributionKeyStatus status) {

    public Double getDistributionKey(String key) {
        return distributionKeys.get(key);
    }
}
