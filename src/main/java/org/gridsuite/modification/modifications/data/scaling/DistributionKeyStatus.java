/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.modifications.data.scaling;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
@Getter
@AllArgsConstructor
public enum DistributionKeyStatus {
    MISSING_FILTER("network.modification.distributionKeys.missingFilter"),
    FILTER_HAS_NO_KEYS("network.modification.distributionKeys.filterHasNoKeys"),
    MISSING_EQUIPMENT_KEY("network.modification.distributionKeys.missingEquipmentKey"),
    DUPLICATED_EQUIPMENT_KEY("network.modification.distributionKeys.duplicatedKey"),
    VALID_KEYS("network.modification.distributionKeys.valid");

    private final String reportKey;
}
