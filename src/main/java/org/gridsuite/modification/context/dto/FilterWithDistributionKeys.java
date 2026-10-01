/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.context.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import org.gridsuite.filter.wip.Filter;

import java.util.Map;

/**
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
@Getter
@Builder
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Schema(description = "Filter with distribution keys")
public final class FilterWithDistributionKeys {

    @Schema(description = "Standalone filter")
    private Filter filter;

    @Schema(description = "Distribution keys")
    private Map<String, Double> distributionKeys;
}
