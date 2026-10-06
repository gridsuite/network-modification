/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.modifications.data.scaling;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import org.gridsuite.filter.wip.Filter;

import java.util.Map;

/**
 * A filter of a {@link ScalingVariationData}, with the distribution keys of the equipments it may select.
 *
 * <p>{@code filter} is {@code null} when the reference could not be resolved at build time.
 *
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@Schema(description = "Filter with the distribution keys of the equipments it may select")
public record VariationFilterData(Filter filter, Map<String, Double> distributionKeys) {

    public VariationFilterData {
        distributionKeys = distributionKeys == null ? Map.of() : distributionKeys;
    }

    @JsonIgnore
    public boolean isResolved() {
        return filter != null;
    }
}
