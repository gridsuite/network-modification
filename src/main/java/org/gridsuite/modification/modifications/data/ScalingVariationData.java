/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */

package org.gridsuite.modification.modifications.data;

import lombok.*;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;

import java.util.List;
import java.util.Map;

/**
 * @author Kamil MARUT {@literal <kamil.marut at rte-france.com>}
 */
@Getter
@Builder
@EqualsAndHashCode
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ScalingVariationData {

    private List<Filter> filters;

    private Map<String, Double> distributionKeysPerEquipmentId;

    private VariationMode variationMode;

    private Double variationValue;

    private ReactiveVariationMode reactiveVariationMode;
}
