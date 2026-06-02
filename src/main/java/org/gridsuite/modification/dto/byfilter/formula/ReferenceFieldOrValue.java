/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.formula;

import lombok.*;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class ReferenceFieldOrValue {
    private String equipmentField;

    private Double value;
}
