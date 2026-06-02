/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
public class OperationalLimitsGroupModificationModel {
    private String id;

    CurrentLimitsModificationModel currentLimits;

    private OperationalLimitsGroupModificationType modificationType;

    private TemporaryLimitModificationType temporaryLimitsModificationType;

    private List<LimitsPropertyModel> limitsProperties;

    private OperationalLimitsGroupModel.Applicability applicability;
}
