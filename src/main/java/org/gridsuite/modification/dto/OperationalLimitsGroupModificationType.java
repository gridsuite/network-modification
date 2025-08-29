/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */

public enum OperationalLimitsGroupModificationType {
    ADD,
    MODIFY,
    MODIFY_OR_ADD, // if the opLG exists it is modified, if not it is created
    REPLACE,
    DELETE,
}
