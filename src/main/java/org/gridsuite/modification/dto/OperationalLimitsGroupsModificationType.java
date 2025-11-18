/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 *
 * Determines how all the operational limits groups will be modified as a group
 */

public enum OperationalLimitsGroupsModificationType {
    // Modification types for Tabular modifications :
    MODIFY, // standard mode : the olg modifications are applied. The unspecified olg are not changed at all
    // Modification type for simple form modifications :
    REPLACE, // All the olg are removed, then the olg modification/add etc are applied
}
