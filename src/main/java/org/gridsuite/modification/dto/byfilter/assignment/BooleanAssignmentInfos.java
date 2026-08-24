/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.assignment;

import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.filter.wip.FilterLoader;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.modifications.data.assignment.BooleanAssignmentData;
import org.gridsuite.modification.modifications.data.assignment.DataType;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class BooleanAssignmentInfos extends AssignmentInfos<Boolean> {

    @Override
    public DataType getDataType() {
        return DataType.BOOLEAN;
    }

    @Override
    public BooleanAssignmentData toData(FilterLoader filterLoader) {
        return BooleanAssignmentData.builder()
                .editedField(getEditedField())
                .value(getValue())
                .filters(filterLoader.getNewFilters(getFilters().stream().map(FilterInfos::getId).toList()))
                .build();
    }
}
