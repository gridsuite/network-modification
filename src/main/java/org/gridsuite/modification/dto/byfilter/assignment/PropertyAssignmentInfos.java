/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.assignment;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class PropertyAssignmentInfos extends AbstractValueAssignmentInfos<String> {

    @Schema(description = "Property name")
    @Getter
    private String propertyName;

    @Override
    public DataType getDataType() {
        return DataType.PROPERTY;
    }

    @JsonIgnore
    @Override
    public String getEditedFieldLabel() {
        return propertyName + " " + super.getEditedFieldLabel();
    }

}
