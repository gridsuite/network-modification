/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.assignment;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.byfilter.AbstractAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.DataType;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class AssignmentInfos<T> extends AbstractAssignmentInfos {
    @Schema(description = "Value")
    private T value;

    public DataType getDataType() {
        throw new UnsupportedOperationException("This method should not be called");
    }

}
