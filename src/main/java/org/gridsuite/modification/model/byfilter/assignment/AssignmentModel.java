 /**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.model.byfilter.assignment;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.byfilter.AbstractAssignmentModel;
import org.gridsuite.modification.model.byfilter.DataType;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "dataType",
    include = JsonTypeInfo.As.EXISTING_PROPERTY)
@JsonSubTypes({
    @JsonSubTypes.Type(value = BooleanAssignmentModel.class, name = "BOOLEAN"),
    @JsonSubTypes.Type(value = EnumAssignmentModel.class, name = "ENUM"),
    @JsonSubTypes.Type(value = DoubleAssignmentModel.class, name = "DOUBLE"),
    @JsonSubTypes.Type(value = IntegerAssignmentModel.class, name = "INTEGER"),
    @JsonSubTypes.Type(value = PropertyAssignmentModel.class, name = "PROPERTY"),
    @JsonSubTypes.Type(value = StringAssignmentModel.class, name = "STRING"),
})
@JsonInclude(JsonInclude.Include.NON_NULL)
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public class AssignmentModel<T> extends AbstractAssignmentModel {
    @Schema(description = "Value")
    private T value;

    public DataType getDataType() {
        throw new UnsupportedOperationException("This method should not be called");
    }
}
