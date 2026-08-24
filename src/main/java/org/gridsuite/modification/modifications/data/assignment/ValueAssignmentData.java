/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.data.assignment;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.byfilter.assignment.*;

/**
 * @author Joris Mancini <joris.mancini at rte-france.com>
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "dataType",
    include = JsonTypeInfo.As.EXISTING_PROPERTY)
@JsonSubTypes({
    @JsonSubTypes.Type(value = BooleanAssignmentData.class, name = "BOOLEAN"),
    @JsonSubTypes.Type(value = EnumAssignmentData.class, name = "ENUM"),
    @JsonSubTypes.Type(value = DoubleAssignmentData.class, name = "DOUBLE"),
    @JsonSubTypes.Type(value = IntegerAssignmentData.class, name = "INTEGER"),
    @JsonSubTypes.Type(value = PropertyAssignmentData.class, name = "PROPERTY"),
    @JsonSubTypes.Type(value = StringAssignmentData.class, name = "STRING")
})
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ValueAssignmentData<T> extends AbstractAssignmentData {

    @Schema(description = "Value")
    private T value;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public DataType getDataType() {
        throw new UnsupportedOperationException("This method should not be called");
    }
}
