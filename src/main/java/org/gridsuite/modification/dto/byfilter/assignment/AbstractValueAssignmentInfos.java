/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter.assignment;

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
import org.gridsuite.modification.dto.byfilter.AbstractAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.AssignmentType;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@JsonTypeInfo(
        use = JsonTypeInfo.Id.NAME,
        include = JsonTypeInfo.As.EXISTING_PROPERTY,
        property = "dataType"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = BooleanAssignmentInfos.class, name = "BOOLEAN"),
    @JsonSubTypes.Type(value = EnumAssignmentInfos.class, name = "ENUM"),
    @JsonSubTypes.Type(value = DoubleAssignmentInfos.class, name = "DOUBLE"),
    @JsonSubTypes.Type(value = IntegerAssignmentInfos.class, name = "INTEGER"),
    @JsonSubTypes.Type(value = PropertyAssignmentInfos.class, name = "PROPERTY"),
    @JsonSubTypes.Type(value = StringAssignmentInfos.class, name = "STRING")
})
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public abstract class AbstractValueAssignmentInfos<T> extends AbstractAssignmentInfos {

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public final AssignmentType getAssignmentType() {
        return AssignmentType.VALUE;
    }

    @Schema(description = "Value")
    private T value;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public abstract DataType getDataType();
}
