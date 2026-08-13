/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto.byfilter;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.byfilter.assignment.*;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;

import java.util.List;
import java.util.UUID;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@JsonTypeInfo(
        use = JsonTypeInfo.Id.NAME,
        property = "dataType",
        include = JsonTypeInfo.As.EXISTING_PROPERTY)
@JsonSubTypes({
    @JsonSubTypes.Type(value = BooleanAssignmentInfos.class, name = "BOOLEAN"),
    @JsonSubTypes.Type(value = EnumAssignmentInfos.class, name = "ENUM"),
    @JsonSubTypes.Type(value = DoubleAssignmentInfos.class, name = "DOUBLE"),
    @JsonSubTypes.Type(value = IntegerAssignmentInfos.class, name = "INTEGER"),
    @JsonSubTypes.Type(value = PropertyAssignmentInfos.class, name = "PROPERTY"),
    @JsonSubTypes.Type(value = StringAssignmentInfos.class, name = "STRING"),
    @JsonSubTypes.Type(value = FormulaInfos.class, name = "FORMULA")
})
@JsonInclude(JsonInclude.Include.NON_NULL)
@Getter
@Setter
@SuperBuilder
@EqualsAndHashCode
@NoArgsConstructor
public abstract class AbstractAssignmentInfos {
    @Schema(description = "id")
    private UUID id;

    @Schema(description = "List of filters")
    private List<FilterInfos> filters;

    @Schema(description = "Edited field")
    private String editedField;

    @JsonIgnore
    public String getEditedFieldLabel() {
        return editedField;
    }
}
