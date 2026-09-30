/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.data.assignment;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.filter.wip.Filter;

import java.util.List;

/**
 * @author Joris MANCINI <joris.mancini at rte-france.com>
 */
@Getter
@Setter
@SuperBuilder
@EqualsAndHashCode
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public abstract class AbstractAssignmentData {

    @Schema(description = "List of filters")
    private List<Filter> filters;

    @Schema(description = "Edited field")
    private String editedField;

    @JsonIgnore
    public String getEditedFieldLabel() {
        return editedField;
    }
}
