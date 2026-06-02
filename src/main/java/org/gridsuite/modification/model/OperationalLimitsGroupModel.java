/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString
public class OperationalLimitsGroupModel {
    private String id;

    private CurrentLimitsModel currentLimits;

    private Applicability applicability;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<LimitsPropertyModel> limitsProperties;

    public enum Applicability {
        EQUIPMENT, // SIDE1 + SIDE2
        SIDE1,
        SIDE2,
    }
}
