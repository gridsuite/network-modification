/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "type"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = LccShuntCompensatorModificationinfos.class, name = "LCC_SHUNT_MODIFICATION")
})

@SuperBuilder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class LccShuntCompensatorInfos {
    private String id;
    private String name;
    private Double maxQAtNominalV;
    private Boolean connectedToHvdc;
}
