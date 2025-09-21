/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Connectable position modification")
public class MoveFeederBayInfos {
    @Schema(description = "Connectable id")
    private String equipmentId;

    @Schema(description = "busbar section id")
    private String busbarSectionId;

    @Schema(description = "connection side")
    private String connectionSide;

    @Schema(description = "connection position")
    private Integer connectionPosition;

    @Schema(description = "connection name")
    private String connectionName;

    @Schema(description = "connection direction")
    private ConnectablePosition.Direction connectionDirection;
}
