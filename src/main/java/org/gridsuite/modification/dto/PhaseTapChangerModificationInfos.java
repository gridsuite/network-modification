/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.powsybl.iidm.network.PhaseTapChanger;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "PhaseTapChanger attributes")
public class PhaseTapChangerModificationInfos extends TapChangerModificationInfos {

    @Schema(description = "regulationMode")
    private AttributeModification<PhaseTapChanger.RegulationMode> regulationMode;

    @Schema(description = "regulationValue")
    private AttributeModification<Double> regulationValue;
}
