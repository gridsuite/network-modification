/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.data;

import lombok.*;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.VoltageRegulationType;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VoltageRegulationModification {
    AttributeModification<Double> targetV;
    AttributeModification<Boolean> voltageRegulationOn;
    AttributeModification<String> regulatingTerminalId;
    AttributeModification<String> regulatingTerminalType;
    AttributeModification<String> regulatingTerminalVlId;
    AttributeModification<VoltageRegulationType> voltageRegulationType;
}
