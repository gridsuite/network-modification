/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
public class TapChangerModificationModel {

    private AttributeModification<Boolean> enabled;

    private AttributeModification<VoltageRegulationType> regulationType;

    private AttributeModification<RegulationSide> regulationSide;

    private AttributeModification<Integer> lowTapPosition;

    private AttributeModification<Integer> tapPosition;

    @JsonProperty("isRegulating")
    private AttributeModification<Boolean> regulating;

    private AttributeModification<Double> targetDeadband;

    private AttributeModification<String> terminalRefConnectableId;

    private AttributeModification<String> terminalRefConnectableType;

    private AttributeModification<String> terminalRefConnectableVlId;

    private List<TapChangerStepCreationModel> steps;

    @JsonProperty("hasLoadTapChangingCapabilities")
    private AttributeModification<Boolean> loadTapChangingCapabilities;
}
