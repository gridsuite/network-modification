/**
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.powsybl.loadflow.LoadFlowParameters;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
@Getter
@AllArgsConstructor
@Builder
@NoArgsConstructor
public class LoadFlowParametersInfos {

    private String provider;

    private Float limitReduction;

    private LoadFlowParameters commonParameters;

    private Map<String, Map<String, String>> specificParametersPerProvider;

    private List<LimitReductionsByVoltageLevel> limitReductions;


}
