/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification;

import org.gridsuite.modification.dto.LoadFlowParametersInfos;

import java.util.UUID;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
public interface ILoadFlowService {
    LoadFlowParametersInfos getLoadFlowParametersInfos(UUID loadFlowParametersUuid);
}
