/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.config.PlatformConfig;
import com.powsybl.commons.extensions.Extension;
import com.powsybl.loadflow.LoadFlowParameters;
import com.powsybl.loadflow.LoadFlowProvider;
import org.gridsuite.modification.dto.LoadFlowParametersInfos;

import java.util.Map;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
public final class LoadFlowParametersUtils {

    private LoadFlowParametersUtils() {
        // Should not be instantiated
    }

    public static LoadFlowParameters mapLoadFlowPrameters(LoadFlowParametersInfos loadFlowParametersInfos) {
        LoadFlowParameters params = loadFlowParametersInfos.getCommonParameters();

        if (loadFlowParametersInfos.getSpecificParametersPerProvider() == null ||
                loadFlowParametersInfos.getSpecificParametersPerProvider().isEmpty()) {
            return params;
        }

        String provider = loadFlowParametersInfos.getProvider();
        Map<String, String> specificParameters = loadFlowParametersInfos.getSpecificParametersPerProvider().get(provider);
        LoadFlowProvider loadFlowProvider = LoadFlowProvider.findAll().stream()
                .filter(p -> p.getName().equals(provider))
                .findFirst()
                .orElseThrow(() -> new PowsyblException("LoadFlow provider " + provider + " not found"));

        Extension<LoadFlowParameters> specificParametersExtension = loadFlowProvider.loadSpecificParameters(PlatformConfig.defaultConfig())
                .orElseThrow(() -> new PowsyblException("Cannot add specific loadflow parameters with provider " + provider));
        params.addExtension((Class) specificParametersExtension.getClass(), specificParametersExtension);
        loadFlowProvider.updateSpecificParameters(specificParametersExtension, specificParameters);

        return params;
    }
}
