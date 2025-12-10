/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 */
public class VoltageLevelCreation extends AbstractModification {

    private final VoltageLevelCreationInfos modificationInfos;

    public VoltageLevelCreation(VoltageLevelCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) {
        String errorMessage = "Voltage level '" + modificationInfos.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().controlVoltageLevelCreation(modificationInfos, network);
        checkIsNotNegativeValue(errorMessage, modificationInfos.getNominalV(), "Nominal Voltage");
        checkIsNotNegativeValue(errorMessage, modificationInfos.getLowVoltageLimit(), "Low voltage limit");
        checkIsNotNegativeValue(errorMessage, modificationInfos.getHighVoltageLimit(), "High voltage limit");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ModificationUtils.getInstance().createVoltageLevel(modificationInfos, subReportNode, network);
        // properties
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getEquipmentId());
        PropertiesUtils.applyProperties(voltageLevel, subReportNode, modificationInfos.getProperties(), "network.modification.VlProperties");
    }

    @Override
    public String getName() {
        return "VoltageLevelCreation";
    }
}
