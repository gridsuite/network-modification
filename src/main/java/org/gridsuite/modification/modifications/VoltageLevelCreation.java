/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.VoltageLevelCreationModel;
import org.gridsuite.modification.utils.ModificationUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_VOLTAGE_LEVEL_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 */
public class VoltageLevelCreation extends AbstractModification {

    private final VoltageLevelCreationModel modificationModel;

    public VoltageLevelCreation(VoltageLevelCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        String errorMessage = "Voltage level '" + modificationModel.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().controlVoltageLevelCreation(modificationModel, network);
        checkIsNotNegativeValue(errorMessage, modificationModel.getNominalV(), CREATE_VOLTAGE_LEVEL_ERROR, "Nominal Voltage");
        checkIsNotNegativeValue(errorMessage, modificationModel.getLowVoltageLimit(), CREATE_VOLTAGE_LEVEL_ERROR, "Low voltage limit");
        checkIsNotNegativeValue(errorMessage, modificationModel.getHighVoltageLimit(), CREATE_VOLTAGE_LEVEL_ERROR, "High voltage limit");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        ModificationUtils.getInstance().createVoltageLevel(modificationModel, subReportNode, network, namingStrategy);
    }

    @Override
    public String getName() {
        return "VoltageLevelCreation";
    }
}
