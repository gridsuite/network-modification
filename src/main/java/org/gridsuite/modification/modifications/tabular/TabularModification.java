/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabular;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.EquipmentModificationModel;
import org.gridsuite.modification.model.ShuntCompensatorModificationModel;
import org.gridsuite.modification.model.tabular.TabularModificationModel;

import static org.gridsuite.modification.NetworkModificationException.Type.TABULAR_MODIFICATION_ERROR;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
public class TabularModification extends AbstractTabularModification {

    public TabularModification(TabularModificationModel modificationModel) {
        super(modificationModel);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationModel == null) {
            throw new NetworkModificationException(TABULAR_MODIFICATION_ERROR, "No tabular modification to apply !!");
        }
    }

    @Override
    public String getName() {
        return "TabularModification";
    }

    @Override
    public String defaultMessage() {
        return modificationModel.formatEquipmentTypeName() + " have been modified";
    }

    @Override
    public String baseTemplateMessage() {
        return "network.modification.tabular.modification";
    }

    @Override
    public void specificCheck(EquipmentModificationModel equipmentModificationModel, Network network, ReportNode subReportNode) {
        if (equipmentModificationModel instanceof ShuntCompensatorModificationModel shuntCompensatorModificationModel) {
            var shuntCompensator = network.getShuntCompensator(shuntCompensatorModificationModel.getEquipmentId());
            if (shuntCompensator.getModelType() == ShuntCompensatorModelType.NON_LINEAR) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.shuntCompensator.modifyImpossible")
                        .withUntypedValue("id", shuntCompensator.getId())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .add();
            } else if (shuntCompensatorModificationModel.getMaxSusceptance() != null) {
                if (shuntCompensatorModificationModel.getShuntCompensatorType() != null && shuntCompensatorModificationModel.getMaxQAtNominalV() != null) {
                    subReportNode.newReportNode()
                            .withMessageTemplate("network.modification.tabular.modification.shuntCompensator.maxSusceptanceIgnored.1")
                            .withUntypedValue("id", shuntCompensator.getId())
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .add();
                } else if (shuntCompensatorModificationModel.getShuntCompensatorType() != null) {
                    subReportNode.newReportNode()
                            .withMessageTemplate("network.modification.tabular.modification.shuntCompensator.maxSusceptanceIgnored.2")
                            .withUntypedValue("id", shuntCompensator.getId())
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .add();
                } else if (shuntCompensatorModificationModel.getMaxQAtNominalV() != null) {
                    subReportNode.newReportNode()
                            .withMessageTemplate("network.modification.tabular.modification.shuntCompensator.maxSusceptanceIgnored.3")
                            .withUntypedValue("id", shuntCompensator.getId())
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .add();
                }
            }
        }
    }
}
