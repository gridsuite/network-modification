/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabular;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.ShuntCompensatorCreationModel;
import org.gridsuite.modification.model.tabular.TabularCreationModel;

import static org.gridsuite.modification.NetworkModificationException.Type.TABULAR_CREATION_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class TabularCreation extends AbstractTabularModification {

    public TabularCreation(TabularCreationModel modificationModel) {
        super(modificationModel);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationModel == null) {
            throw new NetworkModificationException(TABULAR_CREATION_ERROR, "No tabular creation to apply !!");
        }
    }

    @Override
    public String getName() {
        return "TabularCreation";
    }

    @Override
    public String defaultMessage() {
        return modificationModel.formatEquipmentTypeName() + " have been created";
    }

    @Override
    public String baseTemplateMessage() {
        return "network.modification.tabular.creation";
    }

    @Override
    public void specificCheck(ModificationModel equipmentModificationModel, Network network, ReportNode subReportNode) {
        if (equipmentModificationModel instanceof ShuntCompensatorCreationModel shuntCompensatorCreationModel && shuntCompensatorCreationModel.getMaxSusceptance() != null) {
            if (shuntCompensatorCreationModel.getShuntCompensatorType() != null && shuntCompensatorCreationModel.getMaxQAtNominalV() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.creation.shuntCompensator.maxSusceptanceIgnored.1")
                        .withUntypedValue("id", shuntCompensatorCreationModel.getEquipmentId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else if (shuntCompensatorCreationModel.getShuntCompensatorType() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.creation.shuntCompensator.maxSusceptanceIgnored.2")
                        .withUntypedValue("id", shuntCompensatorCreationModel.getEquipmentId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else if (shuntCompensatorCreationModel.getMaxQAtNominalV() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.creation.shuntCompensator.maxSusceptanceIgnored.3")
                        .withUntypedValue("id", shuntCompensatorCreationModel.getEquipmentId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            }
        }
    }
}
