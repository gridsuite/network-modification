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
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.EquipmentModificationInfos;
import org.gridsuite.modification.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.dto.tabular.TabularCreationInfos;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class TabularCreation extends AbstractTabularModification {

    public TabularCreation(TabularCreationInfos modificationInfos) {
        super(modificationInfos);
    }

    @Override
    public void check(Network network) throws NetworkModificationRunException {
        if (modificationInfos == null) {
            throw new NetworkModificationRunException("No tabular creation to apply !!");
        }
    }

    @Override
    public String getName() {
        return "TabularCreation";
    }

    @Override
    public String defaultMessage() {
        return modificationInfos.formatEquipmentTypeName() + " have been created";
    }

    @Override
    public String baseTemplateMessage() {
        return "network.modification.tabular.creation";
    }

    @Override
    public void specificCheck(EquipmentModificationInfos equipmentModificationInfos, Network network, ReportNode subReportNode) {
        if (equipmentModificationInfos instanceof ShuntCompensatorCreationInfos shuntCompensatorCreationInfos && shuntCompensatorCreationInfos.getMaxSusceptance() != null) {
            if (shuntCompensatorCreationInfos.getShuntCompensatorType() != null && shuntCompensatorCreationInfos.getMaxQAtNominalV() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.creation.shuntCompensator.maxSusceptanceIgnored.1")
                        .withUntypedValue("id", shuntCompensatorCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else if (shuntCompensatorCreationInfos.getShuntCompensatorType() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.creation.shuntCompensator.maxSusceptanceIgnored.2")
                        .withUntypedValue("id", shuntCompensatorCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else if (shuntCompensatorCreationInfos.getMaxQAtNominalV() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.creation.shuntCompensator.maxSusceptanceIgnored.3")
                        .withUntypedValue("id", shuntCompensatorCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            }
        }
    }
}
