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
import lombok.Builder;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.EquipmentModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ShuntCompensatorCreationInfos;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.TABULAR_CREATION_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
public class TabularCreation extends AbstractTabularModification {

    @Builder
    public TabularCreation(List<ModificationInfos> modifications, ModificationType modificationType) {
        super(modifications, modificationType);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modifications == null) {
            throw new NetworkModificationException(TABULAR_CREATION_ERROR, "No tabular creation to apply !!");
        }
    }

    @Override
    public String getName() {
        return "TabularCreation";
    }

    @Override
    public String defaultMessage() {
        return formatEquipmentTypeName() + " have been created";
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
