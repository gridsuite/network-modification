/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.EquipmentModificationInfos;
import org.gridsuite.modification.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.dto.TabularModificationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.gridsuite.modification.NetworkModificationException.Type.TABULAR_MODIFICATION_ERROR;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
public class TabularModification extends AbstractModification {

    private static final Logger LOGGER = LoggerFactory.getLogger(TabularModification.class);

    private final TabularModificationInfos modificationInfos;

    public TabularModification(TabularModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(TABULAR_MODIFICATION_ERROR, "No tabular modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        int applicationFailuresCount = 0;
        for (var modifInfos : modificationInfos.getModifications()) {
            ReportNode modifReportNode = subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.tabular.modification.equipmentId")
                    .withUntypedValue("equipmentId", ((EquipmentModificationInfos) modifInfos).getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            try {
                AbstractModification modification = modifInfos.toModification();
                modification.check(network);
                if (modifInfos instanceof ShuntCompensatorModificationInfos shuntModification) {
                    checkShuntCompensatorModification(network, shuntModification, modifReportNode);
                }
                modification.apply(network, modifReportNode);
            } catch (Exception e) {
                applicationFailuresCount++;
                ReportNode errorReportNode = modifReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.modification.error.equipmentError")
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
                errorReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.modification.exception")
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
                LOGGER.warn(e.getMessage());
            }
        }
        String defaultMessage = modificationInfos.formatEquipmentTypeName() + " have been modified";
        if (modificationInfos.getModifications().size() == applicationFailuresCount) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.tabular.modification.error")
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
        } else if (applicationFailuresCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.tabular.modification.warning")
                    .withUntypedValue("modificationsCount", modificationInfos.getModifications().size() - applicationFailuresCount)
                    .withUntypedValue("failuresCount", applicationFailuresCount)
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .add();
        } else {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.tabular.modification")
                    .withUntypedValue("modificationsCount", modificationInfos.getModifications().size())
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }

    @Override
    public String getName() {
        return "TabularModification";
    }

    public void checkShuntCompensatorModification(
            Network network,
            ShuntCompensatorModificationInfos shuntCompensatorModificationInfos,
            ReportNode subReportNode
    ) {
        var shuntCompensator = network.getShuntCompensator(shuntCompensatorModificationInfos.getEquipmentId());
        if (shuntCompensator.getModelType() == ShuntCompensatorModelType.NON_LINEAR) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.tabular.shuntCompensator.modifyImpossible")
                    .withUntypedValue("id", shuntCompensator.getId())
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
        } else if (shuntCompensatorModificationInfos.getMaxSusceptance() != null) {
            if (shuntCompensatorModificationInfos.getShuntCompensatorType() != null && shuntCompensatorModificationInfos.getMaxQAtNominalV() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.modification.shuntCompensator.maxSusceptanceIgnored.1")
                        .withUntypedValue("id", shuntCompensator.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else if (shuntCompensatorModificationInfos.getShuntCompensatorType() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.modification.shuntCompensator.maxSusceptanceIgnored.2")
                        .withUntypedValue("id", shuntCompensator.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else if (shuntCompensatorModificationInfos.getMaxQAtNominalV() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.tabular.modification.shuntCompensator.maxSusceptanceIgnored.3")
                        .withUntypedValue("id", shuntCompensator.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            }
        }
    }
}
