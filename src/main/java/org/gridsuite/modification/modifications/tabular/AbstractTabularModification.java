/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabular;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.dto.EquipmentModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularBaseInfos;
import org.gridsuite.modification.modifications.AbstractModification;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public abstract class AbstractTabularModification extends AbstractModification {

    protected static final String DEFAULT_MESSAGE_KEY = "defaultMessage";

    protected static final Logger LOGGER = LoggerFactory.getLogger(AbstractTabularModification.class);

    protected final TabularBaseInfos modificationInfos;

    protected AbstractTabularModification(TabularBaseInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    public abstract void specificCheck(EquipmentModificationInfos equipmentModificationInfos, Network network, ReportNode subReportNode);

    public abstract String defaultMessage();

    public abstract String baseTemplateMessage();

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        int applicationFailuresCount = 0;
        for (var modifInfos : modificationInfos.getModifications()) {
            EquipmentModificationInfos equipmentModificationInfos = (EquipmentModificationInfos) modifInfos;
            ReportNode modifReportNode = subReportNode.newReportNode()
                    .withMessageTemplate(baseTemplateMessage() + ".equipmentId")
                    .withUntypedValue("equipmentId", equipmentModificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            try {
                AbstractModification modification = equipmentModificationInfos.toModification();
                modification.check(network);
                specificCheck(equipmentModificationInfos, network, modifReportNode);
                modification.apply(network, modifReportNode);
            } catch (Exception e) {
                applicationFailuresCount++;
                ReportNode errorReportNode = modifReportNode.newReportNode()
                        .withMessageTemplate(baseTemplateMessage() + ".error.equipmentError")
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .add();
                errorReportNode.newReportNode()
                        .withMessageTemplate(baseTemplateMessage() + ".exception")
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .add();
                LOGGER.warn(e.getMessage());
            }
        }
        if (modificationInfos.getModifications().size() == applicationFailuresCount) {
            subReportNode.newReportNode()
                    .withMessageTemplate(baseTemplateMessage() + ".error")
                    .withUntypedValue(DEFAULT_MESSAGE_KEY, defaultMessage())
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
        } else if (applicationFailuresCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate(baseTemplateMessage() + ".partial")
                    .withUntypedValue("modificationsCount", modificationInfos.getModifications().size() - applicationFailuresCount)
                    .withUntypedValue("failuresCount", applicationFailuresCount)
                    .withUntypedValue(DEFAULT_MESSAGE_KEY, defaultMessage())
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
        } else {
            subReportNode.newReportNode()
                    .withMessageTemplate(baseTemplateMessage())
                    .withUntypedValue("modificationsCount", modificationInfos.getModifications().size())
                    .withUntypedValue(DEFAULT_MESSAGE_KEY, defaultMessage())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }
}
