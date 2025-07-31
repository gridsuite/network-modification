/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.CreateVoltageLevelSections;
import com.powsybl.iidm.modification.topology.CreateVoltageLevelSectionsBuilder;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.VoltageLevelSectionsCreationInfos;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_VOLTAGE_LEVEL_ERROR;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class VoltageLevelSectionsCreation extends AbstractModification {

    private final VoltageLevelSectionsCreationInfos modificationInfos;

    public VoltageLevelSectionsCreation(VoltageLevelSectionsCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        String errorMessage = "Voltage level '" + modificationInfos.getVoltageLevelId() + "' : ";
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getVoltageLevelId());
        if (voltageLevel == null) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Voltage level not found: " + modificationInfos.getVoltageLevelId());
        }

        CreateVoltageLevelSections modification = new CreateVoltageLevelSectionsBuilder()
                .withReferenceBusbarSectionId(modificationInfos.getReferenceBusbarSectionId())
                .withCreateTheBusbarSectionsAfterTheReferenceBusbarSection(modificationInfos.isCreateTheBusbarSectionsAfterTheReferenceBusbarSection())
                .withAllBusbars(modificationInfos.isAllBusbars())
                .withLeftSwitchKind(modificationInfos.getLeftSwitchKind() != null ? SwitchKind.valueOf(modificationInfos.getLeftSwitchKind()) : null)
                .withRightSwitchKind(modificationInfos.getRightSwitchKind() != null ? SwitchKind.valueOf(modificationInfos.getRightSwitchKind()) : null)
                .withSwitchPrefixId(voltageLevel.getId())
                .withBusbarSectionPrefixId(voltageLevel.getId())
                .build();
        modification.apply(network);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevel.sections.created")
                .withUntypedValue("voltageLevelId", modificationInfos.getVoltageLevelId())
                .add();
    }

    @Override
    public String getName() {
        return ModificationType.VOLTAGE_LEVEL_BUS_BAR_SECTIONS_CREATION.name();
    }
}
