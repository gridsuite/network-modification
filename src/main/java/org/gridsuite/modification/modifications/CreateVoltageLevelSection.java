/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.CreateVoltageLevelSections;
import com.powsybl.iidm.modification.topology.CreateVoltageLevelSectionsBuilder;
import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.BusbarSectionPosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CreateVoltageLevelSectionInfos;

import static org.gridsuite.modification.NetworkModificationException.Type.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class CreateVoltageLevelSection extends AbstractModification {

    private final CreateVoltageLevelSectionInfos modificationInfos;

    public CreateVoltageLevelSection(CreateVoltageLevelSectionInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        var voltageLevel = network.getVoltageLevel(modificationInfos.getVoltageLevelId());
        if (voltageLevel == null) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Voltage level not found: " + modificationInfos.getVoltageLevelId());
        }
        var bbs = network.getBusbarSection(modificationInfos.getBusbarSectionId());
        if (bbs == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, modificationInfos.getBusbarSectionId());
        }
        if (bbs.getExtension(BusbarSectionPosition.class) == null) {
            throw new PowsyblException("not available on these busbar section");
        }
        var busbarIndex = bbs.getExtension(BusbarSectionPosition.class).getBusbarIndex();
        var sectionIndex = bbs.getExtension(BusbarSectionPosition.class).getSectionIndex();
        if (busbarIndex != modificationInfos.getBusbarCount() || sectionIndex != modificationInfos.getSectionCount()) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND);
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getVoltageLevelId());
        BusbarSection busbarSection = network.getBusbarSection(modificationInfos.getBusbarSectionId());
        CreateVoltageLevelSections modification = new CreateVoltageLevelSectionsBuilder()
                .withReferenceBusbarSectionId(busbarSection.getId())
                .withCreateTheBusbarSectionsAfterTheReferenceBusbarSection(modificationInfos.isAfterBusbarSectionId())
                .withAllBusbars(false)
                .withLeftSwitchKind(modificationInfos.getLeftSwitchKind() != null ? SwitchKind.valueOf(modificationInfos.getLeftSwitchKind()) : null)
                .withRightSwitchKind(modificationInfos.getRightSwitchKind() != null ? SwitchKind.valueOf(modificationInfos.getRightSwitchKind()) : null)
                .withSwitchPrefixId(voltageLevel.getId())
                .withBusbarSectionPrefixId(voltageLevel.getId())
                .build();
        modification.apply(network, true, subReportNode);
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevel.sectionCreation")
                .withUntypedValue("busbarIndex", modificationInfos.getBusbarCount())
                .withUntypedValue("newSectionIndex", modificationInfos.isAfterBusbarSectionId() ?
                        modificationInfos.getSectionCount() + 1 : modificationInfos.getSectionCount() - 1)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    @Override
    public String getName() {
        return ModificationType.CREATE_VOLTAGE_LEVEL_SECTION.name();
    }
}
