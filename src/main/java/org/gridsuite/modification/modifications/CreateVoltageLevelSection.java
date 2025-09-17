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
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.BusbarSectionPosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CreateVoltageLevelSectionInfos;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_VOLTAGE_LEVEL_ERROR;

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
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND,
                 String.format("%s not found in voltage level %s",
                     modificationInfos.getBusbarSectionId(),
                     voltageLevel.getId()));
        }
        if (bbs.getExtension(BusbarSectionPosition.class) == null) {
            throw new PowsyblException(String.format("busbar section position extension are not available on this busbar section %s, can not create a new busbar section",
                modificationInfos.getBusbarSectionId()));
        }
        var busbarIndex = bbs.getExtension(BusbarSectionPosition.class).getBusbarIndex();
        if (busbarIndex != modificationInfos.getBusbarIndex()) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND,
                String.format("%s is not the busbar index of the busbar section %s in voltage level %s",
                    modificationInfos.getBusbarIndex(),
                    bbs.getId(),
                    voltageLevel.getId()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getVoltageLevelId());
        BusbarSection busbarSection = network.getBusbarSection(modificationInfos.getBusbarSectionId());
        List<String> busBarIds = new ArrayList<>();
        voltageLevel.getNodeBreakerView().getBusbarSections().forEach(bbs -> busBarIds.add(bbs.getId()));

        CreateVoltageLevelSections modification = new CreateVoltageLevelSectionsBuilder()
                .withReferenceBusbarSectionId(busbarSection.getId())
                .withCreateTheBusbarSectionsAfterTheReferenceBusbarSection(modificationInfos.isAfterBusbarSectionId())
                .withAllBusbars(modificationInfos.isAllBusbars())
                .withLeftSwitchKind(modificationInfos.getLeftSwitchKind() != null ? SwitchKind.valueOf(modificationInfos.getLeftSwitchKind()) : SwitchKind.DISCONNECTOR)
                .withRightSwitchKind(modificationInfos.getRightSwitchKind() != null ? SwitchKind.valueOf(modificationInfos.getRightSwitchKind()) : SwitchKind.DISCONNECTOR)
                .withSwitchPrefixId(voltageLevel.getId())
                .withBusbarSectionPrefixId(voltageLevel.getId())
                .build();
        modification.apply(network, namingStrategy, true, subReportNode);

        if (modificationInfos.isAllBusbars()) {
            List<BusbarSection> newBusbarSections = new ArrayList<>();
            for (BusbarSection bbs : voltageLevel.getNodeBreakerView().getBusbarSections()) {
                if (!busBarIds.contains(bbs.getId())) {
                    newBusbarSections.add(bbs);
                }
            }
            for (BusbarSection section : newBusbarSections) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.voltageLevel.sectionCreation")
                        .withUntypedValue("sectionId", section.getId())
                        .withUntypedValue("busbarIndex", section.getExtension(BusbarSectionPosition.class).getBusbarIndex())
                        .withUntypedValue("sectionIndex", section.getExtension(BusbarSectionPosition.class).getSectionIndex())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
            }
        } else {
            BusbarSection newBusbarSection = null;
            for (BusbarSection bbs : voltageLevel.getNodeBreakerView().getBusbarSections()) {
                if (!busBarIds.contains(bbs.getId())) {
                    newBusbarSection = bbs;
                    break;
                }
            }
            if (newBusbarSection != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.voltageLevel.sectionCreation")
                        .withUntypedValue("sectionId", newBusbarSection.getId())
                        .withUntypedValue("busbarIndex", newBusbarSection.getExtension(BusbarSectionPosition.class).getBusbarIndex())
                        .withUntypedValue("sectionIndex", newBusbarSection.getExtension(BusbarSectionPosition.class).getSectionIndex())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
            }
        }
    }

    @Override
    public String getName() {
        return ModificationType.CREATE_VOLTAGE_LEVEL_SECTION.name();
    }
}
