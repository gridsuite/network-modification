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
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_VOLTAGE_LEVEL_ERROR;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@AllArgsConstructor
@Builder
public class CreateVoltageLevelSection extends AbstractModification {

    private String voltageLevelId;
    private int busbarIndex;
    private boolean afterBusbarSectionId;
    private String leftSwitchKind;
    private String rightSwitchKind;
    private boolean allBusbars;
    private String busbarSectionId;
    private boolean switchOpen;

    @Override
    public void check(Network network) throws NetworkModificationException {
        var voltageLevel = network.getVoltageLevel(voltageLevelId);
        if (voltageLevel == null) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Voltage level not found: " + voltageLevelId);
        }
        var bbs = network.getBusbarSection(busbarSectionId);
        if (bbs == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND,
                 String.format("%s not found in voltage level %s",
                     busbarSectionId,
                     voltageLevel.getId()));
        }
        if (bbs.getExtension(BusbarSectionPosition.class) == null) {
            throw new PowsyblException(String.format("busbar section position extension are not available on this busbar section %s, can not create a new busbar section",
                busbarSectionId));
        }
        var currentBusbarIndex = bbs.getExtension(BusbarSectionPosition.class).getBusbarIndex();
        if (currentBusbarIndex != busbarIndex) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND,
                String.format("%s is not the busbar index of the busbar section %s in voltage level %s",
                    busbarIndex,
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
        VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
        BusbarSection busbarSection = network.getBusbarSection(busbarSectionId);
        List<String> busBarIds = new ArrayList<>();
        voltageLevel.getNodeBreakerView().getBusbarSections().forEach(bbs -> busBarIds.add(bbs.getId()));

        CreateVoltageLevelSections modification = new CreateVoltageLevelSectionsBuilder()
                .withReferenceBusbarSectionId(busbarSection.getId())
                .withCreateTheBusbarSectionsAfterTheReferenceBusbarSection(afterBusbarSectionId)
                .withAllBusbars(allBusbars)
                .withLeftSwitchOpen(switchOpen)
                .withRightSwitchOpen(switchOpen)
                .withLeftSwitchKind(leftSwitchKind != null ? SwitchKind.valueOf(leftSwitchKind) : SwitchKind.DISCONNECTOR)
                .withRightSwitchKind(rightSwitchKind != null ? SwitchKind.valueOf(rightSwitchKind) : SwitchKind.DISCONNECTOR)
                .withSwitchPrefixId(voltageLevel.getId())
                .withBusbarSectionPrefixId(voltageLevel.getId())
                .build();
        modification.apply(network, namingStrategy, true, subReportNode);

        if (allBusbars) {
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
