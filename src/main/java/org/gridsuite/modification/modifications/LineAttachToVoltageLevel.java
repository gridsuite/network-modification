/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.CreateLineOnLine;
import com.powsybl.iidm.modification.topology.CreateLineOnLineBuilder;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import groovyjarjarantlr4.v4.runtime.misc.NotNull;
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.modifications.LineCreation.addLimits;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@Getter
@Setter
@AllArgsConstructor
@Builder
public class LineAttachToVoltageLevel extends AbstractModification {

    private String lineToAttachToId;
    private double percent;
    private String attachmentPointId;
    private String attachmentPointName;
    private VoltageLevelCreation attachmentPointDetailInformation;
    private VoltageLevelCreation mayNewVoltageLevel;
    private String existingVoltageLevelId;
    private String bbsOrBusId;
    private LineCreation attachmentLine;
    private String newLine1Id;
    private String newLine1Name;
    private String newLine2Id;
    private String newLine2Name;

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getLine(lineToAttachToId) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineToAttachToId);
        }
        ModificationUtils.getInstance().controlNewOrExistingVoltageLevel(mayNewVoltageLevel,
                existingVoltageLevelId, bbsOrBusId, network);
        // new fictitious VL
        if (network.getVoltageLevel(attachmentPointId) != null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, attachmentPointId);
        }
        // check future lines don't exist
        if (network.getLine(attachmentLine.getEquipmentId()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, attachmentLine.getEquipmentId());
        }
        if (network.getLine(newLine1Id) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, newLine1Id);
        }
        if (network.getLine(newLine2Id) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, newLine2Id);
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        if (mayNewVoltageLevel != null) {
            ModificationUtils.getInstance().createVoltageLevel(mayNewVoltageLevel, subReportNode, network, namingStrategy);
        }
        LineAdder lineAdder = network.newLine()
                .setId(attachmentLine.getEquipmentId())
                .setName(attachmentLine.getEquipmentName())
                .setR(attachmentLine.getR())
                .setX(attachmentLine.getX())
                .setG1(ModificationUtils.getInstance().zeroIfNull(attachmentLine.getG1()))
                .setB1(ModificationUtils.getInstance().zeroIfNull(attachmentLine.getB1()))
                .setG2(ModificationUtils.getInstance().zeroIfNull(attachmentLine.getG2()))
                .setB2(ModificationUtils.getInstance().zeroIfNull(attachmentLine.getB2()));
        String newSubstationId = attachmentPointDetailInformation != null &&
                attachmentPointDetailInformation.getSubstationCreation() != null ?
                attachmentPointDetailInformation.getSubstationCreation().getEquipmentId() :
                attachmentPointId + "_substation";
        CreateLineOnLine algo = new CreateLineOnLineBuilder()
                .withPositionPercent(percent)
                .withBusbarSectionOrBusId(bbsOrBusId)
                .withFictitiousVoltageLevelId(attachmentPointId)
                .withFictitiousVoltageLevelName(attachmentPointName)
                .withCreateFictitiousSubstation(true)
                .withFictitiousSubstationId(newSubstationId)
                .withLine1Id(newLine1Id)
                .withLine1Name(newLine1Name)
                .withLine2Id(newLine2Id)
                .withLine2Name(newLine2Name)
                .withLine(network.getLine(lineToAttachToId))
                .withLineAdder(lineAdder)
                .build();

        algo.apply(network, true, subReportNode);

        // add extra information on attachment line TODO remove when powsybl core fixes it
        Line createdAttachmentLine = network.getLine(attachmentLine.getEquipmentId());
        addLimits(attachmentLine.getOperationalLimitsGroups(), attachmentLine.getSelectedOperationalLimitsGroupId1(),
                attachmentLine.getSelectedOperationalLimitsGroupId2(), subReportNode, createdAttachmentLine);
        PropertiesUtils.applyProperties(createdAttachmentLine, subReportNode, attachmentLine.getProperties(), "network.modification.LineProperties");

        // override attachment point
        if (attachmentPointDetailInformation != null) {
            // override voltage level
            updateAttachmentVoltageLevel(network, attachmentPointDetailInformation);
        }
    }

    private void updateAttachmentVoltageLevel(Network network, @NotNull VoltageLevelCreation attachmentPointDetailInformation) {
        VoltageLevel voltageLevel = network.getVoltageLevel(attachmentPointId);
        if (attachmentPointDetailInformation.getHighVoltageLimit() != null) {
            voltageLevel.setHighVoltageLimit(attachmentPointDetailInformation.getHighVoltageLimit());
        }
        if (attachmentPointDetailInformation.getLowVoltageLimit() != null) {
            voltageLevel.setLowVoltageLimit(attachmentPointDetailInformation.getLowVoltageLimit());
        }
        if (attachmentPointDetailInformation.getIpMax() != null || attachmentPointDetailInformation.getIpMin() != null) {
            IdentifiableShortCircuitAdder<VoltageLevel> adder = voltageLevel.newExtension(IdentifiableShortCircuitAdder.class);
            if (attachmentPointDetailInformation.getIpMax() != null) {
                adder.withIpMax(attachmentPointDetailInformation.getIpMax());
            }
            if (attachmentPointDetailInformation.getIpMin() != null) {
                adder.withIpMin(attachmentPointDetailInformation.getIpMin());
            }
            adder.add();
        }
        PropertiesUtils.applyProperties(voltageLevel, attachmentPointDetailInformation.getProperties());
        // override substation
        SubstationCreation substationCreationInfos = attachmentPointDetailInformation.getSubstationCreation();
        if (substationCreationInfos != null) {
            updateAttachmentSubstation(network, substationCreationInfos);
        }
    }

    private void updateAttachmentSubstation(Network network, @NotNull SubstationCreation substationCreation) {
        final Substation substation = network.getSubstation(substationCreation.getEquipmentId());
        if (substationCreation.getEquipmentName() != null) {
            substation.setName(substationCreation.getEquipmentName());
        }
        if (substationCreation.getCountry() != null) {
            substation.setCountry(substationCreation.getCountry());
        }
        PropertiesUtils.applyProperties(substation, substationCreation.getProperties());
    }

    @Override
    public String getName() {
        return "LineAttachToVoltageLevel";
    }
}
