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
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import groovyjarjarantlr4.v4.runtime.misc.NotNull;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.LineAttachToVoltageLevelInfos;
import org.gridsuite.modification.dto.LineCreationInfos;
import org.gridsuite.modification.dto.SubstationCreationInfos;
import org.gridsuite.modification.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class LineAttachToVoltageLevel extends AbstractModification {

    private final LineAttachToVoltageLevelInfos modificationInfos;

    public LineAttachToVoltageLevel(LineAttachToVoltageLevelInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) {
        if (network.getLine(modificationInfos.getLineToAttachToId()) == null) {
            throw new NetworkModificationRunException("Line not found: " + modificationInfos.getLineToAttachToId());
        }
        LineCreationInfos attachmentLineInfos = modificationInfos.getAttachmentLine();
        ModificationUtils.getInstance().controlNewOrExistingVoltageLevel(modificationInfos.getMayNewVoltageLevelInfos(),
                modificationInfos.getExistingVoltageLevelId(), modificationInfos.getBbsOrBusId(), network);
        // new fictitious VL
        if (network.getVoltageLevel(modificationInfos.getAttachmentPointId()) != null) {
            throw new NetworkModificationRunException("Voltage level already exists: " + modificationInfos.getAttachmentPointId());
        }
        // check future lines don't exist
        if (network.getLine(attachmentLineInfos.getEquipmentId()) != null) {
            throw new NetworkModificationRunException("Line already exists: " + attachmentLineInfos.getEquipmentId());
        }
        if (network.getLine(modificationInfos.getNewLine1Id()) != null) {
            throw new NetworkModificationRunException("Line already exists: " + modificationInfos.getNewLine1Id());
        }
        if (network.getLine(modificationInfos.getNewLine2Id()) != null) {
            throw new NetworkModificationRunException("Line already exists: " + modificationInfos.getNewLine2Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevelCreationInfos mayNewVL = modificationInfos.getMayNewVoltageLevelInfos();
        if (mayNewVL != null) {
            ModificationUtils.getInstance().createVoltageLevel(mayNewVL, subReportNode, network);
            // properties
            VoltageLevel voltageLevel = network.getVoltageLevel(mayNewVL.getEquipmentId());
            PropertiesUtils.applyProperties(voltageLevel, null, mayNewVL.getProperties(), null);
        }

        LineCreationInfos attachmentLineInfos = modificationInfos.getAttachmentLine();
        LineAdder lineAdder = network.newLine()
                .setId(attachmentLineInfos.getEquipmentId())
                .setName(attachmentLineInfos.getEquipmentName())
                .setR(attachmentLineInfos.getR())
                .setX(attachmentLineInfos.getX())
                .setG1(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getG1()))
                .setB1(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getB1()))
                .setG2(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getG2()))
                .setB2(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getB2()));
        String newSubstationId = modificationInfos.getAttachmentPointDetailInformation() != null ?
                modificationInfos.getAttachmentPointDetailInformation().getSubstationCreation().getEquipmentId() :
                modificationInfos.getAttachmentPointId() + "_substation";
        CreateLineOnLine algo = new CreateLineOnLineBuilder()
                .withPositionPercent(modificationInfos.getPercent())
                .withBusbarSectionOrBusId(modificationInfos.getBbsOrBusId())
                .withFictitiousVoltageLevelId(modificationInfos.getAttachmentPointId())
                .withFictitiousVoltageLevelName(modificationInfos.getAttachmentPointName())
                .withCreateFictitiousSubstation(true)
                .withFictitiousSubstationId(newSubstationId)
                .withLine1Id(modificationInfos.getNewLine1Id())
                .withLine1Name(modificationInfos.getNewLine1Name())
                .withLine2Id(modificationInfos.getNewLine2Id())
                .withLine2Name(modificationInfos.getNewLine2Name())
                .withLine(network.getLine(modificationInfos.getLineToAttachToId()))
                .withLineAdder(lineAdder)
                .build();

        algo.apply(network, true, subReportNode);
        // override attachment point
        if (modificationInfos.getAttachmentPointDetailInformation() != null) {
            // override voltage level
            updateAttachmentVoltageLevel(network, modificationInfos.getAttachmentPointDetailInformation());
        }
    }

    private void updateAttachmentVoltageLevel(Network network, @NotNull VoltageLevelCreationInfos attachmentPointDetailInformation) {
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getAttachmentPointId());
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
        SubstationCreationInfos substationCreationInfos = attachmentPointDetailInformation.getSubstationCreation();
        updateAttachmentSubstation(network, substationCreationInfos);
    }

    private void updateAttachmentSubstation(Network network, @NotNull SubstationCreationInfos substationCreationInfos) {
        final Substation substation = network.getSubstation(substationCreationInfos.getEquipmentId());
        if (substationCreationInfos.getEquipmentName() != null) {
            substation.setName(substationCreationInfos.getEquipmentName());
        }
        if (substationCreationInfos.getCountry() != null) {
            substation.setCountry(substationCreationInfos.getCountry());
        }
        PropertiesUtils.applyProperties(substation, substationCreationInfos.getProperties());
    }

    @Override
    public String getName() {
        return "LineAttachToVoltageLevel";
    }
}
