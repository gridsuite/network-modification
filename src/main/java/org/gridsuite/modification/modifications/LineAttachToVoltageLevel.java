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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.LineAttachToVoltageLevelModel;
import org.gridsuite.modification.model.LineCreationModel;
import org.gridsuite.modification.model.SubstationCreationModel;
import org.gridsuite.modification.model.VoltageLevelCreationModel;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.modifications.LineCreation.addLimits;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class LineAttachToVoltageLevel extends AbstractModification {

    private final LineAttachToVoltageLevelModel modificationModel;

    public LineAttachToVoltageLevel(LineAttachToVoltageLevelModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getLine(modificationModel.getLineToAttachToId()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationModel.getLineToAttachToId());
        }
        LineCreationModel attachmentLineModel = modificationModel.getAttachmentLine();
        ModificationUtils.getInstance().controlNewOrExistingVoltageLevel(modificationModel.getMayNewVoltageLevelInfos(),
            modificationModel.getExistingVoltageLevelId(), modificationModel.getBbsOrBusId(), network);
        // new fictitious VL
        if (network.getVoltageLevel(modificationModel.getAttachmentPointId()) != null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, modificationModel.getAttachmentPointId());
        }
        // check future lines don't exist
        if (network.getLine(attachmentLineModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, attachmentLineModel.getEquipmentId());
        }
        if (network.getLine(modificationModel.getNewLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationModel.getNewLine1Id());
        }
        if (network.getLine(modificationModel.getNewLine2Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationModel.getNewLine2Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        VoltageLevelCreationModel mayNewVL = modificationModel.getMayNewVoltageLevelInfos();
        if (mayNewVL != null) {
            ModificationUtils.getInstance().createVoltageLevel(mayNewVL, subReportNode, network, namingStrategy);
        }
        LineCreationModel attachmentLineModel = modificationModel.getAttachmentLine();
        LineAdder lineAdder = network.newLine()
            .setId(attachmentLineModel.getEquipmentId())
            .setName(attachmentLineModel.getEquipmentName())
            .setR(attachmentLineModel.getR())
            .setX(attachmentLineModel.getX())
            .setG1(ModificationUtils.getInstance().zeroIfNull(attachmentLineModel.getG1()))
            .setB1(ModificationUtils.getInstance().zeroIfNull(attachmentLineModel.getB1()))
            .setG2(ModificationUtils.getInstance().zeroIfNull(attachmentLineModel.getG2()))
            .setB2(ModificationUtils.getInstance().zeroIfNull(attachmentLineModel.getB2()));
        String newSubstationId = modificationModel.getAttachmentPointDetailInformation() != null &&
            modificationModel.getAttachmentPointDetailInformation().getSubstationCreation() != null ?
            modificationModel.getAttachmentPointDetailInformation().getSubstationCreation().getEquipmentId() :
            modificationModel.getAttachmentPointId() + "_substation";
        CreateLineOnLine algo = new CreateLineOnLineBuilder()
            .withPositionPercent(modificationModel.getPercent())
            .withBusbarSectionOrBusId(modificationModel.getBbsOrBusId())
            .withFictitiousVoltageLevelId(modificationModel.getAttachmentPointId())
            .withFictitiousVoltageLevelName(modificationModel.getAttachmentPointName())
            .withCreateFictitiousSubstation(true)
            .withFictitiousSubstationId(newSubstationId)
            .withLine1Id(modificationModel.getNewLine1Id())
            .withLine1Name(modificationModel.getNewLine1Name())
            .withLine2Id(modificationModel.getNewLine2Id())
            .withLine2Name(modificationModel.getNewLine2Name())
            .withLine(network.getLine(modificationModel.getLineToAttachToId()))
            .withLineAdder(lineAdder)
            .build();

        algo.apply(network, true, subReportNode);

        // add extra information on attachment line TODO remove when powsybl core fixes it
        Line createdAttachmentLine = network.getLine(attachmentLineModel.getEquipmentId());
        addLimits(attachmentLineModel, subReportNode, createdAttachmentLine);
        PropertiesUtils.applyProperties(createdAttachmentLine, subReportNode, attachmentLineModel.getProperties(), "network.modification.LineProperties");

        // override attachment point
        if (modificationModel.getAttachmentPointDetailInformation() != null) {
            // override voltage level
            updateAttachmentVoltageLevel(network, modificationModel.getAttachmentPointDetailInformation());
        }
    }

    private void updateAttachmentVoltageLevel(Network network, @NotNull VoltageLevelCreationModel attachmentPointDetailInformation) {
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationModel.getAttachmentPointId());
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
        SubstationCreationModel substationCreationModel = attachmentPointDetailInformation.getSubstationCreation();
        if (substationCreationModel != null) {
            updateAttachmentSubstation(network, substationCreationModel);
        }
    }

    private void updateAttachmentSubstation(Network network, @NotNull SubstationCreationModel substationCreationModel) {
        final Substation substation = network.getSubstation(substationCreationModel.getEquipmentId());
        if (substationCreationModel.getEquipmentName() != null) {
            substation.setName(substationCreationModel.getEquipmentName());
        }
        if (substationCreationModel.getCountry() != null) {
            substation.setCountry(substationCreationModel.getCountry());
        }
        PropertiesUtils.applyProperties(substation, substationCreationModel.getProperties());
    }

    @Override
    public String getName() {
        return "LineAttachToVoltageLevel";
    }
}
