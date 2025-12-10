/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.apache.commons.math3.util.Pair;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.dto.LccModificationInfos;
import org.gridsuite.modification.dto.LccShuntCompensatorModificationInfos;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.gridsuite.modification.utils.ModificationUtils.NO_VALUE;

public class LccModification extends AbstractModification {

    private final LccModificationInfos modificationInfos;

    public LccModification(LccModificationInfos lccModificationInfos) {
        this.modificationInfos = lccModificationInfos;
    }

    @Override
    public String getName() {
        return "LCC_Modification";
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, modificationInfos.getEquipmentId());
        modifyLcc(network, hvdcLine, modificationInfos, subReportNode);
    }

    private static void modifyCharacteristics(HvdcLine hvdcLine, LccModificationInfos modificationInfos, ReportNode subReportNode) {
        List<ReportNode> characteristicsReportsContainer = new ArrayList<>();
        String errorMessage = "Lcc '" + modificationInfos.getEquipmentId() + "' : ";

        //Name
        if (modificationInfos.getEquipmentName() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setName,
                () -> hvdcLine.getOptionalName().orElse(NO_VALUE),
                modificationInfos.getEquipmentName(), "Name"));
        }

        // Nominal Voltage
        if (modificationInfos.getNominalV() != null) {
            ModificationUtils.checkIsNotNegativeValue(errorMessage, modificationInfos.getNominalV().getValue(), "Nominal voltage");
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setNominalV,
                hvdcLine::getNominalV, modificationInfos.getNominalV(), "DC nominal voltage"));
        }

        // DC resistance
        if (modificationInfos.getR() != null) {
            ModificationUtils.checkIsNotNegativeValue(errorMessage, modificationInfos.getR().getValue(), "DC resistance");
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setR,
                hvdcLine::getR, modificationInfos.getR(), "DC resistance"));
        }

        // Maximum active power
        if (modificationInfos.getMaxP() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setMaxP,
                hvdcLine::getMaxP, modificationInfos.getMaxP(), "Power max"));
        }

        // Converters mode
        if (modificationInfos.getConvertersMode() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setConvertersMode,
                hvdcLine::getConvertersMode, modificationInfos.getConvertersMode(), "Converters Mode"));
        }

        // Active power setpoint
        if (modificationInfos.getActivePowerSetpoint() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setActivePowerSetpoint,
                hvdcLine::getActivePowerSetpoint, modificationInfos.getActivePowerSetpoint(), "Active Power Set Point"));
        }

        if (!characteristicsReportsContainer.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReportsContainer, "network.modification.Characteristics");
        }

    }

    private void modifyLcc(@Nonnull Network network, @Nonnull HvdcLine hvdcLine, LccModificationInfos modificationInfos, ReportNode subReportNode) {

        modifyCharacteristics(hvdcLine, modificationInfos, subReportNode);

        // Properties
        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationInfos.getProperties(), "network.modification.LCC_Properties");

        // stations
        if (modificationInfos.getConverterStation1() != null) {
            modifyConverterStation(network, modificationInfos.getConverterStation1(), "1", subReportNode);
        }
        if (modificationInfos.getConverterStation2() != null) {
            modifyConverterStation(network, modificationInfos.getConverterStation2(), "2", subReportNode);
        }

    }

    private void modifyConverterStation(@Nonnull Network network,
                                        @Nonnull LccConverterStationModificationInfos converterStationModificationInfos,
                                        String logFieldName,
                                        ReportNode subReportNode) {

        String errorMessage = "Lcc converter station '" + converterStationModificationInfos.getEquipmentId() + "' : ";
        LccConverterStation converterStation = network.getLccConverterStation(converterStationModificationInfos.getEquipmentId());
        if (!converterStationModificationInfos.hasModifications() || converterStation == null) {
            return;
        }

        ReportNode converterStationReportNode = subReportNode.newReportNode()
            .withMessageTemplate("network.modification.lccConverterStationModified")
            .withUntypedValue("fieldName", logFieldName)
            .withUntypedValue("id", converterStationModificationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        // Characteristics
        List<ReportNode> characteristicsReports = new ArrayList<>();

        if (converterStationModificationInfos.getEquipmentName() != null) {
            characteristicsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setName,
                () -> converterStation.getOptionalName().orElse(NO_VALUE), converterStationModificationInfos.getEquipmentName(), "Equipment name"));
        }

        if (converterStationModificationInfos.getLossFactor() != null) {
            ModificationUtils.checkIsPercentage(errorMessage, converterStationModificationInfos.getLossFactor().getValue(), "Loss factor");
            characteristicsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setLossFactor,
                converterStation::getLossFactor, converterStationModificationInfos.getLossFactor(), "Loss factor"));
        }

        if (converterStationModificationInfos.getPowerFactor() != null) {
            ModificationUtils.checkIsInInterval(errorMessage, converterStationModificationInfos.getPowerFactor().getValue(), new Pair<>(0.f, 1.f), "Power factor");
            characteristicsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setPowerFactor,
                converterStation::getPowerFactor, converterStationModificationInfos.getPowerFactor(), "Power factor"));
        }

        ModificationUtils.getInstance().reportModifications(converterStationReportNode, characteristicsReports,
                "network.modification.Characteristics");

        if (!converterStationModificationInfos.getShuntCompensatorsOnSide().isEmpty()) {
            ReportNode shuntCompensatorReportNode = converterStationReportNode.newReportNode()
                .withMessageTemplate("network.modification.converterStationFilters")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
            modifyShuntCompensatorsOnSide(network, converterStation.getTerminal().getVoltageLevel(),
                converterStationModificationInfos, shuntCompensatorReportNode);
        }
    }

    private void modifyShuntCompensatorsOnSide(Network network, VoltageLevel voltageLevel,
                                              @Nonnull LccConverterStationModificationInfos converterStationInfos, ReportNode reportNode) {

        List<LccShuntCompensatorModificationInfos> shuntCompensatorsOnSide = converterStationInfos.getShuntCompensatorsOnSide();

        Optional.ofNullable(shuntCompensatorsOnSide).ifPresent(shuntCompensators ->
            shuntCompensators.forEach(infos -> {
                ShuntCompensator shuntCompensator = network.getShuntCompensator(infos.getId());
                List<ReportNode> shuntCompensatorReportNodes = new ArrayList<>();
                modifyShuntCompensator(voltageLevel, shuntCompensatorReportNodes, infos, shuntCompensator);
                ModificationUtils.getInstance().reportModifications(reportNode, shuntCompensatorReportNodes,
                        "network.modification.lcc.shuntCompensator.modification", Map.of("id", infos.getId()));
            }));
    }

    private static void modifyShuntCompensator(VoltageLevel voltageLevel, List<ReportNode> nodes, LccShuntCompensatorModificationInfos infos, ShuntCompensator shuntCompensator) {

        if (shuntCompensator == null) {
            nodes.add(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("network.modification.lcc.shuntCompensator.notFound")
                .withUntypedValue("id", infos.getId())
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
            return;
        }

        if (infos.isDeletionMark()) {
            shuntCompensator.remove();
            nodes.add(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("network.modification.lcc.shuntCompensator.removed")
                .withUntypedValue("id", infos.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
            return;
        }

        if (infos.getName() != null && !infos.getName().equals(shuntCompensator.getOptionalName().orElse(NO_VALUE))) {
            nodes.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensator.getOptionalName().orElse(NO_VALUE), infos.getName(), "Name"));
            shuntCompensator.setName(infos.getName());
        }

        if (infos.getConnectedToHvdc() != null) {
            if (Boolean.TRUE.equals(infos.getConnectedToHvdc())) {
                shuntCompensator.getTerminal().connect();
                nodes.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.lcc.shuntCompensator.connected")
                    .withUntypedValue("id", shuntCompensator.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            } else {
                shuntCompensator.getTerminal().disconnect();
                nodes.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.lcc.shuntCompensator.disconnected")
                    .withUntypedValue("id", shuntCompensator.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }
        }

        if (infos.getMaxQAtNominalV() != null) {
            ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
            double newValue = (infos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2);
            double oldValue = model.getBPerSection();
            if (newValue != oldValue) {
                nodes.add(ModificationUtils.getInstance().buildModificationReport(oldValue, newValue, "B Per Section"));
                model.setBPerSection(newValue);
            }
        }
    }

}
