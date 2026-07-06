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
import lombok.*;
import org.apache.commons.math3.util.Pair;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LccShuntCompensatorModificationInfos;
import org.gridsuite.modification.modifications.data.LccConverterStationModification;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.gridsuite.modification.utils.ModificationUtils.NO_VALUE;

@Getter
@Setter
public class LccModification extends AbstractEquipmentModification {

    private AttributeModification<Double> nominalV;
    private AttributeModification<Double> r;
    private AttributeModification<Double> maxP;
    private AttributeModification<HvdcLine.ConvertersMode> convertersMode;
    private AttributeModification<Double> activePowerSetpoint;
    private LccConverterStationModification converterStation1;
    private LccConverterStationModification converterStation2;

    @Builder
    public LccModification(String equipmentId, List<FreePropertyInfos> properties,
                           AttributeModification<String> equipmentName, AttributeModification<Double> nominalV,
                           AttributeModification<Double> r, AttributeModification<Double> maxP,
                           AttributeModification<HvdcLine.ConvertersMode> convertersMode,
                           AttributeModification<Double> activePowerSetpoint,
                           LccConverterStationModification converterStation1,
                           LccConverterStationModification converterStation2) {
        super(equipmentId, properties, equipmentName);
        this.nominalV = nominalV;
        this.r = r;
        this.maxP = maxP;
        this.convertersMode = convertersMode;
        this.activePowerSetpoint = activePowerSetpoint;
        this.converterStation1 = converterStation1;
        this.converterStation2 = converterStation2;
    }

    @Override
    public String getName() {
        return "LCC_Modification";
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, equipmentId);
        modifyLcc(network, hvdcLine, subReportNode);
    }

    private void modifyCharacteristics(HvdcLine hvdcLine, ReportNode subReportNode) {
        List<ReportNode> characteristicsReportsContainer = new ArrayList<>();
        String errorMessage = "Lcc '" + equipmentId + "' : ";

        //Name
        if (equipmentName != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setName,
                () -> hvdcLine.getOptionalName().orElse(NO_VALUE),
                equipmentName, "Name"));
        }

        // Nominal Voltage
        if (nominalV != null) {
            ModificationUtils.checkIsNotNegativeValue(errorMessage, nominalV.getValue(), NetworkModificationException.Type.MODIFY_LCC_ERROR, "Nominal voltage");
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setNominalV,
                hvdcLine::getNominalV, nominalV, "DC nominal voltage"));
        }

        // DC resistance
        if (r != null) {
            ModificationUtils.checkIsNotNegativeValue(errorMessage, r.getValue(), NetworkModificationException.Type.MODIFY_LCC_ERROR, "DC resistance");
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setR,
                hvdcLine::getR, r, "DC resistance"));
        }

        // Maximum active power
        if (maxP != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setMaxP,
                hvdcLine::getMaxP, maxP, "Power max"));
        }

        // Converters mode
        if (convertersMode != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setConvertersMode,
                hvdcLine::getConvertersMode, convertersMode, "Converters Mode"));
        }

        // Active power setpoint
        if (activePowerSetpoint != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setActivePowerSetpoint,
                hvdcLine::getActivePowerSetpoint, activePowerSetpoint, "Active Power Set Point"));
        }

        if (!characteristicsReportsContainer.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReportsContainer, "network.modification.Characteristics");
        }

    }

    private void modifyLcc(@Nonnull Network network, @Nonnull HvdcLine hvdcLine, ReportNode subReportNode) {

        modifyCharacteristics(hvdcLine, subReportNode);

        // Properties
        PropertiesUtils.applyProperties(hvdcLine, subReportNode, properties, "network.modification.LCC_Properties");

        // stations
        if (converterStation1 != null) {
            modifyConverterStation(network, converterStation1, "1", subReportNode);
        }
        if (converterStation2 != null) {
            modifyConverterStation(network, converterStation2, "2", subReportNode);
        }

    }

    private void modifyConverterStation(@Nonnull Network network,
                                        @Nonnull LccConverterStationModification lccConverterStationModification,
                                        String logFieldName,
                                        ReportNode subReportNode) {

        String errorMessage = "Lcc converter station '" + lccConverterStationModification.getEquipmentId() + "' : ";
        LccConverterStation converterStation = network.getLccConverterStation(lccConverterStationModification.getEquipmentId());
        if (!lccConverterStationModification.hasModifications() || converterStation == null) {
            return;
        }

        ReportNode converterStationReportNode = subReportNode.newReportNode()
            .withMessageTemplate("network.modification.lccConverterStationModified")
            .withUntypedValue("fieldName", logFieldName)
            .withUntypedValue("id", lccConverterStationModification.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        // Characteristics
        List<ReportNode> characteristicsReports = new ArrayList<>();

        if (lccConverterStationModification.getEquipmentName() != null) {
            characteristicsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setName,
                () -> converterStation.getOptionalName().orElse(NO_VALUE), lccConverterStationModification.getEquipmentName(), "Equipment name"));
        }

        if (lccConverterStationModification.getLossFactor() != null) {
            ModificationUtils.checkIsPercentage(errorMessage, lccConverterStationModification.getLossFactor().getValue(), NetworkModificationException.Type.MODIFY_LCC_ERROR, "Loss factor");
            characteristicsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setLossFactor,
                converterStation::getLossFactor, lccConverterStationModification.getLossFactor(), "Loss factor"));
        }

        if (lccConverterStationModification.getPowerFactor() != null) {
            ModificationUtils.checkIsInInterval(errorMessage, lccConverterStationModification.getPowerFactor().getValue(), new Pair<>(0.f, 1.f), NetworkModificationException.Type.MODIFY_LCC_ERROR,
                    "Power factor");
            characteristicsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setPowerFactor,
                converterStation::getPowerFactor, lccConverterStationModification.getPowerFactor(), "Power factor"));
        }

        ModificationUtils.getInstance().reportModifications(converterStationReportNode, characteristicsReports,
                "network.modification.Characteristics");

        if (lccConverterStationModification.getShuntCompensatorsOnSide() != null && !lccConverterStationModification.getShuntCompensatorsOnSide().isEmpty()) {
            ReportNode shuntCompensatorReportNode = converterStationReportNode.newReportNode()
                .withMessageTemplate("network.modification.converterStationFilters")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
            modifyShuntCompensatorsOnSide(network, converterStation.getTerminal().getVoltageLevel(),
                lccConverterStationModification, shuntCompensatorReportNode);
        }
    }

    private void modifyShuntCompensatorsOnSide(Network network, VoltageLevel voltageLevel,
                                              @Nonnull LccConverterStationModification lccConverterStationModification, ReportNode reportNode) {

        List<LccShuntCompensatorModificationInfos> shuntCompensatorsOnSide = lccConverterStationModification.getShuntCompensatorsOnSide();

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
