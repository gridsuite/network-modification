package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.LccConverterStation;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.VoltageLevel;
import org.apache.commons.math3.util.Pair;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.dto.LccModificationInfos;
import org.gridsuite.modification.dto.LccShuntCompensatorModificationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class LccModification extends AbstractModification {

    public static final String NO_VALUE = "No value";
    public static final String LCC_CHARACTERISTICS = "lccCharacteristics";
    public static final String EQUIPMENT_CONNECTED_TO_HVDC = "equipmentConnectedToHvdc";
    public static final String EQUIPMENT_NOT_CONNECTED_TO_HVDC = "equipmentNotConnectedToHvdc";

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
            ModificationUtils.checkIsNotNegativeValue(errorMessage, modificationInfos.getNominalV().getValue(), NetworkModificationException.Type.MODIFY_LCC_ERROR, "Nominal voltage");
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setNominalV,
                hvdcLine::getNominalV, modificationInfos.getNominalV(), "DC nominal voltage"));
        }

        // DC resistance
        if (modificationInfos.getR() != null) {
            ModificationUtils.checkIsNotNegativeValue(errorMessage, modificationInfos.getR().getValue(), NetworkModificationException.Type.MODIFY_LCC_ERROR, "DC resistance");
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

        // Properties
        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationInfos.getProperties(), "HvdcLine Properties");

        if (!characteristicsReportsContainer.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReportsContainer, LCC_CHARACTERISTICS, CHARACTERISTICS);
        }

    }

    private void modifyLcc(@Nonnull Network network, @Nonnull HvdcLine hvdcLine, LccModificationInfos modificationInfos, ReportNode subReportNode) {

        // Modify characteristics
        modifyCharacteristics(hvdcLine, modificationInfos, subReportNode);

        // stations
        if (modificationInfos.getConverterStation1() != null) {
            modifyConverterStation(network, modificationInfos.getConverterStation1(), subReportNode);
        }

        if (modificationInfos.getConverterStation2() != null) {
            modifyConverterStation(network, modificationInfos.getConverterStation2(), subReportNode);

        }

    }

    private void modifyConverterStation(@Nonnull Network network,
                                        @Nonnull LccConverterStationModificationInfos converterStationModificationInfos,
                                        ReportNode subReportNode) {

        String errorMessage = "Lcc converter station '" + converterStationModificationInfos.getEquipmentId() + "' : ";
        LccConverterStation converterStation = network.getLccConverterStation(converterStationModificationInfos.getEquipmentId());
        if (!converterStationModificationInfos.hasModifications() || converterStation == null) {
            return;
        }

        ReportNode converterStationReportNode = subReportNode.newReportNode()
            .withMessageTemplate("Converter Station", "Converter station ${id} modified")
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
            ModificationUtils.checkIsPercentage(errorMessage, converterStationModificationInfos.getLossFactor().getValue(), NetworkModificationException.Type.MODIFY_LCC_ERROR, "Loss factor");
            characteristicsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setLossFactor,
                converterStation::getLossFactor, converterStationModificationInfos.getLossFactor(), "Loss factor"));
        }

        if (converterStationModificationInfos.getPowerFactor() != null) {
            ModificationUtils.checkIsInInterval(errorMessage, converterStationModificationInfos.getPowerFactor().getValue(), new Pair<>(0.f, 1.f), NetworkModificationException.Type.MODIFY_LCC_ERROR, "Power factor");
            characteristicsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setPowerFactor,
                converterStation::getPowerFactor, converterStationModificationInfos.getPowerFactor(), "Power factor"));
        }

        ModificationUtils.getInstance().reportModifications(converterStationReportNode, characteristicsReports,
            CHARACTERISTICS, CHARACTERISTICS);

        modifyShuntCompensatorsOnSide(network, converterStation.getTerminal().getVoltageLevel(),
            converterStationModificationInfos, subReportNode);
    }

    private void modifyShuntCompensatorsOnSide(Network network, VoltageLevel voltageLevel,
                                              @Nonnull LccConverterStationModificationInfos converterStationInfos, ReportNode reportNode) {

        List<LccShuntCompensatorModificationInfos> shuntCompensatorsOnSide = converterStationInfos.getShuntCompensatorsOnSide();

        Optional.ofNullable(shuntCompensatorsOnSide).ifPresent(shuntCompensators ->
            shuntCompensators.forEach(infos -> {
                ShuntCompensator shuntCompensator = network.getShuntCompensator(infos.getId());
                modifyShuntCompensator(voltageLevel, reportNode, infos, shuntCompensator);
            }));
    }

    private static void modifyShuntCompensator(VoltageLevel voltageLevel, ReportNode reportNode, LccShuntCompensatorModificationInfos infos, ShuntCompensator shuntCompensator) {
        if (shuntCompensator == null) {
            return;
        }

        if (infos.isDeletionMark()) {
            shuntCompensator.remove();
            return;
        }

        if (infos.getName() != null) {
            shuntCompensator.setName(infos.getName());
        }

        if (infos.getConnectedToHvdc() != null) {
            if (Boolean.TRUE.equals(infos.getConnectedToHvdc())) {
                shuntCompensator.getTerminal().connect();
                reportNode.newReportNode()
                    .withMessageTemplate(EQUIPMENT_CONNECTED_TO_HVDC, "Equipment with id=${id} is connected to Hvdc")
                    .withUntypedValue("id", shuntCompensator.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            } else {
                shuntCompensator.getTerminal().disconnect();
                reportNode.newReportNode()
                    .withMessageTemplate(EQUIPMENT_NOT_CONNECTED_TO_HVDC, "Equipment with id=${id} is disconnected from Hvdc")
                    .withUntypedValue("id", shuntCompensator.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            }
        }

        if (infos.getMaxQAtNominalV() != null) {
            ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
            model.setBPerSection((infos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2));
        }
    }

}
