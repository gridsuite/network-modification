package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.LccConverterStation;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.dto.LccModificationInfos;
import org.gridsuite.modification.dto.LccShuntCompensatorModificationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.gridsuite.modification.modifications.LccCreation.*;

public class LccModification extends AbstractModification {

    public static final String NO_VALUE = "No value";

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

        //Name
        if (modificationInfos.getEquipmentName() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setName,
                () -> hvdcLine.getOptionalName().orElse(NO_VALUE),
                modificationInfos.getEquipmentName(), "Name"));
        }

        // Nominal Voltage
        if (modificationInfos.getNominalV() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setNominalV,
                hvdcLine::getNominalV, modificationInfos.getNominalV(), "DC nominal voltage"));
        }

        // DC resistance
        if (modificationInfos.getR() != null) {
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
        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationInfos.getProperties(), "HvdcLineProperties");

        if (!characteristicsReportsContainer.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReportsContainer, LCC_CHARACTERISTICS, CHARACTERISTICS);
        }

    }

    private void modifyLcc(@Nonnull Network network, @Nonnull HvdcLine hvdcLine, LccModificationInfos modificationInfos, ReportNode subReportNode) {
        modificationInfos.createSubReportNode(subReportNode);

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

        LccConverterStation converterStation = network.getLccConverterStation(converterStationModificationInfos.getEquipmentId());
        if (!converterStationModificationInfos.hasModifications() || converterStation == null) {
            return;
        }

        ReportNode converterStationReportNode = subReportNode.newReportNode()
            .withMessageTemplate("Converter Station", "Converter station ${id} modified")
            .withUntypedValue("id", converterStationModificationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        // Characteristic
        List<ReportNode> characteristicReports = new ArrayList<>();

        if (converterStationModificationInfos.getEquipmentName() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setName,
                () -> converterStation.getOptionalName().orElse(NO_VALUE), converterStationModificationInfos.getEquipmentName(), "EquipmentName"));
        }

        if (converterStationModificationInfos.getLossFactor() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setLossFactor,
                converterStation::getLossFactor, converterStationModificationInfos.getLossFactor(), "LossFactor"));
        }

        if (converterStationModificationInfos.getPowerFactor() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setPowerFactor,
                converterStation::getPowerFactor, converterStationModificationInfos.getPowerFactor(), "PowerFactor"));
        }

        ModificationUtils.getInstance().reportModifications(converterStationReportNode, characteristicReports,
            "Characteristics", "Characteristics");

        modifyShuntCompensatorOnSide(network, converterStation.getTerminal().getVoltageLevel(),
            converterStationModificationInfos, subReportNode);
    }

    private void modifyShuntCompensatorOnSide(Network network, VoltageLevel voltageLevel,
                                              @Nonnull LccConverterStationModificationInfos converterStationInfos, ReportNode reportNode) {

        List<LccShuntCompensatorModificationInfos> shuntCompensatorOnSide = converterStationInfos.getShuntCompensatorsOnSide();

        Optional.ofNullable(shuntCompensatorOnSide).ifPresent(shuntCompensators ->
            shuntCompensators.forEach(infos -> {
                ShuntCompensator shuntCompensator = network.getShuntCompensator(infos.getId());
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
            }));
    }

}
