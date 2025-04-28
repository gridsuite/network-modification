package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.LccConverterStation;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.dto.LccModificationInfos;
import org.gridsuite.modification.dto.LccShuntCompensatorModificationinfos;
import org.gridsuite.modification.utils.ModificationUtils;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.modifications.LccCreation.LCC_CHARACTERISTICS;

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
            modifyConverterStation(network.getLccConverterStation(modificationInfos.getConverterStation1().getEquipmentId()),
                modificationInfos.getConverterStation1(), subReportNode);
        }

        if (modificationInfos.getConverterStation2() != null) {
            modifyConverterStation(network.getLccConverterStation(modificationInfos.getConverterStation2().getEquipmentId()),
                modificationInfos.getConverterStation2(), subReportNode);

        }

    }

    private void modifyConverterStation(LccConverterStation converterStation,
                                        LccConverterStationModificationInfos converterStationModificationInfos, ReportNode subReportNode) {

        if (converterStation == null || converterStationModificationInfos == null || !converterStationModificationInfos.hasModifications()) {
            return;
        }

        ReportNode converterStationReportNode = subReportNode.newReportNode()
            .withMessageTemplate("Converter Station", "Converter station ${id} modified")
            .withUntypedValue("id", converterStation.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        // Characteristic
        List<ReportNode> characteristicReports = new ArrayList<>();

        if (converterStationModificationInfos.getEquipmentName() != null && converterStationModificationInfos.getEquipmentName().getValue() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setName,
                () -> converterStation.getOptionalName().orElse(NO_VALUE), converterStationModificationInfos.getEquipmentName(), "EquipmentName"));
        }

        if (converterStationModificationInfos.getLossFactor() != null && converterStationModificationInfos.getLossFactor().getValue() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setLossFactor,
                converterStation::getLossFactor, converterStationModificationInfos.getLossFactor(), "LossFactor"));
        }

        if (converterStationModificationInfos.getPowerFactor() != null && converterStationModificationInfos.getPowerFactor().getValue() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setPowerFactor,
                converterStation::getPowerFactor, converterStationModificationInfos.getPowerFactor(), "PowerFactor"));
        }

        if (!characteristicReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(converterStationReportNode,
                characteristicReports, "Characteristics", "Characteristics");
        }
    }

    private void modifyShuntCompensatorOnSide(VoltageLevel voltageLevel, ReportNode subReportNode,
                                              @Nullable List<LccShuntCompensatorModificationinfos> shuntCompensatorOnSide) {

/*        List<ReportNode> reportNodes = new ArrayList<>();
        Iterable<ShuntCompensator> shuntCompensators = voltageLevel.getShuntCompensatorStream();
        voltageLevel.
        Optional.ofNullable(shuntCompensatorOnSide).ifPresent(shuntCompensatorInfo ->
            // modify existing shunt compensator

            // Add new Shunt compensator
        );*/
    }

}
