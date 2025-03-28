package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.LccConverterStation;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ReactiveCapabilityCurve;
import com.powsybl.iidm.network.ReactiveCapabilityCurveAdder;
import com.powsybl.iidm.network.ReactiveLimitsKind;
import com.powsybl.iidm.network.VscConverterStation;
import org.gridsuite.modification.dto.ConverterStationModificationInfos;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.dto.LccModificationInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.utils.ModificationUtils;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.gridsuite.modification.modifications.LccCreation.FILTERS;
import static org.gridsuite.modification.modifications.LccCreation.LCC_CHARACTERISTICS;
import static org.gridsuite.modification.modifications.VscCreation.VSC_CHARACTERISTICS;

@SuppressWarnings("checkstyle:RegexpSingleline")
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
        modifyConverterStation(ModificationUtils.getInstance().getLccConverterStation(network,
            hvdcLine.getConverterStation1().getId()), modificationInfos.getConverterStation1(), subReportNode);
        modifyConverterStation(ModificationUtils.getInstance().getLccConverterStation(network,
            hvdcLine.getConverterStation2().getId()), modificationInfos.getConverterStation2(), subReportNode);

    }

    private void modifyConverterStation(LccConverterStation converterStation,
                                        LccConverterStationModificationInfos converterStationModificationInfos, ReportNode subReportNode) {

        if (converterStationModificationInfos == null || !converterStationModificationInfos.hasModifications()) {
            return;
        }

        ReportNode converterStationReportNode = subReportNode.newReportNode()
            .withMessageTemplate("Converter Station", "Converter station ${id} modified")
            .withUntypedValue("id", converterStation.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        // Characteristic
        List<ReportNode> characteristicReports = new ArrayList<>();

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

        List<ReportNode> shuntCompensatorsOnSide = Optional.ofNullable(converterStationModificationInfos.getShuntCompensatorsOnSide())
            .orElse(List.of())
            .stream()
            .flatMap(shuntCompensatorOnSide -> Stream.of(
                ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getId(), "Shunt Compensator ID"),
                ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getName(), "Shunt Compensator Name"),
                ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getMaxQAtNominalV(), "Q max at nominal voltage"),
                ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getConnectedToHvdc(), "Connected To Hvdc")
            ))
            .toList();

        ModificationUtils.getInstance().reportModifications(converterStationReportNode, shuntCompensatorsOnSide, "converterStationFilters", FILTERS);

    }

}
