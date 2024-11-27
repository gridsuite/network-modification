/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LccConverterStationCreationInfos;
import org.gridsuite.modification.dto.LccCreationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_LCC_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.HVDC_LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.utils.ModificationUtils.createInjectionInNodeBreaker;
import static org.gridsuite.modification.utils.ModificationUtils.reportInjectionCreationConnectivity;

/**
 * @author Rehili Ghazwa <ghazwa.rehili at rte-france.com>
 */

public class LccCreation extends AbstractModification {
    public static final String LCC_CHARACTERISTICS = "lccCharacteristics";
    public static final String FILTERS = "Filters";

    public static final String LCC_SETPOINTS = "LccSetPoints";

    private final LccCreationInfos modificationInfos;

    public LccCreation(LccCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getHvdcLine(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }
        checkLccConverterStation(network, modificationInfos.getConverterStation1());
        checkLccConverterStation(network, modificationInfos.getConverterStation2());
    }

    private void checkLccConverterStation(Network network, LccConverterStationCreationInfos converterStation) {
        if (converterStation == null) {
            throw new NetworkModificationException(CREATE_LCC_ERROR, modificationInfos.getEquipmentId() + "Missing required converter station");
        }
        // check connectivity
        ModificationUtils.getInstance().controlConnectivity(network,
                converterStation.getVoltageLevelId(),
                converterStation.getBusOrBusbarSectionId(),
                converterStation.getConnectionPosition());
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        LccConverterStation converterStation1 = createConverterStation(network, modificationInfos.getConverterStation1(),
                subReportNode, "Lcc Converter station 1");
        LccConverterStation converterStation2 = createConverterStation(network, modificationInfos.getConverterStation2(),
                subReportNode, "Lcc Converter station 2");

        HvdcLine hvdcLine = network.newHvdcLine()
                .setId(modificationInfos.getEquipmentId())
                .setName(modificationInfos.getEquipmentName())
                .setNominalV(modificationInfos.getNominalV())
                .setR(modificationInfos.getR())
                .setMaxP(modificationInfos.getMaxP())
                .setActivePowerSetpoint(modificationInfos.getActivePowerSetpoint())
                .setConvertersMode(modificationInfos.getConvertersMode())
                .setConverterStationId1(converterStation1 != null ? converterStation1.getId() : null)
                .setConverterStationId2(converterStation2 != null ? converterStation2.getId() : null)
                .add();

        reportHvdcLineInfos(subReportNode);
        subReportNode.newReportNode()
                .withMessageTemplate("lccCreated", "New lcc with id=${id} created")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        if (modificationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, modificationInfos.getEquipmentName(), "Name");
        }
        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationInfos.getProperties(), "LCC_Properties");
    }

    @Override
    public String getName() {
        return "LCC_Creation";
    }

    private void reportHvdcLineInfos(ReportNode subReportNode) {
        List<ReportNode> characteristicsReport = new ArrayList<>();
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getNominalV(), "DC nominal voltage"));
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getR(), "DC resistance"));
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getMaxP(), "Pmax"));
        ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReport, LCC_CHARACTERISTICS, CHARACTERISTICS);
        List<ReportNode> setPointsReports = new ArrayList<>();
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getConvertersMode(), "Converters mode"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getActivePowerSetpoint(), "Active power"));
        ModificationUtils.getInstance().reportModifications(subReportNode, setPointsReports, LCC_SETPOINTS, SETPOINTS);
    }

    private LccConverterStation createConverterStation(Network network,
                                                       LccConverterStationCreationInfos lccConverterStationCreationInfos,
                                                       ReportNode subReportNode,
                                                       String logFieldName) {
        ReportNode converterStationReporter = subReportNode.newReportNode()
            .withMessageTemplate("converterStationCreated", "${fieldName} with id=${id} created")
            .withUntypedValue("fieldName", logFieldName)
            .withUntypedValue("id", lccConverterStationCreationInfos.getEquipmentId())
            .add();
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, lccConverterStationCreationInfos.getVoltageLevelId());
        return voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER ?
                createConverterStationInNodeBreaker(network, voltageLevel, lccConverterStationCreationInfos, converterStationReporter) :
                createConverterStationInBusBreaker(voltageLevel, lccConverterStationCreationInfos, converterStationReporter);
    }

    private static void createShuntCompensatorInNodeBreaker(VoltageLevel voltageLevel, LccConverterStationCreationInfos.ShuntCompensatorInfos shuntCompensatorInfos) {
        ShuntCompensatorAdder shuntAdder = voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorInfos.getShuntCompensatorId())
                .setName(shuntCompensatorInfos.getShuntCompensatorName())
                .setSectionCount(1);
        shuntAdder.newLinearModel()
                .setBPerSection((shuntCompensatorInfos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2))
                .setMaximumSectionCount(1)
                .add();
    }

    private static void createShuntCompensatorInBusBreaker(VoltageLevel voltageLevel, Bus bus, LccConverterStationCreationInfos.ShuntCompensatorInfos shuntCompensatorInfos) {
        voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorInfos.getShuntCompensatorId())
                .setName(shuntCompensatorInfos.getShuntCompensatorName())
                .setSectionCount(1)
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .newLinearModel()
                .setBPerSection((shuntCompensatorInfos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2))
                .setMaximumSectionCount(1)
                .add()
                .add();
    }

    private LccConverterStation createConverterStationInNodeBreaker(Network network,
                                                                    VoltageLevel voltageLevel,
                                                                    LccConverterStationCreationInfos lccConverterStationCreationInfos,
                                                                    ReportNode subReportNode) {
        LccConverterStationAdder converterStationAdder = voltageLevel.newLccConverterStation()
                .setId(lccConverterStationCreationInfos.getEquipmentId())
                .setName(lccConverterStationCreationInfos.getEquipmentName());
        if (lccConverterStationCreationInfos.getLossFactor() != null) {
            converterStationAdder.setLossFactor(lccConverterStationCreationInfos.getLossFactor());
        }
        if (lccConverterStationCreationInfos.getPowerFactor() != null) {
            converterStationAdder.setPowerFactor(lccConverterStationCreationInfos.getPowerFactor());
        }
        lccConverterStationCreationInfos.getMcsOnSide()
                .forEach(mcsOnSide -> createShuntCompensatorInNodeBreaker(voltageLevel, mcsOnSide));

        createInjectionInNodeBreaker(voltageLevel, lccConverterStationCreationInfos, network, converterStationAdder, subReportNode);
        addExtensionsAndReports(lccConverterStationCreationInfos, subReportNode);
        return ModificationUtils.getInstance().getLccConverterStation(network,
                lccConverterStationCreationInfos.getEquipmentId());
    }

    private LccConverterStation createConverterStationInBusBreaker(VoltageLevel voltageLevel,
                                                                   LccConverterStationCreationInfos lccConverterStationCreationInfos,
                                                                   ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, lccConverterStationCreationInfos.getBusOrBusbarSectionId());
        LccConverterStation lccConverterStation = voltageLevel.newLccConverterStation()
                .setId(lccConverterStationCreationInfos.getEquipmentId())
                .setName(lccConverterStationCreationInfos.getEquipmentName())
                .setBus(bus.getId())
                .setLossFactor(lccConverterStationCreationInfos.getLossFactor())
                .setPowerFactor(lccConverterStationCreationInfos.getPowerFactor())
                .add();
        lccConverterStationCreationInfos.getMcsOnSide()
                .forEach(mcsOnSide -> createShuntCompensatorInBusBreaker(voltageLevel, bus, mcsOnSide));
        addExtensionsAndReports(lccConverterStationCreationInfos, subReportNode);

        return lccConverterStation;
    }

    private void addExtensionsAndReports(LccConverterStationCreationInfos lccConverterStationCreationInfos,
                                         ReportNode subReporter) {
        reportInjectionCreationConnectivity(lccConverterStationCreationInfos, subReporter);
        ModificationUtils.getInstance().reportModifications(subReporter,
                List.of(ModificationUtils.getInstance().buildCreationReport(lccConverterStationCreationInfos.getLossFactor(), "Loss Factor"),
                        ModificationUtils.getInstance().buildCreationReport(lccConverterStationCreationInfos.getPowerFactor(), "Power Factor")),
                "converterStationCharacteristics",
                CHARACTERISTICS);

        List<ReportNode> filters = lccConverterStationCreationInfos.getMcsOnSide().stream()
                .flatMap(mcsOnSide -> Stream.of(
                        ModificationUtils.getInstance().buildCreationReport(mcsOnSide.getShuntCompensatorId(), "Shunt Compensator ID"),
                        ModificationUtils.getInstance().buildCreationReport(mcsOnSide.getShuntCompensatorName(), "Shunt Compensator Name"),
                        ModificationUtils.getInstance().buildCreationReport(mcsOnSide.getMaxQAtNominalV(), "Q max at nominal voltage"),
                        ModificationUtils.getInstance().buildCreationReport(mcsOnSide.getConnectedToHvdc(), "Connected To Hvdc")
                ))
                .toList();
        ModificationUtils.getInstance().reportModifications(subReporter, filters, "converterStationFilters", FILTERS);
    }
}
