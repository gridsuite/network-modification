/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.CreateFeederBay;
import com.powsybl.iidm.modification.topology.CreateFeederBayBuilder;
import com.powsybl.iidm.network.*;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LccConverterStationCreationInfos;
import org.gridsuite.modification.dto.LccCreationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
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

    public static final String EQUIPMENT_CONNECTED_TO_HVDC = "equipmentConnectedToHvdc";
    public static final String EQUIPMENT_NOT_CONNECTED_TO_HVDC = "equipmentNotConnectedToHvdc";

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
        if (converterStation.getPowerFactor() > 1) {
            throw new NetworkModificationException(CREATE_LCC_ERROR, "Loss factor should be less or equal to 1");
        }
        // check connectivity
        ModificationUtils.getInstance().controlConnectivity(network,
                converterStation.getVoltageLevelId(),
                converterStation.getBusOrBusbarSectionId(),
                converterStation.getConnectionPosition());
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        LccConverterStation converterStation1 = createConverterStation(network, modificationInfos.getConverterStation1(), subReportNode);
        LccConverterStation converterStation2 = createConverterStation(network, modificationInfos.getConverterStation2(), subReportNode);

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

        subReportNode.newReportNode()
                .withMessageTemplate("lccCreated", "New lcc with id=${id} created")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        reportHvdcLineInfos(hvdcLine, subReportNode);
        addReportConverterStationLcc(modificationInfos.getConverterStation1(), "Lcc Converter station 1", subReportNode);
        addReportConverterStationLcc(modificationInfos.getConverterStation2(), "Lcc Converter station 2", subReportNode);
    }

    @Override
    public String getName() {
        return "LCC_Creation";
    }

    private LccConverterStation createConverterStation(Network network,
                                                       LccConverterStationCreationInfos lccConverterStationCreationInfos,
                                                       ReportNode subReportNode) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, lccConverterStationCreationInfos.getVoltageLevelId());
        return voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER ?
                createConverterStationInNodeBreaker(network, voltageLevel, lccConverterStationCreationInfos, subReportNode) :
                createConverterStationInBusBreaker(network, voltageLevel, lccConverterStationCreationInfos, subReportNode);
    }

    private ShuntCompensatorAdder createShuntCompensatorInNodeBreaker(VoltageLevel voltageLevel, LccConverterStationCreationInfos.ShuntCompensatorInfos shuntCompensatorInfos) {
        return voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorInfos.getId())
                .setName(shuntCompensatorInfos.getName())
                .setSectionCount(1)
                .newLinearModel()
                .setBPerSection((shuntCompensatorInfos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2))
                .setMaximumSectionCount(1)
                .add();
    }

    private void createShuntCompensatorInBusBreaker(VoltageLevel voltageLevel, Bus bus, LccConverterStationCreationInfos.ShuntCompensatorInfos shuntCompensatorInfos) {
        voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorInfos.getId())
                .setName(shuntCompensatorInfos.getName())
                .setSectionCount(1)
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .newLinearModel()
                .setBPerSection((shuntCompensatorInfos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2))
                .setMaximumSectionCount(1)
                .add()
                .add();
    }

    public void shuntCompensatorConnectedToHvdc(Boolean connectedToHvdc, Injection<?> injection, ReportNode subReportNode) {
        if (Boolean.TRUE.equals(connectedToHvdc)) {
            injection.getTerminal().connect();
            subReportNode.newReportNode()
                    .withMessageTemplate(EQUIPMENT_CONNECTED_TO_HVDC, "Equipment with id=${id} is connected to Hvdc")
                    .withUntypedValue("id", injection.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        } else {
            injection.getTerminal().disconnect();
            subReportNode.newReportNode()
                    .withMessageTemplate(EQUIPMENT_NOT_CONNECTED_TO_HVDC, "Equipment with id=${id} is not connected to Hvdc")
                    .withUntypedValue("id", injection.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }

    private void createShuntCompensatorOnSideInNodeBreaker(VoltageLevel voltageLevel, Network network,
                                                           LccConverterStationCreationInfos lccConverterStationCreationInfos,
                                                           InjectionAdder<?, ?> injectionAdder,
                                                           LccConverterStationCreationInfos.ShuntCompensatorInfos shuntCompensatorOnSide,
                                                           ReportNode subReportNode) {
        int position = ModificationUtils.getInstance().getPosition(lccConverterStationCreationInfos.getBusOrBusbarSectionId(), network, voltageLevel);
        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBusOrBusbarSectionId(lccConverterStationCreationInfos.getBusOrBusbarSectionId())
                .withInjectionDirection(lccConverterStationCreationInfos.getConnectionDirection())
                .withInjectionFeederName(StringUtils.isBlank(shuntCompensatorOnSide.getName())
                        ? shuntCompensatorOnSide.getId()
                        : shuntCompensatorOnSide.getName())
                .withInjectionPositionOrder(position)
                .withInjectionAdder(injectionAdder)
                .build();
        algo.apply(network, true, subReportNode);
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
        createInjectionInNodeBreaker(voltageLevel, lccConverterStationCreationInfos, network, converterStationAdder, subReportNode);
        Optional.ofNullable(lccConverterStationCreationInfos.getShuntCompensatorsOnSide())
                .ifPresent(shuntCompensators ->
                        shuntCompensators.forEach(shuntCompensatorOnSide -> {
                            ShuntCompensatorAdder shuntCompensatorAdder = createShuntCompensatorInNodeBreaker(voltageLevel, shuntCompensatorOnSide);
                            createShuntCompensatorOnSideInNodeBreaker(voltageLevel, network, lccConverterStationCreationInfos,
                                    shuntCompensatorAdder, shuntCompensatorOnSide, subReportNode);
                            shuntCompensatorConnectedToHvdc(shuntCompensatorOnSide.getConnectedToHvdc(),
                                    network.getShuntCompensator(shuntCompensatorOnSide.getId()), subReportNode);
                        }));
        return ModificationUtils.getInstance().getLccConverterStation(network,
                lccConverterStationCreationInfos.getEquipmentId());
    }

    private LccConverterStation createConverterStationInBusBreaker(Network network, VoltageLevel voltageLevel,
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

        Optional.ofNullable(lccConverterStationCreationInfos.getShuntCompensatorsOnSide())
                .ifPresent(shuntCompensators -> shuntCompensators.forEach(shuntCompensatorOnSide -> {
                    createShuntCompensatorInBusBreaker(voltageLevel, bus, shuntCompensatorOnSide);
                    shuntCompensatorConnectedToHvdc(
                            shuntCompensatorOnSide.getConnectedToHvdc(),
                            network.getShuntCompensator(shuntCompensatorOnSide.getId()),
                            subReportNode);
                }));
        return lccConverterStation;
    }

    private void reportHvdcLineInfos(HvdcLine hvdcLine, ReportNode subReportNode) {
        if (modificationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, modificationInfos.getEquipmentName(), "Name");
        }
        List<ReportNode> characteristicsReport = new ArrayList<>();
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getNominalV(), "DC nominal voltage"));
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getR(), "DC resistance"));
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getMaxP(), "Pmax"));
        ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReport, LCC_CHARACTERISTICS, CHARACTERISTICS);
        List<ReportNode> setPointsReports = new ArrayList<>();
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getConvertersMode(), "Converters mode"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getActivePowerSetpoint(), "Active power"));
        ModificationUtils.getInstance().reportModifications(subReportNode, setPointsReports, LCC_SETPOINTS, SETPOINTS);
        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationInfos.getProperties(), "LCC_Properties");
    }

    private void addReportConverterStationLcc(LccConverterStationCreationInfos lccConverterStationCreationInfos,
                                              String logFieldName, ReportNode subReporter) {
        ReportNode reportConverterStationNode = subReporter.newReportNode()
                .withMessageTemplate("converterStationCreated", "${fieldName} with id=${id} created")
                .withUntypedValue("fieldName", logFieldName)
                .withUntypedValue("id", lccConverterStationCreationInfos.getEquipmentId()).add();

        List<ReportNode> characteristicsReport = List.of(ModificationUtils.getInstance().buildCreationReport(lccConverterStationCreationInfos.getLossFactor(), "Loss Factor"),
                        ModificationUtils.getInstance().buildCreationReport(lccConverterStationCreationInfos.getPowerFactor(), "Power Factor"));
        List<ReportNode> shuntCompensatorsOnSide = Optional.ofNullable(lccConverterStationCreationInfos.getShuntCompensatorsOnSide())
                .orElse(List.of())
                .stream()
                .flatMap(shuntCompensatorOnSide -> Stream.of(
                        ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getId(), "Shunt Compensator ID"),
                        ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getName(), "Shunt Compensator Name"),
                        ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getMaxQAtNominalV(), "Q max at nominal voltage"),
                        ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getConnectedToHvdc(), "Connected To Hvdc")
                ))
                .toList();

        ModificationUtils.getInstance().reportModifications(reportConverterStationNode, characteristicsReport, "converterStationCharacteristics", CHARACTERISTICS);
        reportInjectionCreationConnectivity(lccConverterStationCreationInfos, reportConverterStationNode);
        ModificationUtils.getInstance().reportModifications(reportConverterStationNode, shuntCompensatorsOnSide, "converterStationFilters", FILTERS);

    }
}
