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
import lombok.*;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LccShuntCompensatorInfos;
import org.gridsuite.modification.modifications.data.LccConverterStationCreation;
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

@Getter
@Setter
public class LccCreation extends AbstractEquipmentCreation {
    public static final String EQUIPMENT_CONNECTED_TO_HVDC = "network.modification.equipmentConnectedToHvdc";
    public static final String EQUIPMENT_NOT_CONNECTED_TO_HVDC = "network.modification.equipmentNotConnectedToHvdc";

    private Double nominalV;
    private Double r;
    private Double maxP;
    private HvdcLine.ConvertersMode convertersMode;
    private Double activePowerSetpoint;
    private LccConverterStationCreation converterStation1;
    private LccConverterStationCreation converterStation2;

    @Builder
    public LccCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName, Double nominalV,
                       Double r, Double maxP, HvdcLine.ConvertersMode convertersMode, Double activePowerSetpoint,
                       LccConverterStationCreation converterStation1,
                       LccConverterStationCreation converterStation2) {
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
    public void check(Network network) throws NetworkModificationException {
        if (network.getHvdcLine(equipmentId) != null) {
            throw new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, equipmentId);
        }
        checkLccConverterStation(network, converterStation1);
        checkLccConverterStation(network, converterStation2);
    }

    private void checkLccConverterStation(Network network, LccConverterStationCreation converterStation) {
        if (converterStation == null) {
            throw new NetworkModificationException(CREATE_LCC_ERROR, equipmentId + "Missing required converter station");
        }
        if (converterStation.getPowerFactor() > 1) {
            throw new NetworkModificationException(CREATE_LCC_ERROR, "Loss factor should be less or equal to 1");
        }
        // check connectivity
        ModificationUtils.getInstance().controlConnectivity(network,
                converterStation.getVoltageLevelId(),
                converterStation.getBusOrBusbarSectionId()
        );
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        LccConverterStation createdConverterStation1 = createConverterStation(network, converterStation1, subReportNode);
        LccConverterStation createdConverterStation2 = createConverterStation(network, converterStation2, subReportNode);
        if (!converterStation1.isTerminalConnected()) {
            network.getLccConverterStation(converterStation1.getEquipmentId()).getTerminal().disconnect();
        }
        if (!converterStation2.isTerminalConnected()) {
            network.getLccConverterStation(converterStation2.getEquipmentId()).getTerminal().disconnect();
        }
        HvdcLine hvdcLine = network.newHvdcLine()
                .setId(equipmentId)
                .setName(equipmentName)
                .setNominalV(nominalV)
                .setR(r)
                .setMaxP(maxP)
                .setActivePowerSetpoint(activePowerSetpoint)
                .setConvertersMode(convertersMode)
                .setConverterStationId1(createdConverterStation1 != null ? createdConverterStation1.getId() : null)
                .setConverterStationId2(createdConverterStation2 != null ? createdConverterStation2.getId() : null)
                .add();

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.lccCreated")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        reportHvdcLineInfos(hvdcLine, subReportNode);
        addReportConverterStationLcc(converterStation1, "1", subReportNode);
        addReportConverterStationLcc(converterStation2, "2", subReportNode);
    }

    @Override
    public String getName() {
        return "LCC_Creation";
    }

    private LccConverterStation createConverterStation(Network network,
                                                       LccConverterStationCreation lccConverterStationCreation,
                                                       ReportNode subReportNode) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, lccConverterStationCreation.getVoltageLevelId());
        return voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER ?
                createConverterStationInNodeBreaker(network, voltageLevel, lccConverterStationCreation, subReportNode) :
                createConverterStationInBusBreaker(network, voltageLevel, lccConverterStationCreation, subReportNode);
    }

    private ShuntCompensatorAdder createShuntCompensatorInNodeBreaker(VoltageLevel voltageLevel, LccShuntCompensatorInfos shuntCompensatorInfos) {
        return voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorInfos.getId())
                .setName(shuntCompensatorInfos.getName())
                .setSectionCount(1)
                .newLinearModel()
                .setBPerSection((shuntCompensatorInfos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2))
                .setMaximumSectionCount(1)
                .add();
    }

    private void createShuntCompensatorInBusBreaker(VoltageLevel voltageLevel, Bus bus, LccShuntCompensatorInfos shuntCompensatorInfos) {
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
                    .withMessageTemplate(EQUIPMENT_CONNECTED_TO_HVDC)
                    .withUntypedValue("id", injection.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        } else {
            injection.getTerminal().disconnect();
            subReportNode.newReportNode()
                    .withMessageTemplate(EQUIPMENT_NOT_CONNECTED_TO_HVDC)
                    .withUntypedValue("id", injection.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }

    private void createShuntCompensatorOnSideInNodeBreaker(VoltageLevel voltageLevel, Network network,
                                                           LccConverterStationCreation lccConverterStationCreation,
                                                           InjectionAdder<?, ?> injectionAdder,
                                                           LccShuntCompensatorInfos shuntCompensatorOnSide,
                                                           ReportNode subReportNode) {
        int position = ModificationUtils.getInstance().getPosition(lccConverterStationCreation.getBusOrBusbarSectionId(), network, voltageLevel);
        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBusOrBusbarSectionId(lccConverterStationCreation.getBusOrBusbarSectionId())
                .withInjectionDirection(lccConverterStationCreation.getConnectionDirection())
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
                                                                    LccConverterStationCreation lccConverterStationCreation,
                                                                    ReportNode subReportNode) {
        LccConverterStationAdder converterStationAdder = voltageLevel.newLccConverterStation()
                .setId(lccConverterStationCreation.getEquipmentId())
                .setName(lccConverterStationCreation.getEquipmentName());
        if (lccConverterStationCreation.getLossFactor() != null) {
            converterStationAdder.setLossFactor(lccConverterStationCreation.getLossFactor());
        }
        if (lccConverterStationCreation.getPowerFactor() != null) {
            converterStationAdder.setPowerFactor(lccConverterStationCreation.getPowerFactor());
        }
        createInjectionInNodeBreaker(voltageLevel, lccConverterStationCreation.getEquipmentId(), lccConverterStationCreation.getBusOrBusbarSectionId(),
                lccConverterStationCreation.getConnectionName(), lccConverterStationCreation.getConnectionDirection(), lccConverterStationCreation.getConnectionPosition(),
                network, converterStationAdder, subReportNode);
        Optional.ofNullable(lccConverterStationCreation.getShuntCompensatorsOnSide())
                .ifPresent(shuntCompensators ->
                        shuntCompensators.forEach(shuntCompensatorOnSide -> {
                            ShuntCompensatorAdder shuntCompensatorAdder = createShuntCompensatorInNodeBreaker(voltageLevel, shuntCompensatorOnSide);
                            createShuntCompensatorOnSideInNodeBreaker(voltageLevel, network, lccConverterStationCreation,
                                    shuntCompensatorAdder, shuntCompensatorOnSide, subReportNode);
                            shuntCompensatorConnectedToHvdc(shuntCompensatorOnSide.getConnectedToHvdc(),
                                    network.getShuntCompensator(shuntCompensatorOnSide.getId()), subReportNode);
                        }));
        return ModificationUtils.getInstance().getLccConverterStation(network,
                lccConverterStationCreation.getEquipmentId());
    }

    private LccConverterStation createConverterStationInBusBreaker(Network network, VoltageLevel voltageLevel,
                                                                   LccConverterStationCreation lccConverterStationCreation,
                                                                   ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, lccConverterStationCreation.getBusOrBusbarSectionId());
        LccConverterStation lccConverterStation = voltageLevel.newLccConverterStation()
                .setId(lccConverterStationCreation.getEquipmentId())
                .setName(lccConverterStationCreation.getEquipmentName())
                .setBus(bus.getId())
                .setLossFactor(lccConverterStationCreation.getLossFactor())
                .setPowerFactor(lccConverterStationCreation.getPowerFactor())
                .add();

        Optional.ofNullable(lccConverterStationCreation.getShuntCompensatorsOnSide())
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
        if (equipmentName != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, equipmentName, "Name");
        }
        List<ReportNode> characteristicsReport = new ArrayList<>();
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(nominalV, "DC nominal voltage"));
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(r, "DC resistance"));
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(maxP, "Pmax"));
        ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReport, "network.modification.lccCharacteristics");
        List<ReportNode> setPointsReports = new ArrayList<>();
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(convertersMode, "Converters mode"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(activePowerSetpoint, "Active power"));
        ModificationUtils.getInstance().reportModifications(subReportNode, setPointsReports, "network.modification.LccSetPoints");
        PropertiesUtils.applyProperties(hvdcLine, subReportNode, properties, "network.modification.LCC_Properties");
    }

    private void addReportConverterStationLcc(LccConverterStationCreation lccConverterStationCreation,
                                              String logFieldName, ReportNode subReporter) {
        ReportNode reportConverterStationNode = subReporter.newReportNode()
                .withMessageTemplate("network.modification.lccConverterStationCreated")
                .withUntypedValue("fieldName", logFieldName)
                .withUntypedValue("id", lccConverterStationCreation.getEquipmentId()).add();

        List<ReportNode> characteristicsReport = List.of(ModificationUtils.getInstance().buildCreationReport(lccConverterStationCreation.getLossFactor(), "Loss Factor"),
                ModificationUtils.getInstance().buildCreationReport(lccConverterStationCreation.getPowerFactor(), "Power Factor"));
        List<ReportNode> shuntCompensatorsOnSide = Optional.ofNullable(lccConverterStationCreation.getShuntCompensatorsOnSide())
                .orElse(List.of())
                .stream()
                .flatMap(shuntCompensatorOnSide -> Stream.of(
                        ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getId(), "Shunt Compensator ID"),
                        ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getName(), "Shunt Compensator Name"),
                        ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getMaxQAtNominalV(), "Q max at nominal voltage"),
                        ModificationUtils.getInstance().buildCreationReport(shuntCompensatorOnSide.getConnectedToHvdc(), "Connected To Hvdc")
                ))
                .toList();

        ModificationUtils.getInstance().reportModifications(reportConverterStationNode, characteristicsReport, "network.modification.converterStationCharacteristics");
        reportInjectionCreationConnectivity(lccConverterStationCreation.getVoltageLevelId(), lccConverterStationCreation.getBusOrBusbarSectionId(),
                lccConverterStationCreation.getConnectionName(), lccConverterStationCreation.getConnectionDirection(), lccConverterStationCreation.getConnectionPosition(),
                lccConverterStationCreation.isTerminalConnected(), lccConverterStationCreation.getEquipmentId(), reportConverterStationNode);
        ModificationUtils.getInstance().reportModifications(reportConverterStationNode, shuntCompensatorsOnSide, "network.modification.converterStationFilters");

    }
}
