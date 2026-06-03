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
import org.gridsuite.modification.model.LccConverterStationCreationModel;
import org.gridsuite.modification.model.LccCreationModel;
import org.gridsuite.modification.model.LccShuntCompensatorModel;
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
    public static final String EQUIPMENT_CONNECTED_TO_HVDC = "network.modification.equipmentConnectedToHvdc";
    public static final String EQUIPMENT_NOT_CONNECTED_TO_HVDC = "network.modification.equipmentNotConnectedToHvdc";

    private final LccCreationModel modificationModel;

    public LccCreation(LccCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getHvdcLine(modificationModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, modificationModel.getEquipmentId());
        }
        checkLccConverterStation(network, modificationModel.getConverterStation1());
        checkLccConverterStation(network, modificationModel.getConverterStation2());
    }

    private void checkLccConverterStation(Network network, LccConverterStationCreationModel converterStation) {
        if (converterStation == null) {
            throw new NetworkModificationException(CREATE_LCC_ERROR, modificationModel.getEquipmentId() + "Missing required converter station");
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
        LccConverterStation converterStation1 = createConverterStation(network, modificationModel.getConverterStation1(), subReportNode);
        LccConverterStation converterStation2 = createConverterStation(network, modificationModel.getConverterStation2(), subReportNode);
        if (!modificationModel.getConverterStation1().isTerminalConnected()) {
            network.getLccConverterStation(modificationModel.getConverterStation1().getEquipmentId()).getTerminal().disconnect();
        }
        if (!modificationModel.getConverterStation2().isTerminalConnected()) {
            network.getLccConverterStation(modificationModel.getConverterStation2().getEquipmentId()).getTerminal().disconnect();
        }
        HvdcLine hvdcLine = network.newHvdcLine()
                .setId(modificationModel.getEquipmentId())
                .setName(modificationModel.getEquipmentName())
                .setNominalV(modificationModel.getNominalV())
                .setR(modificationModel.getR())
                .setMaxP(modificationModel.getMaxP())
                .setActivePowerSetpoint(modificationModel.getActivePowerSetpoint())
                .setConvertersMode(modificationModel.getConvertersMode())
                .setConverterStationId1(converterStation1 != null ? converterStation1.getId() : null)
                .setConverterStationId2(converterStation2 != null ? converterStation2.getId() : null)
                .add();

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.lccCreated")
                .withUntypedValue("id", modificationModel.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        reportHvdcLineModel(hvdcLine, subReportNode);
        addReportConverterStationLcc(modificationModel.getConverterStation1(), "1", subReportNode);
        addReportConverterStationLcc(modificationModel.getConverterStation2(), "2", subReportNode);
    }

    @Override
    public String getName() {
        return "LCC_Creation";
    }

    private LccConverterStation createConverterStation(Network network,
                                                       LccConverterStationCreationModel lccConverterStationCreationModel,
                                                       ReportNode subReportNode) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, lccConverterStationCreationModel.getVoltageLevelId());
        return voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER ?
                createConverterStationInNodeBreaker(network, voltageLevel, lccConverterStationCreationModel, subReportNode) :
                createConverterStationInBusBreaker(network, voltageLevel, lccConverterStationCreationModel, subReportNode);
    }

    private ShuntCompensatorAdder createShuntCompensatorInNodeBreaker(VoltageLevel voltageLevel, LccShuntCompensatorModel shuntCompensatorModel) {
        return voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorModel.getId())
                .setName(shuntCompensatorModel.getName())
                .setSectionCount(1)
                .newLinearModel()
                .setBPerSection((shuntCompensatorModel.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2))
                .setMaximumSectionCount(1)
                .add();
    }

    private void createShuntCompensatorInBusBreaker(VoltageLevel voltageLevel, Bus bus, LccShuntCompensatorModel shuntCompensatorModel) {
        voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorModel.getId())
                .setName(shuntCompensatorModel.getName())
                .setSectionCount(1)
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .newLinearModel()
                .setBPerSection((shuntCompensatorModel.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2))
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
                                                           LccConverterStationCreationModel lccConverterStationCreationModel,
                                                           InjectionAdder<?, ?> injectionAdder,
                                                           LccShuntCompensatorModel shuntCompensatorOnSide,
                                                           ReportNode subReportNode) {
        int position = ModificationUtils.getInstance().getPosition(lccConverterStationCreationModel.getBusOrBusbarSectionId(), network, voltageLevel);
        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBusOrBusbarSectionId(lccConverterStationCreationModel.getBusOrBusbarSectionId())
                .withInjectionDirection(lccConverterStationCreationModel.getConnectionDirection())
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
                                                                    LccConverterStationCreationModel lccConverterStationCreationModel,
                                                                    ReportNode subReportNode) {
        LccConverterStationAdder converterStationAdder = voltageLevel.newLccConverterStation()
                .setId(lccConverterStationCreationModel.getEquipmentId())
                .setName(lccConverterStationCreationModel.getEquipmentName());
        if (lccConverterStationCreationModel.getLossFactor() != null) {
            converterStationAdder.setLossFactor(lccConverterStationCreationModel.getLossFactor());
        }
        if (lccConverterStationCreationModel.getPowerFactor() != null) {
            converterStationAdder.setPowerFactor(lccConverterStationCreationModel.getPowerFactor());
        }
        createInjectionInNodeBreaker(voltageLevel, lccConverterStationCreationModel, network, converterStationAdder, subReportNode);
        Optional.ofNullable(lccConverterStationCreationModel.getShuntCompensatorsOnSide())
                .ifPresent(shuntCompensators ->
                        shuntCompensators.forEach(shuntCompensatorOnSide -> {
                            ShuntCompensatorAdder shuntCompensatorAdder = createShuntCompensatorInNodeBreaker(voltageLevel, shuntCompensatorOnSide);
                            createShuntCompensatorOnSideInNodeBreaker(voltageLevel, network, lccConverterStationCreationModel,
                                    shuntCompensatorAdder, shuntCompensatorOnSide, subReportNode);
                            shuntCompensatorConnectedToHvdc(shuntCompensatorOnSide.getConnectedToHvdc(),
                                    network.getShuntCompensator(shuntCompensatorOnSide.getId()), subReportNode);
                        }));
        return ModificationUtils.getInstance().getLccConverterStation(network,
                lccConverterStationCreationModel.getEquipmentId());
    }

    private LccConverterStation createConverterStationInBusBreaker(Network network, VoltageLevel voltageLevel,
                                                                   LccConverterStationCreationModel lccConverterStationCreationModel,
                                                                   ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, lccConverterStationCreationModel.getBusOrBusbarSectionId());
        LccConverterStation lccConverterStation = voltageLevel.newLccConverterStation()
                .setId(lccConverterStationCreationModel.getEquipmentId())
                .setName(lccConverterStationCreationModel.getEquipmentName())
                .setBus(bus.getId())
                .setLossFactor(lccConverterStationCreationModel.getLossFactor())
                .setPowerFactor(lccConverterStationCreationModel.getPowerFactor())
                .add();

        Optional.ofNullable(lccConverterStationCreationModel.getShuntCompensatorsOnSide())
                .ifPresent(shuntCompensators -> shuntCompensators.forEach(shuntCompensatorOnSide -> {
                    createShuntCompensatorInBusBreaker(voltageLevel, bus, shuntCompensatorOnSide);
                    shuntCompensatorConnectedToHvdc(
                            shuntCompensatorOnSide.getConnectedToHvdc(),
                            network.getShuntCompensator(shuntCompensatorOnSide.getId()),
                            subReportNode);
                }));
        return lccConverterStation;
    }

    private void reportHvdcLineModel(HvdcLine hvdcLine, ReportNode subReportNode) {
        if (modificationModel.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, modificationModel.getEquipmentName(), "Name");
        }
        List<ReportNode> characteristicsReport = new ArrayList<>();
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getNominalV(), "DC nominal voltage"));
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getR(), "DC resistance"));
        characteristicsReport.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getMaxP(), "Pmax"));
        ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReport, "network.modification.lccCharacteristics");
        List<ReportNode> setPointsReports = new ArrayList<>();
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getConvertersMode(), "Converters mode"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getActivePowerSetpoint(), "Active power"));
        ModificationUtils.getInstance().reportModifications(subReportNode, setPointsReports, "network.modification.LccSetPoints");
        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationModel.getProperties(), "network.modification.LCC_Properties");
    }

    private void addReportConverterStationLcc(LccConverterStationCreationModel lccConverterStationCreationModel,
                                              String logFieldName, ReportNode subReporter) {
        ReportNode reportConverterStationNode = subReporter.newReportNode()
                .withMessageTemplate("network.modification.lccConverterStationCreated")
                .withUntypedValue("fieldName", logFieldName)
                .withUntypedValue("id", lccConverterStationCreationModel.getEquipmentId()).add();

        List<ReportNode> characteristicsReport = List.of(ModificationUtils.getInstance().buildCreationReport(lccConverterStationCreationModel.getLossFactor(), "Loss Factor"),
                ModificationUtils.getInstance().buildCreationReport(lccConverterStationCreationModel.getPowerFactor(), "Power Factor"));
        List<ReportNode> shuntCompensatorsOnSide = Optional.ofNullable(lccConverterStationCreationModel.getShuntCompensatorsOnSide())
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
        reportInjectionCreationConnectivity(lccConverterStationCreationModel, reportConverterStationNode);
        ModificationUtils.getInstance().reportModifications(reportConverterStationNode, shuntCompensatorsOnSide, "network.modification.converterStationFilters");

    }
}
