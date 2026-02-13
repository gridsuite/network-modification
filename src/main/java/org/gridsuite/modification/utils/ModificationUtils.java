/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.*;
import com.powsybl.iidm.modification.topology.*;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import org.apache.commons.math3.util.Pair;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.AbstractBranchModification;
import org.gridsuite.modification.modifications.BusbarSectionFinderTraverser;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Nullable;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.powsybl.iidm.network.TwoSides.ONE;
import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE1;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE2;
import static org.gridsuite.modification.modifications.AbstractBranchModification.*;
import static org.gridsuite.modification.modifications.byfilter.AbstractModificationByAssignment.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
// TODO transfer to powsybl-core (com.powsybl.iidm.modification)
// TODO remove public qualifier for all methods
public final class ModificationUtils {

    public static final String DISCONNECTOR = "disconnector_";
    public static final String BREAKER = "breaker_";
    public static final String BUS_BAR_SECTION_ID = "busbarSectionId";

    public static final String DOES_NOT_EXIST_IN_NETWORK = " does not exist in network";
    public static final String EQUIPMENT_DISCONNECTED = "network.modification.equipmentDisconnected";
    public static final String NO_VALUE = "No value";
    public static final String LIMITS = "network.modification.limits";
    public static final String REACTIVE_LIMITS = "network.modification.ReactiveLimits";
    public static final String OPERATIONAL_LIMITS_GROUP_ADDED_LOG_DETAIL = "network.modification.operationalLimitsGroupAdded.detail";
    private static final String SETPOINTS = "network.modification.Setpoints";
    private static final String MIN_REACTIVE_POWER_FIELDNAME = "Minimum reactive power";
    private static final String MAX_REACTIVE_POWER_FIELDNAME = "Maximum reactive power";
    public static final String CONNECTION_NAME_FIELD_NAME = "Connection name";
    public static final String CONNECTION_DIRECTION_FIELD_NAME = "Connection direction";
    public static final String CONNECTION_POSITION_FIELD_NAME = "Connection position";
    public static final String NOT_EXIST_IN_NETWORK = " does not exist in network";
    public static final String TRANSIENT_REACTANCE = "Transient reactance";
    public static final String TRANSFORMER_REACTANCE = "Transformer reactance";
    private static final String DROOP = "Droop";
    private static final String PARTICIPATE = "Participate";

    private static final String COULD_NOT_ACTION_EQUIPMENT = "Could not %s equipment '%s'";
    private static final String COULD_NOT_ACTION_EQUIPMENT_ON_SIDE = COULD_NOT_ACTION_EQUIPMENT + " on side %s";
    public static final String CONNECT = "connect";
    public static final String DISCONNECT = "disconnect";
    private static final String MEASUREMENT_VALIDITY_PROPERTY = "validity";
    public static final String FIELD_MAX_ACTIVE_POWER = "Maximum active power";
    public static final String FIELD_MIN_ACTIVE_POWER = "Minimum active power";
    public static final String FIELD_PLANNED_ACTIVE_POWER_SET_POINT = "Planned active power set point";
    public static final String FIELD_ACTIVE_POWER_TARGET = "Active power target";

    public static String applicabilityToString(OperationalLimitsGroupInfos.Applicability applicability) {
        return switch (applicability) {
            case EQUIPMENT -> "sides 1 & 2";
            case SIDE1 -> "side 1";
            case SIDE2 -> "side 2";
        };
    }

    public enum FeederSide {
        INJECTION_SINGLE_SIDE,
        BRANCH_SIDE_ONE,
        BRANCH_SIDE_TWO
    }

    private ModificationUtils() {
    }

    public static ModificationUtils getInstance() {
        return new ModificationUtils();
    }

    public Double zeroIfNull(Double d) {
        return d != null ? d : 0.0;
    }

    public static Double nanIfNull(Double d) {
        return d == null ? Double.NaN : d;
    }

    public VoltageLevel getVoltageLevel(Network network, String voltageLevelId) {
        VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
        if (voltageLevel == null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, voltageLevelId);
        }
        return voltageLevel;
    }

    public Line getLine(Network network, String lineId) {
        Line line = network.getLine(lineId);
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineId);
        }
        return line;
    }

    public Battery getBattery(Network network, String batteryId) {
        Battery battery = network.getBattery(batteryId);
        if (battery == null) {
            throw new NetworkModificationException(BATTERY_NOT_FOUND, "Battery " + batteryId + NOT_EXIST_IN_NETWORK);
        }
        return battery;
    }

    public Generator getGenerator(Network network, String generatorId) {
        Generator generator = network.getGenerator(generatorId);
        if (generator == null) {
            throw new NetworkModificationException(GENERATOR_NOT_FOUND, "Generator " + generatorId + NOT_EXIST_IN_NETWORK);
        }
        return generator;
    }

    public VscConverterStation getVscConverterStation(Network network, String converterStationId) {
        VscConverterStation vscConverterStation = network.getVscConverterStation(converterStationId);
        if (vscConverterStation == null) {
            throw new NetworkModificationException(VSC_CONVERTER_STATION_NOT_FOUND, "Vsc converter station  " + converterStationId + NOT_EXIST_IN_NETWORK);
        }
        return vscConverterStation;
    }

    public LccConverterStation getLccConverterStation(Network network, String converterStationId) {
        LccConverterStation lccConverterStation = network.getLccConverterStation(converterStationId);
        if (lccConverterStation == null) {
            throw new NetworkModificationException(LCC_CONVERTER_STATION_NOT_FOUND, "Lcc converter station  " + converterStationId + NOT_EXIST_IN_NETWORK);
        }
        return lccConverterStation;
    }

    //get hvdcline
    public HvdcLine getHvdcLine(Network network, String hvdcLineId) {
        HvdcLine hvdcLine = network.getHvdcLine(hvdcLineId);
        if (hvdcLine == null) {
            throw new NetworkModificationException(HVDC_LINE_NOT_FOUND, "Hvdc line  " + hvdcLineId + NOT_EXIST_IN_NETWORK);
        }
        return hvdcLine;
    }

    public StaticVarCompensator getStaticVarCompensator(Network network, String staticVarCompensatorId) {
        StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(staticVarCompensatorId);
        if (staticVarCompensator == null) {
            throw new NetworkModificationException(STATIC_VAR_COMPENSATOR_NOT_FOUND, "Static var compensator " + staticVarCompensatorId + DOES_NOT_EXIST_IN_NETWORK);
        }
        return staticVarCompensator;
    }

    public void controlConnectivity(Network network, String voltageLevelId, String busOrBusbarSectionId) {
        VoltageLevel voltageLevel = getVoltageLevel(network, voltageLevelId);
        controlBus(voltageLevel, busOrBusbarSectionId);
    }

    public void controlBus(VoltageLevel voltageLevel, String busOrBusbarSectionId) {
        if (voltageLevel.getTopologyKind() == TopologyKind.BUS_BREAKER) {
            getBusBreakerBus(voltageLevel, busOrBusbarSectionId);
        } else {
            getNodeBreakerBusbarSection(voltageLevel, busOrBusbarSectionId);
        }
    }

    public void controlBranchCreation(Network network, String voltageLevelId1, String busOrBusbarSectionId1,
                                      String voltageLevelId2, String busOrBusbarSectionId2) {
        VoltageLevel voltageLevel1 = getVoltageLevel(network, voltageLevelId1);
        VoltageLevel voltageLevel2 = getVoltageLevel(network, voltageLevelId2);
        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER &&
                voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            controlConnectivity(network, voltageLevelId1,
                    busOrBusbarSectionId1);
            controlConnectivity(network, voltageLevelId2,
                    busOrBusbarSectionId2);
        } else {
            // bus or mixed mode
            controlBus(voltageLevel1, busOrBusbarSectionId1);
            controlBus(voltageLevel2, busOrBusbarSectionId2);
        }
    }

    public int getPosition(Integer defaultPosition, String busOrBusbarSectionId, Network network, VoltageLevel voltageLevel) {
        return defaultPosition != null
                ? defaultPosition
                : getPosition(busOrBusbarSectionId, network, voltageLevel);
    }

    public int getPosition(String busOrBusbarSectionId, Network network, VoltageLevel voltageLevel) {
        var position = 0;
        var bbs = network.getBusbarSection(busOrBusbarSectionId);
        if (bbs == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, busOrBusbarSectionId);
        }

        var extensionExist = bbs.getExtension(BusbarSectionPosition.class) != null;
        if (!extensionExist) {
            return position;
        }

        if (voltageLevel.getConnectableStream().anyMatch(c -> !(c instanceof BusbarSection))) {
            var rightRange = TopologyModificationUtils.getUnusedOrderPositionsAfter(bbs);
            if (rightRange.isPresent()) {
                position = rightRange.get().getMinimum();
            } else {
                var leftRange = TopologyModificationUtils.getUnusedOrderPositionsBefore(bbs);
                if (leftRange.isPresent()) {
                    position = leftRange.get().getMaximum();
                }
            }
        }

        return position;
    }

    public Bus getBusBreakerBus(VoltageLevel voltageLevel, String busId) {
        VoltageLevel.BusBreakerView busBreakerView = voltageLevel.getBusBreakerView();
        Bus bus = busBreakerView.getBus(busId);
        if (bus == null) {
            throw new NetworkModificationException(BUS_NOT_FOUND, busId);
        }
        return bus;
    }

    public BusbarSection getNodeBreakerBusbarSection(VoltageLevel voltageLevel, String busBarSectionId) {
        VoltageLevel.NodeBreakerView nodeBreakerView = voltageLevel.getNodeBreakerView();
        BusbarSection busbarSection = nodeBreakerView.getBusbarSection(busBarSectionId);
        if (busbarSection == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, busBarSectionId);
        }
        return busbarSection;
    }

    public int createNodeBreakerCellSwitches(VoltageLevel voltageLevel, String busBarSectionId, String equipmentId,
                                             String equipmentName, String sideSuffix) {
        VoltageLevel.NodeBreakerView nodeBreakerView = voltageLevel.getNodeBreakerView();
        BusbarSection busbarSection = nodeBreakerView.getBusbarSection(busBarSectionId);
        if (busbarSection == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, busBarSectionId);
        }

        // creating the disconnector
        int newNode = nodeBreakerView.getMaximumNodeIndex();
        String disconnectorId = DISCONNECTOR + equipmentId + sideSuffix;
        String disconnectorName = equipmentName != null ? DISCONNECTOR + equipmentName + sideSuffix : null;
        nodeBreakerView.newSwitch()
            .setId(disconnectorId)
            .setName(disconnectorName)
            .setKind(SwitchKind.DISCONNECTOR)
            .setRetained(false)
            .setOpen(false)
            .setFictitious(false)
            .setNode1(busbarSection.getTerminal().getNodeBreakerView().getNode())
            .setNode2(newNode + 1)
            .add();

        // creating the breaker
        String breakerId = BREAKER + equipmentId + sideSuffix;
        String breakerName = equipmentName != null ? BREAKER + equipmentName + sideSuffix : null;
        nodeBreakerView.newSwitch()
            .setId(breakerId)
            .setName(breakerName)
            .setKind(SwitchKind.BREAKER)
            .setRetained(false)
            .setOpen(false)
            .setFictitious(false)
            .setNode1(newNode + 1)
            .setNode2(newNode + 2)
            .add();

        return newNode + 2;
    }

    public void controlNewOrExistingVoltageLevel(VoltageLevelCreationInfos mayNewVL,
                String existingVoltageLevelId, String bbsOrBusId, Network network) {
        if (mayNewVL != null) {
            controlVoltageLevelCreation(mayNewVL, network);
        } else {
            // use existing VL
            VoltageLevel vl = network.getVoltageLevel(existingVoltageLevelId);
            if (vl == null) {
                throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, existingVoltageLevelId);
            }
            // check existing busbar/bus
            controlBus(vl, bbsOrBusId);
        }
    }

    public void controlVoltageLevelCreation(VoltageLevelCreationInfos voltageLevelCreationInfos, Network network) {
        if (network.getVoltageLevel(voltageLevelCreationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, voltageLevelCreationInfos.getEquipmentId());
        }
        if (voltageLevelCreationInfos.getCouplingDevices().stream()
                .anyMatch(cd -> cd.getBusbarSectionId1().equals(cd.getBusbarSectionId2()))) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR,
                    "Coupling between same bus bar section is not allowed");
        }
        if (Objects.nonNull(voltageLevelCreationInfos.getIpMin()) && voltageLevelCreationInfos.getIpMin() < 0) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMin must be positive");
        }
        if (Objects.nonNull(voltageLevelCreationInfos.getIpMax()) && voltageLevelCreationInfos.getIpMax() < 0) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMax must be positive");
        }
        if (Objects.nonNull(voltageLevelCreationInfos.getIpMin()) && Objects.isNull(voltageLevelCreationInfos.getIpMax())) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMax is required");
        }
        if (Objects.nonNull(voltageLevelCreationInfos.getIpMin()) && Objects.nonNull(voltageLevelCreationInfos.getIpMax())
                && voltageLevelCreationInfos.getIpMin() > voltageLevelCreationInfos.getIpMax()) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMin cannot be greater than IpMax");
        }
    }

    private boolean checkBbs(Network network, String busbarSectionId1, String busbarSectionId2, ReportNode subReportNode) {
        Identifiable<?> busOrBbs1 = network.getIdentifiable(busbarSectionId1);
        Identifiable<?> busOrBbs2 = network.getIdentifiable(busbarSectionId2);
        if (busOrBbs1 == null) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.notFoundBurOrBusbarSection")
                    .withUntypedValue(BUS_BAR_SECTION_ID, busbarSectionId1)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
            return false;
        }
        if (busOrBbs2 == null) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.notFoundBurOrBusbarSection")
                    .withUntypedValue(BUS_BAR_SECTION_ID, busbarSectionId2)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
            return false;
        }
        return true;
    }

    public void createSubstation(SubstationCreationInfos substationCreationInfos,
                                   ReportNode subReportNode, Network network) {
        network.newSubstation()
                .setId(substationCreationInfos.getEquipmentId())
                .setName(substationCreationInfos.getEquipmentName())
                .setCountry(substationCreationInfos.getCountry())
                .add();

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.substationCreated")
                .withUntypedValue("id", substationCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        // name and country
        if (substationCreationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, substationCreationInfos.getEquipmentName(), "Name");
        }
        if (substationCreationInfos.getCountry() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, substationCreationInfos.getCountry(), "Country");
        }
    }

    public void createVoltageLevel(VoltageLevelCreationInfos voltageLevelCreationInfos,
                                   ReportNode subReportNode, Network network) {
        String substationId = voltageLevelCreationInfos.getSubstationId();
        SubstationCreationInfos substationCreation = voltageLevelCreationInfos.getSubstationCreation();
        Substation substation;
        if (substationCreation != null) {
            substationId = substationCreation.getEquipmentId();
            createSubstation(substationCreation, subReportNode, network);
            substation = network.getSubstation(substationId);
            PropertiesUtils.applyProperties(substation, subReportNode, substationCreation.getProperties(), "network.modification.SubstationProperties");
        } else {
            substation = network.getSubstation(substationId);
        }
        if (substation == null) {
            throw new NetworkModificationException(SUBSTATION_NOT_FOUND, substationId);
        }
        VoltageLevel voltageLevel = substation.newVoltageLevel()
            .setId(voltageLevelCreationInfos.getEquipmentId())
            .setName(voltageLevelCreationInfos.getEquipmentName())
            .setTopologyKind(TopologyKind.NODE_BREAKER)
            .setNominalV(voltageLevelCreationInfos.getNominalV())
            .add();

        if (voltageLevelCreationInfos.getLowVoltageLimit() != null) {
            voltageLevel.setLowVoltageLimit(voltageLevelCreationInfos.getLowVoltageLimit());
        }
        if (voltageLevelCreationInfos.getHighVoltageLimit() != null) {
            voltageLevel.setHighVoltageLimit(voltageLevelCreationInfos.getHighVoltageLimit());
        }

        if (voltageLevelCreationInfos.getIpMax() != null && voltageLevelCreationInfos.getIpMin() != null) {
            voltageLevel.newExtension(IdentifiableShortCircuitAdder.class)
                    .withIpMin(voltageLevelCreationInfos.getIpMin())
                    .withIpMax(voltageLevelCreationInfos.getIpMax())
                    .add();
        } else if (voltageLevelCreationInfos.getIpMax() != null && voltageLevelCreationInfos.getIpMin() == null) {
            voltageLevel.newExtension(IdentifiableShortCircuitAdder.class)
                    .withIpMax(voltageLevelCreationInfos.getIpMax())
                    .add();
        } else if (voltageLevelCreationInfos.getIpMax() == null && voltageLevelCreationInfos.getIpMin() != null) {
            voltageLevel.newExtension(IdentifiableShortCircuitAdder.class)
                    .withIpMin(voltageLevelCreationInfos.getIpMin())
                    .add();
        }

        CreateVoltageLevelTopologyBuilder voltageLevelTopologyBuilder = new CreateVoltageLevelTopologyBuilder();
        voltageLevelTopologyBuilder.withVoltageLevelId(voltageLevelCreationInfos.getEquipmentId())
                .withAlignedBusesOrBusbarCount(voltageLevelCreationInfos.getBusbarCount())
                .withSectionCount(voltageLevelCreationInfos.getSectionCount())
                .withSwitchKinds(voltageLevelCreationInfos.getSwitchKinds())
                .build().apply(network);

        voltageLevelCreationInfos.getCouplingDevices().forEach(couplingDevice -> {
            if (!checkBbs(network, couplingDevice.getBusbarSectionId1(), couplingDevice.getBusbarSectionId2(), subReportNode)) {
                return;
            }
            CreateCouplingDeviceBuilder couplingDeviceBuilder = new CreateCouplingDeviceBuilder();
            couplingDeviceBuilder.withBusOrBusbarSectionId1(couplingDevice.getBusbarSectionId1())
                .withBusOrBusbarSectionId2(couplingDevice.getBusbarSectionId2())
                .withSwitchPrefixId(voltageLevelCreationInfos.getEquipmentId() + "_COUPL")
                    .build().apply(network, subReportNode);
        });

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevelCreated")
                .withUntypedValue("id", voltageLevelCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        PropertiesUtils.applyProperties(voltageLevel, subReportNode, voltageLevelCreationInfos.getProperties(), "network.modification.VlProperties");
    }

    public LineAdder createLineAdder(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationInfos lineCreationInfos, boolean withSwitch1, boolean withSwitch2) {

        // common settings
        LineAdder lineAdder = network.newLine()
                .setId(lineCreationInfos.getEquipmentId())
                .setName(lineCreationInfos.getEquipmentName())
                .setVoltageLevel1(lineCreationInfos.getVoltageLevelId1())
                .setVoltageLevel2(lineCreationInfos.getVoltageLevelId2())
                .setR(lineCreationInfos.getR())
                .setX(lineCreationInfos.getX())
                .setG1(lineCreationInfos.getG1() != null ? lineCreationInfos.getG1() : 0.0)
                .setB1(lineCreationInfos.getB1() != null ? lineCreationInfos.getB1() : 0.0)
                .setG2(lineCreationInfos.getG2() != null ? lineCreationInfos.getG2() : 0.0)
                .setB2(lineCreationInfos.getB2() != null ? lineCreationInfos.getB2() : 0.0);

        // lineAdder completion by topology
        setBranchAdderNodeOrBus(lineAdder, voltageLevel1, lineCreationInfos, ONE, withSwitch1);
        setBranchAdderNodeOrBus(lineAdder, voltageLevel2, lineCreationInfos, TwoSides.TWO, withSwitch2);

        return lineAdder;
    }

    public void setBranchAdderNodeOrBus(BranchAdder<?, ?> branchAdder, VoltageLevel voltageLevel, BranchCreationInfos branchCreationInfos,
                                 TwoSides side, boolean withSwitch) {
        String busOrBusbarSectionId = (side == ONE) ? branchCreationInfos.getBusOrBusbarSectionId1() : branchCreationInfos.getBusOrBusbarSectionId2();
        if (voltageLevel.getTopologyKind() == TopologyKind.BUS_BREAKER) {
            setBranchAdderBusBreaker(branchAdder, voltageLevel, side, busOrBusbarSectionId);
        } else {
            if (withSwitch) { // NODE_BREAKER
                setBranchAdderNodeBreaker(branchAdder, voltageLevel, branchCreationInfos, side, busOrBusbarSectionId);
            }
        }
    }

    private void setBranchAdderBusBreaker(BranchAdder<?, ?> branchAdder, VoltageLevel voltageLevel, TwoSides side, String busId) {
        Bus bus = getBusBreakerBus(voltageLevel, busId);

        // complete the lineAdder
        if (side == ONE) {
            branchAdder.setBus1(bus.getId()).setConnectableBus1(bus.getId());
        } else {
            branchAdder.setBus2(bus.getId()).setConnectableBus2(bus.getId());
        }
    }

    private void setBranchAdderNodeBreaker(BranchAdder<?, ?> branchAdder, VoltageLevel voltageLevel,
                                           BranchCreationInfos branchCreationInfos, TwoSides side,
                                           String currentBusBarSectionId) {
        // create cell switches
        String sideSuffix = side != null ? "_" + side.name() : "";
        int nodeNum = createNodeBreakerCellSwitches(voltageLevel,
            currentBusBarSectionId,
            branchCreationInfos.getEquipmentId(),
            branchCreationInfos.getEquipmentName(),
            sideSuffix);

        // complete the lineAdder
        if (side == ONE) {
            branchAdder.setNode1(nodeNum);
        } else {
            branchAdder.setNode2(nodeNum);
        }
    }

    public static void createReport(ReportNode reportNode, String reporterKey, Map<String, Object> values, TypedValue errorSeverity) {
        ReportNodeAdder adder = reportNode.newReportNode()
                .withMessageTemplate(reporterKey)
                .withSeverity(errorSeverity);

        for (Map.Entry<String, Object> valueEntry : values.entrySet()) {
            adder.withUntypedValue(valueEntry.getKey(), valueEntry.getValue().toString());
        }
        adder.add();
    }

    public static <T> Predicate<T> distinctByKey(
            Function<? super T, ?> keyExtractor) {

        Map<Object, Boolean> seen = new ConcurrentHashMap<>();
        return t -> seen.putIfAbsent(keyExtractor.apply(t), Boolean.TRUE) == null;
    }

    public <T> ReportNode applyElementaryModificationsAndReturnReport(Consumer<T> setter, Supplier<T> getter,
                                                                  AttributeModification<T> modification, String fieldName) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            return buildModificationReport(oldValue, newValue, fieldName);
        }
        return null;
    }

    public ReportNode createEnabledDisabledReport(String key, boolean enabled) {
        return ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate(key)
                .withUntypedValue("status", enabled ? "Enabled" : "Disabled")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build();
    }

    public ReportNode reportModifications(ReportNode reportNode, List<ReportNode> reports, String subReportNodeKey) {
        return reportModifications(reportNode, reports, subReportNodeKey, Map.of());
    }

    public void reportModifications(ReportNode reportNode, List<ReportNode> reports) {
        List<ReportNode> validReports = reports.stream().filter(Objects::nonNull).toList();
        if (!validReports.isEmpty() && reportNode != null) {
            for (ReportNode report : validReports) {
                ReportNodeAdder reportNodeAdder = reportNode.newReportNode().withMessageTemplate(report.getMessageKey()).withSeverity(TypedValue.DETAIL_SEVERITY);
                for (Map.Entry<String, TypedValue> valueEntry : report.getValues().entrySet()) {
                    reportNodeAdder.withUntypedValue(valueEntry.getKey(), valueEntry.getValue().toString());
                }
                report.getValue(ReportConstants.SEVERITY_KEY).ifPresent(reportNodeAdder::withSeverity);
                reportNodeAdder.add();
            }
        }
    }

    public ReportNode reportModifications(ReportNode reportNode, List<ReportNode> reports, String subReportNodeKey, Map<String, Object> subReportNodeKeyArgs) {
        List<ReportNode> validReports = reports.stream().filter(Objects::nonNull).toList();
        ReportNode subReportNode = null;
        if (!validReports.isEmpty() && reportNode != null) {
            // new child report node
            ReportNodeAdder subReportNodeAdder = reportNode.newReportNode().withMessageTemplate(subReportNodeKey);
            for (Map.Entry<String, Object> keyArg : subReportNodeKeyArgs.entrySet()) {
                subReportNodeAdder.withUntypedValue(keyArg.getKey(), keyArg.getValue().toString());
            }
            subReportNode = subReportNodeAdder.add();
            for (ReportNode report : validReports) {
                ReportNodeAdder reportNodeAdder = subReportNode.newReportNode().withMessageTemplate(report.getMessageKey()).withSeverity(TypedValue.INFO_SEVERITY);
                for (Map.Entry<String, TypedValue> valueEntry : report.getValues().entrySet()) {
                    reportNodeAdder.withUntypedValue(valueEntry.getKey(), valueEntry.getValue().toString());
                }
                report.getValue(ReportConstants.SEVERITY_KEY).ifPresent(reportNodeAdder::withSeverity);
                reportNodeAdder.add();
            }
        }
        return subReportNode;
    }

    public <T> void applyElementaryModifications(Consumer<T> setter, Supplier<T> getter,
            AttributeModification<T> modification,
            ReportNode subReportNode, String fieldName) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            if (subReportNode != null) {
                insertReportNode(subReportNode, buildModificationReport(oldValue, newValue, fieldName));
            }
        }
    }

    public <T> ReportNode applyAndBuildModificationReport(Consumer<T> setter, Supplier<T> getter, AttributeModification<T> modification, String fieldName) {
        T oldValue = getter.get();
        T newValue = modification.applyModification(oldValue);
        setter.accept(newValue);
        return buildModificationReport(oldValue, newValue, fieldName, TypedValue.INFO_SEVERITY);
    }

    public <T> ReportNode buildModificationReport(T oldValue, T newValue, String fieldName) {
        return buildModificationReport(oldValue, newValue, fieldName, TypedValue.INFO_SEVERITY);
    }

    public static <T> ReportNode buildModificationReport(T oldValue, T newValue, String fieldName, TypedValue severity) {
        final String oldValueString = (oldValue == null || oldValue instanceof Double oldDouble && Double.isNaN(oldDouble))
                ? NO_VALUE : oldValue.toString();
        final String newValueString = (newValue == null || newValue instanceof Double newDouble && Double.isNaN(newDouble))
                ? NO_VALUE : newValue.toString();
        return ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("network.modification.fieldModification")
                .withUntypedValue("arrow", "→") // Workaround to use non-ISO-8859-1 characters in the internationalization file
                .withUntypedValue("fieldName", fieldName)
                .withUntypedValue("oldValue", oldValueString)
                .withUntypedValue("newValue", newValueString)
                .withSeverity(severity)
                .build();
    }

    public Terminal getTerminalFromIdentifiable(Network network, String equipmentId, String type, String voltageLevelId) {
        if (network != null && equipmentId != null && type != null && voltageLevelId != null) {
            Identifiable<?> identifiable = getEquipmentByIdentifiableType(network, IdentifiableType.valueOf(type), equipmentId);

            if (identifiable == null) {
                throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=" + equipmentId + " not found with type " + type);
            }
            // checking if voltage level exists
            VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
            if (voltageLevel == null) {
                throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "Voltage level with id=" + voltageLevelId + " not found");
            }

            if (identifiable instanceof Injection<?>) {
                return ((Injection<?>) identifiable).getTerminal();
            } else if (identifiable instanceof Branch<?>) {
                return ((Branch<?>) identifiable).getTerminal(voltageLevelId);
            }
        }

        return null;
    }

    public List<Terminal> getTerminalsFromIdentifiable(Identifiable<?> identifiable) {
        if (identifiable instanceof Branch<?> branch) {
            return Stream.of(
                    branch.getTerminal1(),
                    branch.getTerminal2()
            ).toList();
        } else if (identifiable instanceof ThreeWindingsTransformer w3t) {
            return Stream.of(
                    w3t.getLeg1().getTerminal(),
                    w3t.getLeg2().getTerminal(),
                    w3t.getLeg3().getTerminal()
            ).toList();
        } else if (identifiable instanceof HvdcLine hvdcLine) {
            return Stream.of(
                    hvdcLine.getConverterStation1().getTerminal(),
                    hvdcLine.getConverterStation2().getTerminal()
            ).toList();
        }
        throw NetworkModificationException.createEquipmentTypeNotSupported(identifiable.getClass().getSimpleName());
    }

    public static boolean isInjectionConnected(Injection<?> injection) {
        return injection != null && injection.getTerminal().isConnected();
    }

    public void disconnectCreatedInjection(InjectionCreationInfos modificationInfos, Injection<?> injection, ReportNode subReportNode) {
        // A newly created injection is connected by default, unless we choose not to do
        if (!modificationInfos.isTerminalConnected()) {
            injection.getTerminal().disconnect();
            subReportNode.newReportNode()
                    .withMessageTemplate(EQUIPMENT_DISCONNECTED)
                    .withUntypedValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }

    public ReportNode modifyInjectionConnectivityAttributes(ConnectablePosition<?> connectablePosition,
                                                            ConnectablePositionAdder<?> connectablePositionAdder,
                                                            Injection<?> injection,
                                                            InjectionModificationInfos modificationInfos,
                                                            ReportNode connectivityReports) {
        List<ReportNode> reports = new ArrayList<>();
        processConnectivityPosition(connectablePosition, connectablePositionAdder, modificationInfos, injection.getNetwork(), reports);
        modifyConnection(modificationInfos.getTerminalConnected(), injection, injection.getTerminal(), reports);

        return reportModifications(connectivityReports, reports, "network.modification.ConnectivityModified");
    }

    public ReportNode modifyBranchConnectivityAttributes(ConnectablePosition<?> connectablePosition,
                                                         ConnectablePositionAdder<?> connectablePositionAdder,
                                                         Branch<?> branch,
                                                         BranchModificationInfos modificationInfos,
                                                         ReportNode connectivityReports) {
        List<ReportNode> reports = new ArrayList<>();
        processConnectivityPosition(connectablePosition, connectablePositionAdder, modificationInfos, branch.getNetwork(), reports);

        List<NetworkModificationException> exceptions = new ArrayList<>();
        // Pair for Side and Pair for update and terminal
        List<Pair<ThreeSides, Pair<AttributeModification<Boolean>, Terminal>>> modificationsBySides =
                List.of(
                        Pair.create(ThreeSides.ONE, Pair.create(modificationInfos.getTerminal1Connected(), branch.getTerminal1())),
                        Pair.create(ThreeSides.TWO, Pair.create(modificationInfos.getTerminal2Connected(), branch.getTerminal2()))
                );
        // We want information for the both sides, not only the first in error
        for (Pair<ThreeSides, Pair<AttributeModification<Boolean>, Terminal>> side : modificationsBySides) {
            try {
                modifyConnection(side.getSecond().getFirst(), branch, side.getSecond().getSecond(), reports, side.getFirst());
            } catch (NetworkModificationException nme) {
                exceptions.add(nme);
            }
        }
        if (exceptions.size() == 1) {
            // One exception to throw for one side
            throw exceptions.getFirst();
        } else if (exceptions.size() > 1) {
            // One exception to throw for the two sides
            List<String> actions = new ArrayList<>(); // Action = connect|disconnect
            actions.add(branch.getTerminal1().isConnected() ? DISCONNECT : CONNECT);
            actions.add(branch.getTerminal2().isConnected() ? DISCONNECT : CONNECT);
            throw new NetworkModificationException(exceptions.getFirst().getType(),
                    String.format("Could not %s equipment '%s' on side %s",
                            actions.stream().distinct().collect(Collectors.joining("/")),
                            branch.getId(),
                            "1 & 2")); // Exactly the both sides awaited here
        }

        return reportModifications(connectivityReports, reports, "network.modification.ConnectivityModified");
    }

    private void processConnectivityPosition(ConnectablePosition<?> connectablePosition,
                                             ConnectablePositionAdder<?> connectablePositionAdder,
                                             BasicEquipmentModificationInfos modificationInfos,
                                             Network network,
                                             List<ReportNode> reports) {
        if (connectablePosition != null) {
            modifyExistingConnectivityPosition(connectablePosition, modificationInfos, reports);
        } else {
            createNewConnectivityPosition(connectablePositionAdder, modificationInfos, network, reports);
        }
    }

    private void modifyExistingConnectivityPosition(ConnectablePosition<?> connectablePosition,
                                                    BasicEquipmentModificationInfos modificationInfos,
                                                    List<ReportNode> reports) {
        if (modificationInfos instanceof BranchModificationInfos) {
            modifyConnectablePosition(connectablePosition.getFeeder1(), modificationInfos, reports, FeederSide.BRANCH_SIDE_ONE);
            modifyConnectablePosition(connectablePosition.getFeeder2(), modificationInfos, reports, FeederSide.BRANCH_SIDE_TWO);
        } else if (modificationInfos instanceof InjectionModificationInfos) {
            modifyConnectablePosition(connectablePosition.getFeeder(), modificationInfos, reports, FeederSide.INJECTION_SINGLE_SIDE);
        }
    }

    private void createNewConnectivityPosition(ConnectablePositionAdder<?> adder,
                                               BasicEquipmentModificationInfos modificationInfos,
                                               Network network,
                                               List<ReportNode> reports) {
        if (modificationInfos instanceof BranchModificationInfos) {
            addConnectablePosition(adder, modificationInfos, network, reports, FeederSide.BRANCH_SIDE_ONE);
            addConnectablePosition(adder, modificationInfos, network, reports, FeederSide.BRANCH_SIDE_TWO);
        } else if (modificationInfos instanceof InjectionModificationInfos) {
            addConnectablePosition(adder, modificationInfos, network, reports, FeederSide.INJECTION_SINGLE_SIDE);
        }
    }

    private void modifyConnectablePosition(ConnectablePosition.Feeder feeder,
                                        BasicEquipmentModificationInfos modificationInfos,
                                        List<ReportNode> reports,
                                        FeederSide feederSide) {
        if (feeder != null) {
            applyModifications(feeder, modificationInfos, reports, feederSide);
        }
    }

    private void applyModifications(ConnectablePosition.Feeder feeder,
                                    BasicEquipmentModificationInfos modificationInfos,
                                    List<ReportNode> reports,
                                    FeederSide feederSide) {
        ReportNode connectionNameReport = applyElementaryModificationsAndReturnReport(feeder::setName,
                feeder.getName()::get,
                getConnectionName(modificationInfos, feederSide),
                getConnectionNameField(feederSide));
        if (connectionNameReport != null) {
            reports.add(connectionNameReport);
        }
        ReportNode connectionDirectionReport = applyElementaryModificationsAndReturnReport(feeder::setDirection,
                feeder::getDirection,
                getConnectionDirection(modificationInfos, feederSide),
                getConnectionDirectionField(feederSide));
        if (connectionDirectionReport != null) {
            reports.add(connectionDirectionReport);
        }
        ReportNode connectionPositionReport = applyElementaryModificationsAndReturnReport(feeder::setOrder,
                feeder.getOrder()::get,
                getConnectionPosition(modificationInfos, feederSide),
                getConnectionPositionField(feederSide));
        if (connectionPositionReport != null) {
            reports.add(connectionPositionReport);
        }
    }

    private void addConnectablePosition(ConnectablePositionAdder<?> adder,
                                        BasicEquipmentModificationInfos modificationInfos,
                                        Network network,
                                        List<ReportNode> reports,
                                        FeederSide feederSide) {
        AttributeModification<String> connectionName = getConnectionName(modificationInfos, feederSide);
        AttributeModification<ConnectablePosition.Direction> connectionDirection = getConnectionDirection(modificationInfos, feederSide);
        AttributeModification<Integer> connectionPosition = getConnectionPosition(modificationInfos, feederSide);
        if (Objects.isNull(connectionName) && Objects.isNull(connectionDirection) && Objects.isNull(connectionPosition)) {
            return;
        }
        AttributeModification<String> equipmentId = getEquipmentId(modificationInfos);
        AttributeModification<String> voltageLevelId = getVoltageLevelId(modificationInfos, feederSide);
        AttributeModification<String> busOrBusbarSectionId = getBusOrBusbarSectionId(modificationInfos, feederSide);
        int position = getPosition(connectionPosition, busOrBusbarSectionId, voltageLevelId, equipmentId, feederSide, network);
        ConnectablePositionAdder.FeederAdder<?> feeder;
        switch (feederSide) {
            case INJECTION_SINGLE_SIDE -> feeder = adder.newFeeder();
            case BRANCH_SIDE_ONE -> feeder = adder.newFeeder1();
            case BRANCH_SIDE_TWO -> feeder = adder.newFeeder2();
            default -> {
                return;
            }
        }
        ReportNode connectionNameReport = applyConnectablePositionAttribute(
                feeder::withName, connectionName, equipmentId, reports,
                getConnectionNameField(feederSide), connectionDirection, connectionPosition
        );

        ReportNode connectionDirectionReport = applyConnectablePositionAttribute(
                feeder::withDirection, connectionDirection,
                new AttributeModification<>(ConnectablePosition.Direction.UNDEFINED, OperationType.SET),
                reports, getConnectionDirectionField(feederSide),
                connectionName, connectionPosition
        );

        ReportNode connectionPositionReport = applyConnectablePositionAttribute(
                feeder::withOrder, connectionPosition,
                new AttributeModification<>(position, OperationType.SET),
                reports, getConnectionPositionField(feederSide),
                connectionName, connectionDirection
        );

        if (connectionNameReport != null || connectionDirectionReport != null || connectionPositionReport != null) {
            feeder.add();
            adder.add();
        }
    }

    private <T> ReportNode applyConnectablePositionAttribute(Consumer<T> setter,
                                                             AttributeModification<T> newValue,
                                                             AttributeModification<T> defaultValue,
                                                             List<ReportNode> reports,
                                                             String fieldName,
                                                             AttributeModification<?>... dependentAttributes) {

        AttributeModification<T> finalModification = (newValue == null && isAnyAttributesNonNull(dependentAttributes))
                ? defaultValue
                : newValue;

        ReportNode report = applyElementaryModificationsAndReturnReport(setter, () -> null, finalModification, fieldName);
        if (report != null) {
            reports.add(report);
        }
        return report;
    }

    private boolean isAnyAttributesNonNull(AttributeModification<?>... attributes) {
        return Arrays.stream(attributes).anyMatch(Objects::nonNull);
    }

    private <T> T getConnectionDetail(BasicEquipmentModificationInfos modificationInfos, FeederSide feederSide,
                                      Function<BranchModificationInfos, T> branchFunc1,
                                      Function<BranchModificationInfos, T> branchFunc2,
                                      Function<InjectionModificationInfos, T> injectionFunc) {
        if (modificationInfos instanceof BranchModificationInfos branchInfo) {
            if (Objects.requireNonNull(feederSide) == FeederSide.BRANCH_SIDE_ONE) {
                return branchFunc1.apply(branchInfo);
            } else if (feederSide == FeederSide.BRANCH_SIDE_TWO) {
                return branchFunc2.apply(branchInfo);
            }
        } else if (modificationInfos instanceof InjectionModificationInfos injectionInfo) {
            return injectionFunc.apply(injectionInfo);
        }
        return null;
    }

    static String getConnectionFieldName(FeederSide feederSide, String baseFieldName) {
        return switch (feederSide) {
            case INJECTION_SINGLE_SIDE -> baseFieldName;
            case BRANCH_SIDE_ONE -> baseFieldName + " 1";
            case BRANCH_SIDE_TWO -> baseFieldName + " 2";
        };
    }

    private AttributeModification<String> getVoltageLevelId(BasicEquipmentModificationInfos modificationInfos, FeederSide feederSide) {
        return getConnectionDetail(modificationInfos, feederSide,
                BranchModificationInfos::getVoltageLevelId1, BranchModificationInfos::getVoltageLevelId2,
                InjectionModificationInfos::getVoltageLevelId);
    }

    private AttributeModification<String> getBusOrBusbarSectionId(BasicEquipmentModificationInfos modificationInfos, FeederSide feederSide) {
        return getConnectionDetail(modificationInfos, feederSide,
                BranchModificationInfos::getBusOrBusbarSectionId1, BranchModificationInfos::getBusOrBusbarSectionId2,
                InjectionModificationInfos::getBusOrBusbarSectionId);
    }

    private AttributeModification<String> getEquipmentId(BasicEquipmentModificationInfos modificationInfos) {
        return AttributeModification.toAttributeModification(modificationInfos.getEquipmentId(), OperationType.SET);
    }

    private AttributeModification<String> getConnectionName(BasicEquipmentModificationInfos modificationInfos, FeederSide feederSide) {
        return getConnectionDetail(modificationInfos, feederSide,
                BranchModificationInfos::getConnectionName1, BranchModificationInfos::getConnectionName2,
                InjectionModificationInfos::getConnectionName);
    }

    private static String getConnectionNameField(FeederSide feederSide) {
        return getConnectionFieldName(feederSide, CONNECTION_NAME_FIELD_NAME);
    }

    private static String getConnectionDirectionField(FeederSide feederSide) {
        return getConnectionFieldName(feederSide, CONNECTION_DIRECTION_FIELD_NAME);
    }

    private static String getConnectionPositionField(FeederSide feederSide) {
        return getConnectionFieldName(feederSide, CONNECTION_POSITION_FIELD_NAME);
    }

    private AttributeModification<ConnectablePosition.Direction> getConnectionDirection(BasicEquipmentModificationInfos modificationInfos, FeederSide feederSide) {
        return getConnectionDetail(modificationInfos, feederSide,
                BranchModificationInfos::getConnectionDirection1, BranchModificationInfos::getConnectionDirection2,
                InjectionModificationInfos::getConnectionDirection);
    }

    private AttributeModification<Integer> getConnectionPosition(BasicEquipmentModificationInfos modificationInfos, FeederSide feederSide) {
        return getConnectionDetail(modificationInfos, feederSide,
                BranchModificationInfos::getConnectionPosition1, BranchModificationInfos::getConnectionPosition2,
                InjectionModificationInfos::getConnectionPosition);
    }

    public String getBusOrBusbarSection(Terminal terminal) {
        String busOrBusbarSectionId;
        if (terminal.getVoltageLevel().getTopologyKind().equals(TopologyKind.BUS_BREAKER)) {
            if (terminal.isConnected()) {
                busOrBusbarSectionId = terminal.getBusBreakerView().getBus().getId();
            } else {
                busOrBusbarSectionId = terminal.getBusBreakerView().getConnectableBus().getId();
            }
        } else {
            busOrBusbarSectionId = BusbarSectionFinderTraverser.findBusbarSectionId(terminal);
        }
        return busOrBusbarSectionId;
    }

    private int getPosition(AttributeModification<Integer> connectionPosition,
                            AttributeModification<String> busOrBusbarSectionId,
                            AttributeModification<String> voltageLevelId,
                            AttributeModification<String> equipmentId,
                            FeederSide feederSide,
                            Network network) {
        String equipmentValue = equipmentId.getValue();
        Terminal selectedTerminal = null;
        switch (feederSide) {
            case INJECTION_SINGLE_SIDE -> selectedTerminal = network.getIdentifiable(equipmentValue) instanceof Injection<?> injection ? injection.getTerminal() : null;
            case BRANCH_SIDE_ONE -> selectedTerminal = network.getIdentifiable(equipmentValue) instanceof Branch<?> branch ? branch.getTerminal1() : null;
            case BRANCH_SIDE_TWO -> selectedTerminal = network.getIdentifiable(equipmentValue) instanceof Branch<?> branch ? branch.getTerminal2() : null;
        }
        String voltageLevel = (voltageLevelId != null && voltageLevelId.getValue() != null)
                ? voltageLevelId.getValue()
                : Optional.ofNullable(selectedTerminal).map(terminal -> terminal.getVoltageLevel().getId()).orElse(null);
        String busOrBusbarSection = (busOrBusbarSectionId != null && busOrBusbarSectionId.getValue() != null)
                ? busOrBusbarSectionId.getValue()
                : Optional.ofNullable(selectedTerminal).map(this::getBusOrBusbarSection).orElse(null);
        Integer connectionPositionValue = (connectionPosition != null) ? connectionPosition.getValue() : null;
        return getPosition(connectionPositionValue, busOrBusbarSection, network, getVoltageLevel(network, voltageLevel));
    }

    private void modifyConnection(AttributeModification<Boolean> terminalConnected, Identifiable<?> equipment, Terminal terminal, List<ReportNode> reports) {
        modifyConnection(terminalConnected, equipment, terminal, reports, null);
    }

    private void modifyConnection(AttributeModification<Boolean> terminalConnected, Identifiable<?> equipment, Terminal terminal, List<ReportNode> reports, ThreeSides side) {
        if (terminalConnected == null || equipment == null) {
            return;
        }

        boolean isConnected = terminal.isConnected();
        if (isConnected && Boolean.FALSE.equals(terminalConnected.getValue())) {
            terminal.disconnect();
            validateConnectionChange(!terminal.isConnected(), equipment, DISCONNECT, reports, side);
        } else if (!isConnected && Boolean.TRUE.equals(terminalConnected.getValue())) {
            terminal.connect();
            validateConnectionChange(terminal.isConnected(), equipment, CONNECT, reports, side);
        }
    }

    private void validateConnectionChange(boolean success, Identifiable<?> equipment, String action, List<ReportNode> reports, ThreeSides side) {
        if (!success) {
            throw new NetworkModificationException(
                    equipment instanceof Branch<?> ? BRANCH_MODIFICATION_ERROR : INJECTION_MODIFICATION_ERROR,
                    side == null ? String.format(COULD_NOT_ACTION_EQUIPMENT, action, equipment.getId())
                            : String.format(COULD_NOT_ACTION_EQUIPMENT_ON_SIDE, action, equipment.getId(), side.getNum()));
        }

        String reportKey = "network.modification.equipment" + capitalize(action) + (side != null ? ".side" : "");
        ReportNodeBuilder builder = ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate(reportKey)
            .withUntypedValue("id", equipment.getId())
            .withSeverity(TypedValue.INFO_SEVERITY);

        if (side != null) {
            builder.withUntypedValue("side", side.getNum());
        }

        reports.add(builder.build());
    }

    private String capitalize(String input) {
        return input.substring(0, 1).toUpperCase() + input.substring(1);
    }

    public void disconnectBranch(BranchCreationInfos modificationInfos, Branch<?> branch, ReportNode subReportNode) {
        // A newly created branch is connected by default on both sides, unless we choose not to do
        if (!modificationInfos.isConnected1()) {
            branch.getTerminal1().disconnect();
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.terminal1Disconnected")
                    .withUntypedValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        if (!modificationInfos.isConnected2()) {
            branch.getTerminal2().disconnect();
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.terminal2Disconnected")
                    .withUntypedValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }

    public Identifiable<?> getEquipmentByIdentifiableType(Network network, IdentifiableType type, String equipmentId) {
        if (type == null || equipmentId == null) {
            return null;
        }

        return switch (type) {
            case HVDC_LINE -> network.getHvdcLine(equipmentId);
            case LINE -> network.getLine(equipmentId);
            case TWO_WINDINGS_TRANSFORMER -> network.getTwoWindingsTransformer(equipmentId);
            case THREE_WINDINGS_TRANSFORMER -> network.getThreeWindingsTransformer(equipmentId);
            case GENERATOR -> network.getGenerator(equipmentId);
            case LOAD -> network.getLoad(equipmentId);
            case BATTERY -> network.getBattery(equipmentId);
            case SHUNT_COMPENSATOR -> network.getShuntCompensator(equipmentId);
            case STATIC_VAR_COMPENSATOR -> network.getStaticVarCompensator(equipmentId);
            case DANGLING_LINE -> network.getDanglingLine(equipmentId);
            case HVDC_CONVERTER_STATION -> network.getHvdcConverterStation(equipmentId);
            case SUBSTATION -> network.getSubstation(equipmentId);
            case VOLTAGE_LEVEL -> network.getVoltageLevel(equipmentId);
            case BUSBAR_SECTION -> network.getBusbarSection(equipmentId);
            default -> null;
        };
    }

    /**
     * @param reportNode Limit set report node
     * @param opLimitsGroup added current limits
     * @param branch branch to which limits are going to be added
     * @param side which side of the branch receives the limits
     */
    public void setCurrentLimitsOnASide(ReportNode reportNode, OperationalLimitsGroupInfos opLimitsGroup, Branch<?> branch, TwoSides side) {

        boolean hasPermanent = opLimitsGroup.getCurrentLimits().getPermanentLimit() != null;
        boolean hasTemporary = !CollectionUtils.isEmpty(opLimitsGroup.getCurrentLimits().getTemporaryLimits());
        boolean hasLimits = hasPermanent || hasTemporary;

        if (!hasPermanent && !hasLimits || opLimitsGroup.getId() == null) {
            return;
        }
        ReportNode limitSetReportDetail = reportNode.newReportNode()
                .withMessageTemplate(OPERATIONAL_LIMITS_GROUP_ADDED_LOG_DETAIL)
                .withUntypedValue(SIDE, applicabilityToString(side == ONE ? SIDE1 : SIDE2))
                .withSeverity(TypedValue.DETAIL_SEVERITY).add();

        OperationalLimitsGroup opGroup = side == ONE
                ? branch.newOperationalLimitsGroup1(opLimitsGroup.getId())
                : branch.newOperationalLimitsGroup2(opLimitsGroup.getId());
        List<ReportNode> detailsOnLimitSet = new ArrayList<>();

        CurrentLimitsAdder limitsAdder = opGroup.newCurrentLimits();
        if (hasPermanent) {
            limitsAdder.setPermanentLimit(opLimitsGroup.getCurrentLimits().getPermanentLimit());
            detailsOnLimitSet.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate("network.modification.permanentLimit")
                    .withUntypedValue(VALUE, opLimitsGroup.getCurrentLimits().getPermanentLimit())
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build());
        }
        if (hasTemporary) {
            opLimitsGroup.getCurrentLimits().getTemporaryLimits().forEach(limit -> {
                double value = limit.getValue() != null ? limit.getValue() : Double.MAX_VALUE;
                int duration = limit.getAcceptableDuration() != null ? limit.getAcceptableDuration() : Integer.MAX_VALUE;
                detailsOnLimitSet.add(ReportNode.newRootReportNode()
                                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                                .withMessageTemplate("network.modification.temporaryLimitModified.name")
                                .withUntypedValue(AbstractBranchModification.NAME, limit.getName())
                                .withUntypedValue(DURATION, duration)
                                .withUntypedValue(AbstractBranchModification.VALUE, value)
                                .withSeverity(TypedValue.DETAIL_SEVERITY)
                                .build());

                limitsAdder.beginTemporaryLimit()
                        .setName(limit.getName())
                        .setValue(value)
                        .setAcceptableDuration(duration)
                        .endTemporaryLimit();
            });
        }
        limitsAdder.add();

        //add properties
        if (!CollectionUtils.isEmpty(opLimitsGroup.getLimitsProperties()) &&
            checkPropertiesUnicity(opLimitsGroup.getLimitsProperties(), detailsOnLimitSet)) {
            opLimitsGroup.getLimitsProperties().forEach(property -> {
                detailsOnLimitSet.add(
                        ReportNode.newRootReportNode().withSeverity(TypedValue.DETAIL_SEVERITY)
                        .withMessageTemplate("network.modification.propertyAdded")
                        .withUntypedValue(NAME, property.name())
                        .withUntypedValue(VALUE, property.value()).build());
                opGroup.setProperty(property.name(), property.value());
            });
        }
        if (!detailsOnLimitSet.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(limitSetReportDetail, detailsOnLimitSet);
        }
    }

    private static boolean checkPropertiesUnicity(List<LimitsPropertyInfos> properties, List<ReportNode> reportNodeList) {

        if (CollectionUtils.isEmpty(properties)) {
            return true;
        }
        boolean unicity = true;
        for (LimitsPropertyInfos property : properties) {
            if (properties.stream().filter(prop -> prop.name().equals(property.name())).toList().size() > 1) {
                reportNodeList.add(ReportNode.newRootReportNode().withSeverity(TypedValue.ERROR_SEVERITY)
                    .withMessageTemplate("network.modification.propertyNameUnique")
                    .withUntypedValue(NAME, property.name()).build());
                unicity = false;
            }
        }
        return unicity;
    }

    public <T> ReportNode buildCreationReport(T value, String fieldName) {
        String newValueString = value == null ? NO_VALUE : value.toString();
        return ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("network.modification.creation.fieldName")
                .withUntypedValue("fieldName", fieldName)
                .withUntypedValue("value", newValueString)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build();
    }

    public <T> void reportElementaryCreation(ReportNode subReportNode, T value, String fieldName) {
        insertReportNode(subReportNode, buildCreationReport(value, fieldName));
    }

    public String formatRegulationModeReport(PhaseTapChanger.RegulationMode regulationMode) {
        return switch (regulationMode) {
            case CURRENT_LIMITER -> "    Current limiter";
            case ACTIVE_POWER_CONTROL -> "    Active power control";
        };
    }

    public void modifyReactiveCapabilityCurvePoints(Collection<ReactiveCapabilityCurve.Point> points,
                                                    List<ReactiveCapabilityCurvePointsInfos> modificationPoints,
                                                    ReactiveCapabilityCurveAdder adder,
                                                    ReportNode subReportNode, ReportNode subReportNodeLimits) {
        List<ReportNode> reports = new ArrayList<>();
        List<ReactiveCapabilityCurve.Point> equipementIdPoints = new ArrayList<>(points);
        IntStream.range(0, modificationPoints.size())
                .forEach(i -> {
                    String fieldSuffix;
                    ReactiveCapabilityCurve.Point oldPoint = i < equipementIdPoints.size() - 1 ? equipementIdPoints.get(i) : null;
                    ReactiveCapabilityCurvePointsInfos newPoint = modificationPoints.get(i);
                    if (i == 0) {
                        fieldSuffix = "min";
                    } else if (i == (modificationPoints.size() - 1)) {
                        fieldSuffix = "max";
                        if (!CollectionUtils.isEmpty(equipementIdPoints)) {
                            oldPoint = equipementIdPoints.get(equipementIdPoints.size() - 1);
                        }
                    } else {
                        fieldSuffix = Integer.toString(i);
                    }
                    createReactiveCapabilityCurvePoint(adder, newPoint, oldPoint, reports, fieldSuffix);
                });
        adder.add();
        ReportNode subReportNodeReactiveLimits = null;
        ReportNode subReporterLimits2 = subReportNodeLimits;
        if (!reports.isEmpty()) {
            if (subReportNodeLimits == null) {
                subReporterLimits2 = subReportNode.newReportNode().withMessageTemplate(LIMITS).add();
            }
            if (subReporterLimits2 != null) {
                subReportNodeReactiveLimits = subReporterLimits2.newReportNode().withMessageTemplate(REACTIVE_LIMITS).add();
            }
        }
        reportModifications(subReportNodeReactiveLimits, reports, "network.modification.curveReactiveLimitsModified");
    }

    public void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder,
                                                    ReactiveCapabilityCurvePointsInfos newPoint,
                                                    ReactiveCapabilityCurve.Point oldPoint,
                                                    List<ReportNode> reports,
                                                    String fieldSuffix) {
        Double oldMaxQ = Double.NaN;
        Double oldMinQ = Double.NaN;
        Double oldP = Double.NaN;
        if (oldPoint != null) {
            oldMaxQ = oldPoint.getMaxQ();
            oldMinQ = oldPoint.getMinQ();
            oldP = oldPoint.getP();
        }
        var maxQ = newPoint.getMaxQ() != null ? newPoint.getMaxQ() : oldMaxQ;
        var minQ = newPoint.getMinQ() != null ? newPoint.getMinQ() : oldMinQ;
        var p = newPoint.getP() != null ? newPoint.getP() : oldP;

        adder.beginPoint()
                .setMaxQ(maxQ)
                .setMinQ(minQ)
                .setP(p)
                .endPoint();
        addToReports(reports, p, oldP, "P" + fieldSuffix);
        addToReports(reports, minQ, oldMinQ, "QminP" + fieldSuffix);
        addToReports(reports, maxQ, oldMaxQ, "QmaxP" + fieldSuffix);
    }

    public void addToReports(List<ReportNode> reports, Double newValue, Double oldValue, String fieldName) {
        if (newValue != null) {
            reports.add(buildModificationReport(oldValue, newValue, fieldName));
        }
    }

    public void modifyMinMaxReactiveLimits(AttributeModification<Double> minimumReactivePower, AttributeModification<Double> maximumReactivePower, ReactiveLimitsHolder reactiveLimitsHolder,
                                           ReportNode subReportNode, ReportNode subReportNodeLimits) {
        MinMaxReactiveLimits minMaxReactiveLimits = null;
        ReactiveLimits reactiveLimits = reactiveLimitsHolder.getReactiveLimits();
        MinMaxReactiveLimitsAdder newMinMaxReactiveLimitsAdder = reactiveLimitsHolder.newMinMaxReactiveLimits();
        if (reactiveLimits != null) {
            ReactiveLimitsKind limitsKind = reactiveLimits.getKind();
            if (limitsKind == ReactiveLimitsKind.MIN_MAX) {
                minMaxReactiveLimits = reactiveLimitsHolder.getReactiveLimits(MinMaxReactiveLimits.class);
            }
        }
        modifyMinMaxReactiveLimits(minMaxReactiveLimits,
                newMinMaxReactiveLimitsAdder, subReportNode, subReportNodeLimits,
                minimumReactivePower,
                maximumReactivePower);
    }

    public void modifyMinMaxReactiveLimits(MinMaxReactiveLimits minMaxReactiveLimits, MinMaxReactiveLimitsAdder newMinMaxReactiveLimits,
                                           ReportNode subReportNode, ReportNode subReportNodeLimits, AttributeModification<Double> minimumReactivePower, AttributeModification<Double> maximumReactivePower) {
        List<ReportNode> reports = new ArrayList<>();

        if (minimumReactivePower != null
                && maximumReactivePower != null) {
            newMinMaxReactiveLimits.setMinQ(minimumReactivePower.getValue())
                    .setMaxQ(maximumReactivePower.getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    minimumReactivePower.getValue(),
                    MIN_REACTIVE_POWER_FIELDNAME));
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    maximumReactivePower.getValue(),
                    MAX_REACTIVE_POWER_FIELDNAME));
        } else if (minimumReactivePower != null) {
            newMinMaxReactiveLimits.setMinQ(minimumReactivePower.getValue())
                    .setMaxQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.MAX_VALUE)
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    minimumReactivePower.getValue(),
                    MIN_REACTIVE_POWER_FIELDNAME));
        } else if (maximumReactivePower != null) {
            newMinMaxReactiveLimits
                    .setMinQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : -Double.MAX_VALUE)
                    .setMaxQ(maximumReactivePower.getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    maximumReactivePower.getValue(),
                    MAX_REACTIVE_POWER_FIELDNAME));
        } else if (minMaxReactiveLimits == null) {
            newMinMaxReactiveLimits.setMinQ(-Double.MAX_VALUE)
                    .setMaxQ(Double.MAX_VALUE)
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(Double.NaN,
                    -Double.MAX_VALUE,
                    MIN_REACTIVE_POWER_FIELDNAME));
            reports.add(ModificationUtils.getInstance().buildModificationReport(Double.NaN,
                    Double.MAX_VALUE,
                    MAX_REACTIVE_POWER_FIELDNAME));
        }
        ReportNode subReportNodeReactiveLimits = null;
        ReportNode subReportNodeLimits2 = subReportNodeLimits;
        if (subReportNodeLimits == null && !reports.isEmpty()) {
            subReportNodeLimits2 = subReportNode.newReportNode().withMessageTemplate(LIMITS).add();
        }
        if (subReportNodeLimits2 != null && !reports.isEmpty()) {
            subReportNodeReactiveLimits = subReportNodeLimits2.newReportNode().withMessageTemplate(REACTIVE_LIMITS).add();
        }
        reportModifications(subReportNodeReactiveLimits, reports, "network.modification.minMaxReactiveLimitsModified");
    }

    private void modifyExistingActivePowerControl(ActivePowerControl<?> activePowerControl,
                                                  AttributeModification<Boolean> participateInfo,
                                                  AttributeModification<Float> droopInfo,
                                                  List<ReportNode> reports) {
        double oldDroop = activePowerControl.getDroop();
        boolean oldParticipate = activePowerControl.isParticipate();

        Optional.ofNullable(participateInfo).ifPresent(info -> {
            activePowerControl.setParticipate(info.getValue());
            if (reports != null) {
                reports.add(buildModificationReport(oldParticipate, info.getValue(), PARTICIPATE));
            }
        });

        Optional.ofNullable(droopInfo).ifPresent(info -> {
            activePowerControl.setDroop(info.getValue());
            if (reports != null) {
                reports.add(buildModificationReport(oldDroop, info.getValue(), DROOP));
            }
        });
    }

    private void createNewActivePowerControl(ActivePowerControlAdder<?> adder,
                                             AttributeModification<Boolean> participateInfo,
                                             AttributeModification<Float> droopInfo,
                                             List<ReportNode> reports,
                                             NetworkModificationException.Type exceptionType,
                                             String errorMessage) {
        Boolean participate = Optional.ofNullable(participateInfo).map(AttributeModification::getValue).orElse(null);
        Float droop = Optional.ofNullable(droopInfo).map(AttributeModification::getValue).orElse(null);
        checkActivePowerControl(participate, droop, exceptionType, errorMessage);
        if (participate != null) {
            adder.withParticipate(participate)
                .withDroop(droop != null ? droop : Double.NaN)
                .add();
            if (reports != null) {
                reports.add(buildModificationReport(null, participate, PARTICIPATE));
                reports.add(buildModificationReport(Double.NaN, droop, DROOP));
            }
        }
    }

    public void checkActivePowerControl(Boolean participate, Float droop, NetworkModificationException.Type exceptionType, String errorMessage) {
        if (Boolean.TRUE.equals(participate) && droop == null) {
            throw new NetworkModificationException(exceptionType, String.format("%s Active power regulation on : missing required droop value", errorMessage));
        }
    }

    public ReportNode modifyActivePowerControlAttributes(ActivePowerControl<?> activePowerControl,
                                                         ActivePowerControlAdder<?> activePowerControlAdder,
                                                         AttributeModification<Boolean> participateInfo,
                                                         AttributeModification<Float> droopInfo,
                                                         ReportNode subReportNode,
                                                         ReportNode subReporterSetpoints,
                                                         NetworkModificationException.Type exceptionType,
                                                         String errorMessage) {
        List<ReportNode> reports = new ArrayList<>();
        if (activePowerControl != null) {
            modifyExistingActivePowerControl(activePowerControl, participateInfo, droopInfo, reports);
        } else {
            createNewActivePowerControl(activePowerControlAdder, participateInfo, droopInfo, reports, exceptionType, errorMessage);
        }
        if (subReportNode != null) {
            ReportNode subReportNodeSetpoints2 = subReporterSetpoints;
            if (subReporterSetpoints == null && !reports.isEmpty()) {
                subReportNodeSetpoints2 = subReportNode.newReportNode().withMessageTemplate(SETPOINTS).add();
            }
            reportModifications(subReportNodeSetpoints2, reports, "network.modification.activePowerControlModified");
            return subReportNodeSetpoints2;
        }
        return null;
    }

    public void checkMaxQGreaterThanMinQ(
            List<ReactiveCapabilityCurvePointsInfos> modificationPoints,
            NetworkModificationException.Type exceptionType, String errorMessage
    ) {
        for (var point : modificationPoints) {
            double maxQ = Double.NaN;
            double minQ = Double.NaN;

            if (point.getMaxQ() != null) {
                maxQ = point.getMaxQ();
            }

            if (point.getMinQ() != null) {
                minQ = point.getMinQ();
            }

            if (maxQ < minQ) {
                throw new NetworkModificationException(
                    exceptionType,
                    errorMessage + "maximum reactive power " + maxQ + " is expected to be greater than or equal to minimum reactive power " + minQ
                );
            }
        }
    }

    public void checkMaxReactivePowerGreaterThanMinReactivePower(MinMaxReactiveLimits minMaxReactiveLimits, AttributeModification<Double> minimumReactivePowerInfo, AttributeModification<Double> maximumReactivePowerInfo, NetworkModificationException.Type exceptionType, String errorMessage) {
        double previousMinimumReactivePower = minMaxReactiveLimits.getMinQ();
        double previousMaximumReactivePower = minMaxReactiveLimits.getMaxQ();
        double minReactivePower = minimumReactivePowerInfo != null ? minimumReactivePowerInfo.getValue() : previousMinimumReactivePower;
        double maxReactivePower = maximumReactivePowerInfo != null ? maximumReactivePowerInfo.getValue() : previousMaximumReactivePower;
        if (minReactivePower > maxReactivePower) {
            throw new NetworkModificationException(exceptionType, errorMessage + "maximum reactive power " + maxReactivePower + " is expected to be greater than or equal to minimum reactive power " + minReactivePower);
        }
    }

    public void checkReactiveLimit(ReactiveLimitsHolder reactiveLimitsHolder, AttributeModification<Double> minimumReactivePower, AttributeModification<Double> maximumReactivePower,
                                   List<ReactiveCapabilityCurvePointsInfos> modificationPoints, NetworkModificationException.Type exeptionType, String errorMessage) {
        if (reactiveLimitsHolder.getReactiveLimits().getKind() == ReactiveLimitsKind.MIN_MAX
                && (minimumReactivePower != null || maximumReactivePower != null)) {
            MinMaxReactiveLimits minMaxReactiveLimits = reactiveLimitsHolder.getReactiveLimits(MinMaxReactiveLimits.class);
            ModificationUtils.getInstance().checkMaxReactivePowerGreaterThanMinReactivePower(minMaxReactiveLimits, minimumReactivePower, maximumReactivePower, exeptionType, errorMessage);
        }
        if (modificationPoints != null) {
            ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(modificationPoints, exeptionType, errorMessage);
        }
    }

    public void checkEnableRegulation(AttributeModification<VoltageRegulationType> voltageRegulationType,
                                         AttributeModification<String> regulatingTerminalId,
                                         AttributeModification<String> regulatingTerminalType,
                                         AttributeModification<String> regulatingTerminalVlId,
                                         Terminal localTerminal,
                                         Terminal oldRegulatingTerminal,
                                         Network network,
                                         NetworkModificationException.Type exceptionType,
                                         String errorMessage) {
        // checking if regulating is set to distant
        if (voltageRegulationType == null || voltageRegulationType.getValue().equals(VoltageRegulationType.LOCAL)) {
            return;
        }
        boolean isRegulatingTerminalInfoMissing =
            (regulatingTerminalType == null || regulatingTerminalType.getValue() == null) &&
            (regulatingTerminalVlId == null || regulatingTerminalVlId.getValue() == null) &&
            (regulatingTerminalId == null || regulatingTerminalId.getValue() == null);
        boolean isRegulatingTerminalInfoIncomplete =
            regulatingTerminalType == null || regulatingTerminalType.getValue() == null
            || regulatingTerminalVlId == null || regulatingTerminalVlId.getValue() == null
            || regulatingTerminalId == null || regulatingTerminalId.getValue() == null;
        if (isRegulatingTerminalInfoMissing) {
            if (oldRegulatingTerminal == null) {
                // all modifications are null
                // and regulating terminal is null
                // the regulation should be local or regulating terminal modifications must be provided
                throw new NetworkModificationException(exceptionType, errorMessage + "Regulation is set to Distant but regulating terminal is missing");
            } else if (oldRegulatingTerminal.equals(localTerminal)) {
                // all modifications are null
                // and regulating terminal is local
                // the regulation should be local or regulating terminal modifications must be provided
                throw new NetworkModificationException(exceptionType, errorMessage + "Regulation is set to Distant but regulating terminal is local and there is no modification about regulating terminal");
            }
            // all modifications are null but oldRegulatingTerminal is not
            // we will retrieve the old regulating terminal
        } else if (isRegulatingTerminalInfoIncomplete) {
            // at least one information about new regulating terminal is null
            // meaning regulating terminal modification information is incomplete
            throw new NetworkModificationException(exceptionType, errorMessage + "Regulation is set to Distant but regulating terminal information are incomplete");
        } else {
            // regulating terminal modification information is complete
            // check if the regulating terminal exists
            getTerminalFromIdentifiable(network,
                regulatingTerminalId.getValue(),
                regulatingTerminalType.getValue(),
                regulatingTerminalVlId.getValue());
        }
    }

    public void checkActivePowerZeroOrBetweenMinAndMaxActivePower(AttributeModification<Double> activePowerInfos, AttributeModification<Double> minActivePowerInfos, AttributeModification<Double> maxActivePowerInfos, Double previousMinActivePower, Double previousMaxActivePower, Double previousActivePower, NetworkModificationException.Type exceptionType, String errorMessage) {
        Double minActivePower = minActivePowerInfos != null ? minActivePowerInfos.getValue() : previousMinActivePower;
        Double maxActivePower = maxActivePowerInfos != null ? maxActivePowerInfos.getValue() : previousMaxActivePower;
        Double activePower = activePowerInfos != null ? activePowerInfos.getValue() : previousActivePower;

        if (activePower != 0 && (activePower < minActivePower || activePower > maxActivePower)) {
            throw new NetworkModificationException(exceptionType, errorMessage + "Active power " + activePower + " is expected to be equal to 0 or within the range of minimum active power and maximum active power: [" + minActivePower + ", " + maxActivePower + "]");
        }
    }

    private NetworkModificationException makeEquipmentException(NetworkModificationException.Type errorType,
                                                                       String equipmentId,
                                                                       String equipmentName,
                                                                       String msgSuffix) {
        return new NetworkModificationException(errorType,
                equipmentName + " '" + equipmentId + "' : " + msgSuffix);
    }

    private void checkReactiveCapabilityCurvePoints(List<ReactiveCapabilityCurvePointsInfos> points,
                                                    NetworkModificationException.Type errorType,
                                                    String equipmentId,
                                                    String equipmentName) {
        if (points.size() < 2) {
            throw makeEquipmentException(errorType, equipmentId, equipmentName, "a reactive capability curve should have at least two points");
        }
        IntStream.range(0, points.size())
                .forEach(i -> {
                    ReactiveCapabilityCurvePointsInfos newPoint = points.get(i);
                    if (newPoint.getP() == null || Double.isNaN(newPoint.getP())) {
                        throw makeEquipmentException(errorType, equipmentId, equipmentName, "P is not set in a reactive capability curve limits point");
                    } else if (newPoint.getMinQ() == null || Double.isNaN(newPoint.getMinQ())) {
                        throw makeEquipmentException(errorType, equipmentId, equipmentName, "min Q is not set in a reactive capability curve limits point");
                    } else if (newPoint.getMaxQ() == null || Double.isNaN(newPoint.getMaxQ())) {
                        throw makeEquipmentException(errorType, equipmentId, equipmentName, "max Q is not set in a reactive capability curve limits point");
                    }
                });
    }

    public void checkReactiveLimitsCreation(ReactiveLimitsHolderInfos modificationInfos,
                                            NetworkModificationException.Type errorType,
                                            String equipmentId,
                                            String equipmentName) {
        // check min max reactive limits
        if (modificationInfos.getMinQ() != null && modificationInfos.getMaxQ() != null) {
            if (Double.isNaN(modificationInfos.getMinQ())) {
                throw makeEquipmentException(errorType, equipmentId, equipmentName, "minimum reactive power is not set");
            } else if (Double.isNaN(modificationInfos.getMaxQ())) {
                throw makeEquipmentException(errorType, equipmentId, equipmentName, "maximum reactive power is not set");
            } else if (modificationInfos.getMaxQ() < modificationInfos.getMinQ()) {
                throw makeEquipmentException(errorType, equipmentId, equipmentName, "maximum reactive power is expected to be greater than or equal to minimum reactive power");
            }
        }

        // check reactive capability curve limits
        List<ReactiveCapabilityCurvePointsInfos> points = modificationInfos.getReactiveCapabilityCurvePoints();
        if (!org.apache.commons.collections4.CollectionUtils.isEmpty(points)) {
            checkReactiveCapabilityCurvePoints(points, errorType, equipmentId, equipmentName);
        }
    }

    public void checkReactivePowerLimitsAndSetPointsCreation(StaticVarCompensatorCreationInfos creationInfos) {
        String equipmentName = "StaticVarCompensator";
        // check min max reactive limits
        if (Objects.isNull(creationInfos.getMinSusceptance()) && Objects.isNull(creationInfos.getMinQAtNominalV())) {
            throw makeEquipmentException(creationInfos.getErrorType(), creationInfos.getEquipmentId(), equipmentName, "minimum susceptance is not set");
        }
        if (Objects.isNull(creationInfos.getMaxSusceptance()) && Objects.isNull(creationInfos.getMaxQAtNominalV())) {
            throw makeEquipmentException(creationInfos.getErrorType(), creationInfos.getEquipmentId(), equipmentName, "maximum susceptance is not set");
        }
        if (Objects.nonNull(creationInfos.getMaxSusceptance()) && Objects.nonNull(creationInfos.getMinSusceptance()) && creationInfos.getMaxSusceptance() < creationInfos.getMinSusceptance()) {
            throw makeEquipmentException(creationInfos.getErrorType(), creationInfos.getEquipmentId(), equipmentName, "maximum susceptance is expected to be greater than or equal to minimum susceptance");
        }
        if (Objects.nonNull(creationInfos.getMaxQAtNominalV()) && Objects.nonNull(creationInfos.getMinQAtNominalV()) && creationInfos.getMaxQAtNominalV() < creationInfos.getMinQAtNominalV()) {
            throw makeEquipmentException(creationInfos.getErrorType(), creationInfos.getEquipmentId(), equipmentName, "maximum Q at nominal voltage is expected to be greater than or equal to minimum Q");
        }

        // check set points
        if (creationInfos.getRegulationMode() == StaticVarCompensator.RegulationMode.VOLTAGE && creationInfos.getVoltageSetpoint() == null) {
            throw makeEquipmentException(creationInfos.getErrorType(), creationInfos.getEquipmentId(), equipmentName, "Voltage setpoint is not set");
        }
        if (creationInfos.getRegulationMode() == StaticVarCompensator.RegulationMode.REACTIVE_POWER && creationInfos.getReactivePowerSetpoint() == null) {
            throw makeEquipmentException(creationInfos.getErrorType(), creationInfos.getEquipmentId(), equipmentName, "Reactive power setpoint is not set");
        }
    }

    public void checkStandbyAutomatonCreation(StaticVarCompensatorCreationInfos creationInfos) {
        String equipmentName = "StaticVarCompensator";
        if (creationInfos.isStandby() && creationInfos.getRegulationMode() != StaticVarCompensator.RegulationMode.VOLTAGE && !creationInfos.isRegulating()) {
            throw makeEquipmentException(creationInfos.getErrorType(), creationInfos.getEquipmentId(), equipmentName, "Standby is only supported in Voltage Regulation mode");
        }
        if (Objects.nonNull(creationInfos.getB0()) && Objects.nonNull(creationInfos.getMinSusceptance()) && Objects.nonNull(creationInfos.getMaxSusceptance()) &&
                (creationInfos.getB0() < creationInfos.getMinSusceptance() || creationInfos.getB0() > creationInfos.getMaxSusceptance())) {
            throw makeEquipmentException(creationInfos.getErrorType(), creationInfos.getEquipmentId(), equipmentName,
                     "b0 must be within the range of minimum susceptance and maximum susceptance");
        }
        if (Objects.nonNull(creationInfos.getQ0()) && Objects.nonNull(creationInfos.getMinQAtNominalV()) && Objects.nonNull(creationInfos.getMaxQAtNominalV()) &&
                (creationInfos.getQ0() < creationInfos.getMinQAtNominalV() || creationInfos.getQ0() > creationInfos.getMaxQAtNominalV())) {
            throw makeEquipmentException(creationInfos.getErrorType(), creationInfos.getEquipmentId(), equipmentName,
                    "q0 must be within the range of minimum Q and maximum Q");
        }
    }

    public static void addToReports(List<ReportNode> reports, Double newValue, String fieldName) {
        if (newValue != null) {
            reports.add(ModificationUtils.getInstance().buildCreationReport(newValue, fieldName));
        }
    }

    public void createReactiveLimits(ReactiveLimitsHolderInfos creationInfos,
                                            ReactiveLimitsHolder reactiveLimitsHolder,
                                     ReportNode subReporter) {
        if (Boolean.TRUE.equals(creationInfos.getReactiveCapabilityCurve())) {
            createReactiveCapabilityCurve(creationInfos, reactiveLimitsHolder, subReporter);
        } else if (Boolean.FALSE.equals(creationInfos.getReactiveCapabilityCurve())) {
            createMinMaxReactiveLimits(creationInfos, reactiveLimitsHolder, subReporter);
        }
    }

    public void createMinMaxReactiveLimits(ReactiveLimitsHolderInfos batteryCreationInfos,
                                                  ReactiveLimitsHolder reactiveLimitsHolder,
                                           ReportNode subReportNode) {
        List<ReportNode> minMaxReactiveLimitsReports = new ArrayList<>();
        if (batteryCreationInfos.getMinQ() != null && batteryCreationInfos.getMaxQ() != null) {
            reactiveLimitsHolder.newMinMaxReactiveLimits()
                    .setMinQ(batteryCreationInfos.getMinQ())
                    .setMaxQ(batteryCreationInfos.getMaxQ())
                    .add();

            minMaxReactiveLimitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                    batteryCreationInfos.getMinQ(),
                    MIN_REACTIVE_POWER_FIELDNAME));

            minMaxReactiveLimitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                    batteryCreationInfos.getMaxQ(),
                    MAX_REACTIVE_POWER_FIELDNAME));

            ReportNode subReporterReactiveLimits = subReportNode.newReportNode().withMessageTemplate(REACTIVE_LIMITS).add();

            ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, minMaxReactiveLimitsReports, "network.modification.minMaxReactiveLimitsCreated");
        }
    }

    public void createReactiveCapabilityCurve(ReactiveLimitsHolderInfos creationInfos,
                                                     ReactiveLimitsHolder reactiveLimitsHolder,
                                              ReportNode subReportNode) {
        List<ReportNode> pointsReports = new ArrayList<>();
        ReactiveCapabilityCurveAdder adder = reactiveLimitsHolder.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurvePointsInfos> points = creationInfos.getReactiveCapabilityCurvePoints();
        IntStream.range(0, points.size())
                .forEach(i -> {
                    String fieldSuffix;
                    ReactiveCapabilityCurvePointsInfos newPoint = points.get(i);
                    if (i == 0) {
                        fieldSuffix = "min";
                    } else if (i == (points.size() - 1)) {
                        fieldSuffix = "max";
                    } else {
                        fieldSuffix = Integer.toString(i - 1);
                    }
                    createReactiveCapabilityCurvePoint(adder, newPoint, pointsReports, fieldSuffix);
                });
        adder.add();
        ReportNode subReporterReactiveLimits = subReportNode.newReportNode().withMessageTemplate(REACTIVE_LIMITS).add();
        ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, pointsReports, "network.modification.curveReactiveLimitsCreated");
    }

    private void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder,
                                                           ReactiveCapabilityCurvePointsInfos point,
                                                           List<ReportNode> reports,
                                                           String fieldSuffix) {
        adder.beginPoint()
                .setMaxQ(point.getMaxQ())
                .setMinQ(point.getMinQ())
                .setP(point.getP())
                .endPoint();
        addToReports(reports, point.getP(), "P" + fieldSuffix);
        addToReports(reports, point.getMinQ(), "QminP" + fieldSuffix);
        addToReports(reports, point.getMaxQ(), "QmaxP" + fieldSuffix);
    }

    public boolean isValidFilter(ReportNode subReportNode,
                                 NetworkModificationException.Type errorType,
                                 Map<UUID, FilterEquipments> exportFilters) {
        boolean noValidEquipmentId = exportFilters.values().stream()
                .allMatch(filterEquipments -> CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes()));

        if (noValidEquipmentId) {
            createReport(subReportNode, "network.modification.invalidFilters", Map.of("errorType", errorType), TypedValue.ERROR_SEVERITY);
            return false;
        }

        return true;
    }

    public static Set<IdentifiableAttributes> getIdentifiableAttributes(Map<UUID, FilterEquipments> exportFilters, List<FilterInfos> filterInfos, ReportNode subReportNode) {
        filterInfos.stream()
                .filter(f -> !exportFilters.containsKey(f.getId()))
                .forEach(f -> createReport(subReportNode, "network.modification.filterNotFound", Map.of("name", f.getName()), TypedValue.WARN_SEVERITY));

        return filterInfos
                .stream()
                .filter(f -> exportFilters.containsKey(f.getId()))
                .flatMap(f -> exportFilters.get(f.getId())
                        .getIdentifiableAttributes()
                        .stream())
                .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    @Nullable
    public static Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(IFilterService filterService, Network network, ReportNode subReportNode, Map<UUID, String> filters, NetworkModificationException.Type errorType) {
        Map<UUID, FilterEquipments> exportFilters = filterService.getUuidFilterEquipmentsMap(network, filters);

        boolean isValidFilter = ModificationUtils.getInstance().isValidFilter(subReportNode, errorType, exportFilters);
        return isValidFilter ? exportFilters : null;
    }

    public static void logWrongEquipmentsIdsFilters(ReportNode subReportNode, Map<UUID, FilterEquipments> exportFilters, Map<UUID, String> filters) {
        // collect logs for all filters with wrong equipments ids
        exportFilters.entrySet().stream()
                .filter(e -> !CollectionUtils.isEmpty(e.getValue().getNotFoundEquipments()))
                .forEach(f -> {
                    FilterEquipments filterEquipments = f.getValue();
                    var equipmentIds = String.join(", ", filterEquipments.getNotFoundEquipments());
                    createReport(subReportNode,
                            "network.modification.filterEquipmentsNotFound.inFilter",
                            Map.of("equipmentIds", equipmentIds, "filters", filters.get(filterEquipments.getFilterId())), TypedValue.WARN_SEVERITY);
                });
    }

    public static void insertReportNode(ReportNode parent, ReportNode child) {
        ReportNodeAdder adder = parent.newReportNode().withMessageTemplate(child.getMessageKey());
        for (Map.Entry<String, TypedValue> valueEntry : child.getValues().entrySet()) {
            adder.withUntypedValue(valueEntry.getKey(), valueEntry.getValue().toString());
        }
        child.getValue(ReportConstants.SEVERITY_KEY).ifPresent(adder::withSeverity);
        ReportNode insertedChild = adder.add();
        if (child.getChildren() != null) {
            child.getChildren().forEach(grandChild -> insertReportNode(insertedChild, grandChild));
        }
    }

    public static void createInjectionInNodeBreaker(VoltageLevel voltageLevel, InjectionCreationInfos injectionCreationInfos,
                                                         Network network, InjectionAdder<?, ?> injectionAdder, ReportNode subReportNode) {
        int position = ModificationUtils.getInstance().getPosition(injectionCreationInfos.getConnectionPosition(),
                injectionCreationInfos.getBusOrBusbarSectionId(), network, voltageLevel);
        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBusOrBusbarSectionId(injectionCreationInfos.getBusOrBusbarSectionId())
                .withInjectionDirection(injectionCreationInfos.getConnectionDirection())
                .withInjectionFeederName(injectionCreationInfos.getConnectionName() != null
                        ? injectionCreationInfos.getConnectionName()
                        : injectionCreationInfos.getEquipmentId())
                .withInjectionPositionOrder(position)
                .withInjectionAdder(injectionAdder)
                .withLogOrThrowIfIncorrectPositionOrder(false)
                .build();
        algo.apply(network, true, subReportNode);
    }

    public static void createBranchInNodeBreaker(VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, BranchCreationInfos branchCreationInfos,
                                                    Network network, BranchAdder<?, ?> branchAdder, ReportNode subReportNode) {
        var position1 = ModificationUtils.getInstance().getPosition(branchCreationInfos.getConnectionPosition1(), branchCreationInfos.getBusOrBusbarSectionId1(), network, voltageLevel1);
        var position2 = ModificationUtils.getInstance().getPosition(branchCreationInfos.getConnectionPosition2(), branchCreationInfos.getBusOrBusbarSectionId2(), network, voltageLevel2);

        CreateBranchFeederBays algo = new CreateBranchFeederBaysBuilder()
                .withBusOrBusbarSectionId1(branchCreationInfos.getBusOrBusbarSectionId1())
                .withBusOrBusbarSectionId2(branchCreationInfos.getBusOrBusbarSectionId2())
                .withFeederName1(branchCreationInfos.getConnectionName1() != null ? branchCreationInfos.getConnectionName1() : branchCreationInfos.getEquipmentId())
                .withFeederName2(branchCreationInfos.getConnectionName2() != null ? branchCreationInfos.getConnectionName2() : branchCreationInfos.getEquipmentId())
                .withDirection1(branchCreationInfos.getConnectionDirection1())
                .withDirection2(branchCreationInfos.getConnectionDirection2())
                .withPositionOrder1(position1)
                .withPositionOrder2(position2)
                .withBranchAdder(branchAdder).build();
        algo.apply(network, true, subReportNode);
    }

    public static void reportInjectionCreationConnectivity(InjectionCreationInfos injectionCreationInfos, ReportNode subReporter) {
        if (Objects.isNull(injectionCreationInfos.getVoltageLevelId()) || Objects.isNull(injectionCreationInfos.getBusOrBusbarSectionId())) {
            return;
        }

        List<ReportNode> connectivityReports = buildConnectivityReports(
                injectionCreationInfos.getConnectionName(),
                injectionCreationInfos.getConnectionDirection(),
                injectionCreationInfos.getConnectionPosition(),
                injectionCreationInfos.isTerminalConnected(),
                injectionCreationInfos.getEquipmentId(),
                FeederSide.INJECTION_SINGLE_SIDE
        );

        if (!connectivityReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReporter, connectivityReports, "network.modification.ConnectivityCreated");
        }
    }

    public static void reportBranchCreationConnectivity(BranchCreationInfos branchCreationInfos, ReportNode subReporter) {
        List<ReportNode> connectivityReports = new ArrayList<>();

        if (Objects.isNull(branchCreationInfos.getVoltageLevelId1()) || Objects.isNull(branchCreationInfos.getBusOrBusbarSectionId1())) {
            return;
        }
        connectivityReports.addAll(buildConnectivityReports(
                branchCreationInfos.getConnectionName1(),
                branchCreationInfos.getConnectionDirection1(),
                branchCreationInfos.getConnectionPosition1(),
                branchCreationInfos.isConnected1(),
                branchCreationInfos.getEquipmentId(),
                FeederSide.BRANCH_SIDE_ONE
        ));

        if (Objects.isNull(branchCreationInfos.getVoltageLevelId2()) || Objects.isNull(branchCreationInfos.getBusOrBusbarSectionId2())) {
            return;
        }
        if (!Objects.isNull(branchCreationInfos.getVoltageLevelId2()) && !Objects.isNull(branchCreationInfos.getBusOrBusbarSectionId2())) {
            connectivityReports.addAll(buildConnectivityReports(
                    branchCreationInfos.getConnectionName2(),
                    branchCreationInfos.getConnectionDirection2(),
                    branchCreationInfos.getConnectionPosition2(),
                    branchCreationInfos.isConnected2(),
                    branchCreationInfos.getEquipmentId(),
                    FeederSide.BRANCH_SIDE_TWO
            ));
        }

        if (!connectivityReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReporter, connectivityReports, "network.modification.ConnectivityCreated");
        }
    }

    private static List<ReportNode> buildConnectivityReports(
            String connectionName,
            ConnectablePosition.Direction connectionDirection,
            Integer connectionPosition,
            boolean isConnected,
            String equipmentId,
            FeederSide side) {

        List<ReportNode> reports = new ArrayList<>();

        if (connectionName != null || connectionDirection != null || connectionPosition != null) {
            if (connectionName != null) {
                reports.add(ModificationUtils.getInstance()
                        .buildCreationReport(connectionName, getConnectionNameField(side)));
            }

            if (connectionDirection != null) {
                reports.add(ModificationUtils.getInstance()
                        .buildCreationReport(connectionDirection, getConnectionDirectionField(side)));
            }

            if (connectionPosition != null) {
                reports.add(ModificationUtils.getInstance()
                        .buildCreationReport(connectionPosition, getConnectionPositionField(side)));
            }

            if (!isConnected) {
                reports.add(ReportNode.newRootReportNode()
                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                        .withMessageTemplate(EQUIPMENT_DISCONNECTED)
                        .withUntypedValue("id", equipmentId)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        }

        return reports;
    }

    public static Double parseDoubleOrNaNIfNull(String value) {
        return value != null ? Double.parseDouble(value) : Double.NaN;
    }

    public static void checkIsValueInferior(String errorMessage, Double minValueToCheck, Double maxValue, NetworkModificationException.Type exceptionType, String minValueName, String maxValueName) throws NetworkModificationException {
        if (minValueToCheck != null && !Double.isNaN(minValueToCheck) && maxValue != null && !Double.isNaN(maxValue) && maxValue < minValueToCheck) {
            throw new NetworkModificationException(exceptionType, errorMessage + " " + minValueName + " (" + minValueToCheck + ") must be inferior to " + maxValueName + " (" + maxValue + ")");
        }
    }

    public static void checkIsValueSuperior(String errorMessage, Double minValue, Double maxValueToCheck, NetworkModificationException.Type exceptionType, String minValueName, String maxValueName) throws NetworkModificationException {
        if (minValue != null && !Double.isNaN(minValue) && maxValueToCheck != null && !Double.isNaN(maxValueToCheck) && maxValueToCheck < minValue) {
            throw new NetworkModificationException(exceptionType, errorMessage + " " + maxValueName + " (" + maxValueToCheck + ") must be superior to " + minValueName + " (" + minValue + ")");
        }
    }

    public static void checkIsNotNegativeValue(String errorMessage, Double valueToCheck, NetworkModificationException.Type exceptionType, String valueName) throws NetworkModificationException {
        if (valueToCheck != null && !Double.isNaN(valueToCheck) && valueToCheck < 0) {
            throw new NetworkModificationException(exceptionType, errorMessage + "can not have a negative value for " + valueName);
        }
    }

    public static void checkIsPercentage(String errorMessage, Float valueToCheck, NetworkModificationException.Type exceptionType, String valueName) throws NetworkModificationException {
        if (valueToCheck != null) {
            checkIsPercentage(errorMessage, valueToCheck.doubleValue(), exceptionType, valueName);
        }
    }

    public static void checkIsPercentage(String errorMessage, Double valueToCheck, NetworkModificationException.Type exceptionType, String valueName) throws NetworkModificationException {
        if (valueToCheck != null && (valueToCheck < 0 || valueToCheck > 100)) {
            throw new NetworkModificationException(exceptionType, errorMessage + "must have " + valueName + " between 0 and 100");
        }
    }

    public static void checkIsBetween0And1(String errorMessage, Double valueToCheck, NetworkModificationException.Type exceptionType, String valueName) throws NetworkModificationException {
        if (valueToCheck != null && (valueToCheck < 0 || valueToCheck > 1)) {
            throw new NetworkModificationException(exceptionType, errorMessage + "must have " + valueName + " between 0 and 1");
        }
    }

    public static void checkIsInInterval(String errorMessage, Float valueToCheck, Pair<Float, Float> interval, NetworkModificationException.Type exceptionType, String valueName) throws NetworkModificationException {
        if (valueToCheck != null && (valueToCheck < interval.getFirst() || valueToCheck > interval.getSecond())) {
            throw new NetworkModificationException(exceptionType, errorMessage + "must have " + valueName + "  " + interval.getFirst() + " and " + interval.getSecond());
        }
    }

    public static void checkLimitsGroupExist(String errorMessage, String limitsGroupIdToSet, NetworkModificationException.Type exceptionType, List<String> existingOperationalLimitsGroupIds, int side) throws NetworkModificationException {
        if (StringUtils.hasText(limitsGroupIdToSet) && !existingOperationalLimitsGroupIds.contains(limitsGroupIdToSet)) {
            throw new NetworkModificationException(exceptionType, errorMessage +
                String.format("missing limit set %s applicable on side %d : equipment ignored", limitsGroupIdToSet, side));
        }
    }

    public static void checkActivePowerValue(String errorMessage, String fieldName, double newValue, double minP, double maxP, NetworkModificationException.Type exceptionType) throws NetworkModificationException {
        if (newValue > maxP || newValue < minP) {
            String message = String.format("Invalid value %.2f field %s should be within interval [%.2f; %.2f]", newValue, fieldName, minP, maxP);
            throw new NetworkModificationException(exceptionType, errorMessage + message);
        }
    }

    public static void checkPowerValues(String errorMessage, double minP, double maxP, double targetP, Double plannedActivePowerSetPoint, NetworkModificationException.Type exceptionType) throws NetworkModificationException {
        if (targetP != 0) { // exception for the rule minP <= targetP <= maxP
            checkActivePowerValue(errorMessage, FIELD_ACTIVE_POWER_TARGET, targetP, minP, maxP, exceptionType);
        }
        if (plannedActivePowerSetPoint != null) {
            checkActivePowerValue(errorMessage, FIELD_PLANNED_ACTIVE_POWER_SET_POINT, plannedActivePowerSetPoint, minP, maxP, exceptionType);
        }
        checkMinimumActivePower(errorMessage, maxP, targetP, plannedActivePowerSetPoint, minP, exceptionType);
        checkMaximumActivePower(errorMessage, minP, targetP, plannedActivePowerSetPoint, maxP, exceptionType);
    }

    public static void checkMinimumActivePower(String errorMessage, double maxP, double targetP, Double pImp, double newValue, NetworkModificationException.Type exceptionType) throws NetworkModificationException {
        // targetP = 0 is an exception to the rule
        double pMin = targetP != 0 ? Math.min(targetP, maxP) : maxP;
        if (pImp != null) {
            pMin = Math.min(pMin, pImp);
        }
        if (pMin < newValue) {
            throw new NetworkModificationException(exceptionType, errorMessage + String.format("Invalid value %.2f of field %s should be be smaller or equal to %.2f", newValue, FIELD_MIN_ACTIVE_POWER, pMin));
        }
    }

    public static void checkMaximumActivePower(String errorMessage, double minP, double targetP, Double plannedActivePowerSetPoint, double newValue, NetworkModificationException.Type exceptionType) throws NetworkModificationException {
        // targetP = 0 is an exception to the rule
        double pMax = targetP != 0 ? Math.max(targetP, minP) : minP;
        if (plannedActivePowerSetPoint != null) {
            pMax = Math.max(pMax, plannedActivePowerSetPoint);
        }
        if (pMax > newValue) {
            throw new NetworkModificationException(exceptionType, errorMessage + String.format("Invalid value %.2f of field %s should be be greater or equal to %.2f", newValue, FIELD_MAX_ACTIVE_POWER, pMax));
        }
    }

    public static boolean validateMinimumActivePower(Generator generator, List<ReportNode> reports, double newValue) {
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        Double plannedActivePowerSetPoint = generatorStartup != null && !Double.isNaN(generatorStartup.getPlannedActivePowerSetpoint()) ? generatorStartup.getPlannedActivePowerSetpoint() : null;

        // targetP = 0 is an exception to the rule
        double minP = generator.getTargetP() != 0 ? Math.min(generator.getTargetP(), generator.getMaxP()) : generator.getMaxP();
        if (plannedActivePowerSetPoint != null) {
            minP = Math.min(minP, plannedActivePowerSetPoint);
        }

        if (minP < newValue) {
            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate("network.modification.generator.ValueShouldBeSmallerThan")
                .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, generator.getId())
                .withUntypedValue(VALUE_KEY_FIELD_NAME, FIELD_MIN_ACTIVE_POWER)
                .withUntypedValue(VALUE_KEY_FIELD_VALUE, newValue)
                .withUntypedValue(VALUE_KEY_TARGET_VALUE, minP)
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
            return false;
        }
        return true;
    }

    public static boolean validateMaximumActivePower(Generator generator, List<ReportNode> reports, double newValue) {
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        Double pImp = generatorStartup != null && !Double.isNaN(generatorStartup.getPlannedActivePowerSetpoint()) ? generatorStartup.getPlannedActivePowerSetpoint() : null;

        // targetP = 0 is an exception to the rule
        double maxP = generator.getTargetP() != 0 ? Math.max(generator.getTargetP(), generator.getMinP()) : generator.getMinP();
        if (pImp != null) {
            maxP = Math.min(maxP, pImp);
        }

        if (newValue < maxP) {
            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate("network.modification.generator.ValueShouldBeGreaterThan")
                .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, generator.getId())
                .withUntypedValue(VALUE_KEY_FIELD_NAME, FIELD_MAX_ACTIVE_POWER)
                .withUntypedValue(VALUE_KEY_FIELD_VALUE, newValue)
                .withUntypedValue(VALUE_KEY_TARGET_VALUE, maxP)
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
            return false;
        }
        return true;
    }

    public static boolean validateActivePowerValue(Generator generator, String fieldName, List<ReportNode> reports, double newValue) {
        if (newValue > generator.getMaxP() || newValue < generator.getMinP()) {
            reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate("network.modification.generator.ValueShouldBeWithinInterval")
                .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, generator.getId())
                .withUntypedValue(VALUE_KEY_FIELD_NAME, fieldName)
                .withUntypedValue(VALUE_KEY_FIELD_VALUE, newValue)
                .withUntypedValue(VALUE_KEY_MIN_VALUE, generator.getMinP())
                .withUntypedValue(VALUE_KEY_MAX_VALUE, generator.getMaxP())
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
            return false;
        }
        return true;
    }

    public static List<OperationalLimitsGroupInfos> getOperationalLimitsGroupsOnSide(List<OperationalLimitsGroupInfos> operationalLimitsGroupInfos,
                                                                               OperationalLimitsGroupInfos.Applicability applicability) {
        if (operationalLimitsGroupInfos == null || operationalLimitsGroupInfos.isEmpty()) {
            return List.of();
        }
        return operationalLimitsGroupInfos.stream().filter(info -> info.getApplicability() == applicability
            || info.getApplicability() == OperationalLimitsGroupInfos.Applicability.EQUIPMENT).toList();
    }

    public static boolean hasLimitSet(List<OperationalLimitsGroupInfos> operationalLimitsGroupInfos, String limitSet) {
        return operationalLimitsGroupInfos.stream().anyMatch(
            info -> Objects.equals(info.getId(), limitSet));
    }

    private boolean isNotModificationVoltageLevelBusOrBusBarInfos(AttributeModification<String> voltageLevelId,
                                                                 AttributeModification<String> busOrBusbarSectionId) {
        return Optional.ofNullable(voltageLevelId).isEmpty() && Optional.ofNullable(busOrBusbarSectionId).isEmpty()
                || Optional.ofNullable(voltageLevelId).map(AttributeModification::getValue).isEmpty() &&
                Optional.ofNullable(busOrBusbarSectionId).map(AttributeModification::getValue).isEmpty();
    }

    public void moveFeederBay(Connectable<?> connectable, Terminal terminal,
                              AttributeModification<String> voltageLevelId,
                              AttributeModification<String> busOrBusbarSectionId,
                              ReportNode subReportNode) {
        if (isNotModificationVoltageLevelBusOrBusBarInfos(voltageLevelId, busOrBusbarSectionId)) {
            return;
        }

        Network network = connectable.getNetwork();

        String finalVoltageLevelId = voltageLevelId.getValue() != null ?
                voltageLevelId.getValue() :
                network.getVoltageLevel(terminal.getVoltageLevel().getId()).getId();

        String finalBusOrBusbarSectionId = busOrBusbarSectionId != null ?
                busOrBusbarSectionId.getValue() :
                ModificationUtils.getInstance().getBusOrBusbarSection(terminal);

        new MoveFeederBayBuilder().withConnectableId(connectable.getId())
                .withTargetVoltageLevelId(finalVoltageLevelId)
                .withTerminal(terminal)
                .withTargetBusOrBusBarSectionId(finalBusOrBusbarSectionId)
                .build().apply(network, true, subReportNode);
    }

    public void checkVoltageLevelModification(Network network,
                                               AttributeModification<String> voltageLevelId,
                                               AttributeModification<String> busOrBusbarSectionId,
                                               Terminal terminal) {
        Terminal targetTerminal = Objects.requireNonNull(terminal);
        if (checkAttributeModificationValue(voltageLevelId) && checkAttributeModificationValue(busOrBusbarSectionId)) {
            ModificationUtils.getInstance().controlBus(ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId.getValue()), busOrBusbarSectionId.getValue());
        } else if (!checkAttributeModificationValue(voltageLevelId) && checkAttributeModificationValue(busOrBusbarSectionId)) {
            ModificationUtils.getInstance().controlBus(targetTerminal.getVoltageLevel(), busOrBusbarSectionId.getValue());
        } else if (checkAttributeModificationValue(voltageLevelId) && !checkAttributeModificationValue(busOrBusbarSectionId)) {
            ModificationUtils.getInstance().controlBus(ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId.getValue()),
                    BusbarSectionFinderTraverser.findBusbarSectionId(terminal)
            );
        }
    }

    private boolean checkAttributeModificationValue(AttributeModification<String> attribute) {
        return attribute != null && attribute.getValue() != null;
    }

    // for battery and generator
    public void createShortCircuitExtension(Double stepUpTransformerX, Double directTransX, String equipmentId,
                                            ShortCircuitExtensionAdder<?, ?, ?> shortCircuitExtensionAdder,
                                            ReportNode subReportNode, String equipmentType) {
        if (directTransX != null) {
            List<ReportNode> shortCircuitReports = new ArrayList<>();
            try {
                shortCircuitExtensionAdder.withDirectTransX(directTransX);
                if (stepUpTransformerX != null) {
                    shortCircuitExtensionAdder.withStepUpTransformerX(stepUpTransformerX);
                }
                shortCircuitExtensionAdder.add();
                shortCircuitReports.add(buildCreationReport(directTransX, TRANSIENT_REACTANCE));
                if (stepUpTransformerX != null) {
                    shortCircuitReports.add(buildCreationReport(stepUpTransformerX, TRANSFORMER_REACTANCE));
                }
            } catch (PowsyblException e) {
                shortCircuitReports.add(ReportNode.newRootReportNode()
                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                        .withMessageTemplate("network.modification.ShortCircuitExtensionAddError")
                        .withUntypedValue("id", equipmentId)
                        .withUntypedValue("message", e.getMessage())
                        .withUntypedValue("equipmentType", equipmentType)
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            reportModifications(subReportNode, shortCircuitReports, "network.modification.shortCircuitCreated");
        }
    }

    public void modifyShortCircuitExtension(AttributeModification<Double> directTransX,
                                                   AttributeModification<Double> stepUpTransformerX,
                                                   ShortCircuitExtension<?> shortCircuitExtension,
                                                   Supplier<ShortCircuitExtensionAdder<?, ?, ?>> shortCircuitExtensionAdderSupplier,
                                                   ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        double oldTransientReactance = shortCircuitExtension != null ? shortCircuitExtension.getDirectTransX() : Double.NaN;
        double oldStepUpTransformerReactance = shortCircuitExtension != null ? shortCircuitExtension.getStepUpTransformerX() : Double.NaN;
        // Either transient reactance or step-up transformer reactance are modified or
        // both
        String stepUpTransformerXNewValue = stepUpTransformerX != null ? stepUpTransformerX.getValue().toString() : null;
        if (directTransX != null && stepUpTransformerX != null) {
            shortCircuitExtensionAdderSupplier.get()
                    .withDirectTransX(directTransX.getValue())
                    .withStepUpTransformerX(stepUpTransformerX.getValue())
                    .add();
            reports.add(buildModificationReport(
                    oldTransientReactance,
                    directTransX.getValue(),
                    TRANSIENT_REACTANCE));
            reports.add(buildModificationReport(
                    oldStepUpTransformerReactance,
                    stepUpTransformerXNewValue,
                    TRANSFORMER_REACTANCE));

        } else if (directTransX != null) {
            shortCircuitExtensionAdderSupplier.get()
                    .withStepUpTransformerX(oldStepUpTransformerReactance)
                    .withDirectTransX(directTransX.getValue())
                    .add();
            reports.add(buildModificationReport(
                    oldTransientReactance,
                    directTransX.getValue(),
                    TRANSIENT_REACTANCE));
        } else if (stepUpTransformerX != null) {
            if (Double.isNaN(stepUpTransformerX.getValue())) {
                shortCircuitExtensionAdderSupplier.get()
                        .withDirectTransX(oldTransientReactance)
                        .add();
                stepUpTransformerXNewValue = NO_VALUE;
            } else {
                shortCircuitExtensionAdderSupplier.get()
                        .withStepUpTransformerX(stepUpTransformerX.getValue())
                        .withDirectTransX(oldTransientReactance)
                        .add();
            }
            reports.add(buildModificationReport(
                    oldStepUpTransformerReactance,
                    stepUpTransformerXNewValue,
                    TRANSFORMER_REACTANCE));
        }
        if (subReportNode != null) {
            reportModifications(subReportNode, reports, "network.modification.shortCircuitAttributesModified");
        }
    }

    public void createNewActivePowerControlForInjectionCreation(ActivePowerControlAdder<?> adder, Boolean participate, Float droop, ReportNode subReporter) {
        if (participate != null) {
            List<ReportNode> activePowerRegulationReports = new ArrayList<>();
            double droopNotNull = droop != null ? droop : Double.NaN;
            adder.withParticipate(participate).withDroop(droopNotNull).add();
            activePowerRegulationReports.add(ModificationUtils.getInstance().buildCreationReport(participate, PARTICIPATE));
            activePowerRegulationReports.add(ModificationUtils.getInstance().buildCreationReport(droopNotNull, DROOP));
            reportModifications(subReporter, activePowerRegulationReports, "network.modification.ActivePowerRegulationCreated");
        }

    }

    public static void updateMeasurementValidity(Measurement measurement, boolean requestedValidity) {
        measurement.setValid(requestedValidity);
        if (measurement.getProperty(MEASUREMENT_VALIDITY_PROPERTY) != null) {
            if (requestedValidity) {
                switch (measurement.getProperty(MEASUREMENT_VALIDITY_PROPERTY)) {
                    //validity = 1 →  TM non valid & not masked
                    //validity = 3 →  TM non valid & masked
                    case "1": measurement.putProperty(MEASUREMENT_VALIDITY_PROPERTY, "0"); break;
                    case "3": measurement.putProperty(MEASUREMENT_VALIDITY_PROPERTY, "2"); break;
                    default: break;
                }
            } else {
                switch (measurement.getProperty(MEASUREMENT_VALIDITY_PROPERTY)) {
                    //validity = 0 →  TM valid & not masked
                    //validity = 2 →  TM valid & masked
                    case "0": measurement.putProperty(MEASUREMENT_VALIDITY_PROPERTY, "1"); break;
                    case "2": measurement.putProperty(MEASUREMENT_VALIDITY_PROPERTY, "3"); break;
                    default: break;
                }
            }
        }
    }
}
