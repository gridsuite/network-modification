/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class MoveVoltageLevelFeederBays extends AbstractModification {
    private static final String VOLTAGE_LEVEL_NOT_FOUND = "Voltage level %s is not found";
    private static final String CONNECTABLE_NOT_FOUND = "Connectable %s not found";
    private static final String BUSBAR_NOT_FOUND = "Bus or busbar section %s where connectable %s is supposed to be is not found in voltage level %s";
    private static final String UNSUPPORTED_CONNECTABLE = "MoveVoltageLevelFeederBays is not implemented for %s";
    private static final String INVALID_CONNECTION_SIDE = "Invalid connection side: %s for branch %s";

    private final MoveVoltageLevelFeederBaysInfos modificationInfos;

    public MoveVoltageLevelFeederBays(MoveVoltageLevelFeederBaysInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        VoltageLevel voltageLevel = getVoltageLevelOrThrow(network, modificationInfos.getVoltageLevelId());
        for (MoveFeederBayInfos info : modificationInfos.getFeederBays()) {
            checkBusOrBusbarSectionExist(voltageLevel, info);
            checkConnectable(network, info);
        }
    }

    private VoltageLevel getVoltageLevelOrThrow(Network network, String voltageLevelId) {
        VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
        if (voltageLevel == null) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(VOLTAGE_LEVEL_NOT_FOUND, voltageLevelId));
        }
        return voltageLevel;
    }

    private void checkBusOrBusbarSectionExist(VoltageLevel voltageLevel, MoveFeederBayInfos info) {
        boolean busOrBusbarSectionExists = voltageLevel.getTopologyKind().equals(TopologyKind.NODE_BREAKER)
                ? voltageLevel.getNodeBreakerView().getBusbarSection(info.getBusbarSectionId()) != null
                : voltageLevel.getBusBreakerView().getBus(info.getBusbarSectionId()) != null;
        if (!busOrBusbarSectionExists) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(BUSBAR_NOT_FOUND,
                    info.getBusbarSectionId(), info.getEquipmentId(), modificationInfos.getVoltageLevelId()));
        }
    }

    private void checkConnectable(Network network, MoveFeederBayInfos info) {
        Connectable<?> connectable = network.getConnectable(info.getEquipmentId());
        if (connectable == null) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(CONNECTABLE_NOT_FOUND, info.getEquipmentId()));
        }
        if (connectable instanceof BusbarSection || connectable instanceof ThreeWindingsTransformer) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(UNSUPPORTED_CONNECTABLE, connectable.getClass()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        for (MoveFeederBayInfos info : modificationInfos.getFeederBays()) {
            Connectable<?> connectable = network.getConnectable(info.getEquipmentId());
            modifyConnectablePosition(network, connectable, info, subReportNode);
        }
    }

    @Override
    public String getName() {
        return ModificationType.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS.name();
    }

    private void modifyConnectablePosition(Network network, Connectable<?> connectable, MoveFeederBayInfos newConnectablePositionInfos, ReportNode subReportNode) {
        ConnectablePosition<?> oldConnectablePosition = (ConnectablePosition<?>) connectable.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<?> connectablePositionAdder = connectable.newExtension(ConnectablePositionAdder.class);

        switch (connectable) {
            case Injection<?> injection -> {
                InjectionModificationInfos injectionModificationInfos = buildInjectionModificationInfos(newConnectablePositionInfos);
                ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(oldConnectablePosition, connectablePositionAdder, injection, injectionModificationInfos, subReportNode);
            }
            case Branch<?> branch -> {
                BranchModificationInfos branchModificationInfos = buildBranchModificationInfos(newConnectablePositionInfos);
                ModificationUtils.getInstance().modifyBranchConnectivityAttributes(oldConnectablePosition, connectablePositionAdder, branch, branchModificationInfos, subReportNode);
            }
            default -> throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(UNSUPPORTED_CONNECTABLE, connectable.getClass()));
        }
        moveFeederBay(network, connectable, newConnectablePositionInfos, subReportNode);
    }

    private InjectionModificationInfos buildInjectionModificationInfos(MoveFeederBayInfos newConnectablePositionInfos) {
        InjectionModificationInfos injectionInfos = new InjectionModificationInfos();
        injectionInfos.setEquipmentId(newConnectablePositionInfos.getEquipmentId());
        setConnectionAttributes(injectionInfos::setConnectionPosition,
                injectionInfos::setConnectionName,
                injectionInfos::setConnectionDirection,
                newConnectablePositionInfos);
        return injectionInfos;
    }

    private BranchModificationInfos buildBranchModificationInfos(MoveFeederBayInfos info) {
        BranchModificationInfos branchInfos = new BranchModificationInfos();
        branchInfos.setEquipmentId(info.getEquipmentId());
        ThreeSides connectionSide = ThreeSides.valueOf(info.getConnectionSide());
        switch (connectionSide) {
            case ONE -> setConnectionAttributes(branchInfos::setConnectionPosition1,
                    branchInfos::setConnectionName1,
                    branchInfos::setConnectionDirection1,
                    info);
            case TWO -> setConnectionAttributes(branchInfos::setConnectionPosition2,
                    branchInfos::setConnectionName2,
                    branchInfos::setConnectionDirection2,
                    info);
            default -> throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(INVALID_CONNECTION_SIDE, info.getConnectionSide(), branchInfos.getEquipmentId()));
        }
        return branchInfos;
    }

    private void setConnectionAttributes(java.util.function.Consumer<AttributeModification<Integer>> setPosition,
                                         java.util.function.Consumer<AttributeModification<String>> setName,
                                         java.util.function.Consumer<AttributeModification<ConnectablePosition.Direction>> setDirection,
                                         MoveFeederBayInfos info) {
        setPosition.accept(new AttributeModification<>(info.getConnectionPosition(), OperationType.SET));
        setName.accept(new AttributeModification<>(info.getConnectionName(), OperationType.SET));
        setDirection.accept(new AttributeModification<>(info.getConnectionDirection(), OperationType.SET));
    }

    private void moveFeederBay(Network network, Connectable<?> connectable, MoveFeederBayInfos info, ReportNode subReportNode) {
        Terminal terminal = getTerminal(network, info);
        String currentBusbarId = ModificationUtils.getInstance().getBusOrBusbarSection(terminal);
        String targetBusbarId = info.getBusbarSectionId();
        if (!currentBusbarId.equals(targetBusbarId)) {
            ModificationUtils.getInstance().moveFeederBay(
                    connectable, terminal,
                    new AttributeModification<>(modificationInfos.getVoltageLevelId(), OperationType.SET),
                    new AttributeModification<>(targetBusbarId, OperationType.SET),
                    subReportNode);
        }
    }

    public Terminal getTerminal(Network network, MoveFeederBayInfos info) {
        Connectable<?> connectable = network.getConnectable(info.getEquipmentId());
        return switch (connectable) {
            case Injection<?> injection -> injection.getTerminal();
            case Branch<?> branch -> {
                try {
                    TwoSides side = TwoSides.valueOf(info.getConnectionSide());
                    yield branch.getTerminal(side);
                } catch (IllegalArgumentException e) {
                    throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR,
                            String.format(INVALID_CONNECTION_SIDE, info.getConnectionSide(), branch.getId()));
                }
            }
            default -> throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(UNSUPPORTED_CONNECTABLE, connectable.getClass()));
        };
    }
}
