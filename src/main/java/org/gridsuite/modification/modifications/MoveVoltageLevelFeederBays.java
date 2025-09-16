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
    private static final String UNSUPPORTED_CONNECTABLE = "ConnectablePositionModification is not implemented for %s";
    private static final String INVALID_CONNECTION_SIDE = "Invalid connection side: %s for branch %s";

    private final MoveVoltageLevelFeederBaysInfos modificationInfos;

    public MoveVoltageLevelFeederBays(MoveVoltageLevelFeederBaysInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        VoltageLevel voltageLevel = checkVoltageLevelOrThrow(network, modificationInfos.getVoltageLevelId());
        for (ConnectablePositionModificationInfos info : modificationInfos.getFeederBaysAttributeList()) {
            checkBusOrBusbarSection(voltageLevel, info);
            checkConnectable(network, info);
        }
    }

    private VoltageLevel checkVoltageLevelOrThrow(Network network, String voltageLevelId) {
        VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
        if (voltageLevel == null) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(VOLTAGE_LEVEL_NOT_FOUND, voltageLevelId));
        }
        return voltageLevel;
    }

    private void checkBusOrBusbarSection(VoltageLevel voltageLevel, ConnectablePositionModificationInfos info) {
        boolean busOrBusbarSectionExists = voltageLevel.getTopologyKind().equals(TopologyKind.NODE_BREAKER)
                ? voltageLevel.getNodeBreakerView().getBusbarSection(info.getBusbarSectionId()) != null
                : voltageLevel.getBusBreakerView().getBus(info.getBusbarSectionId()) != null;
        if (!busOrBusbarSectionExists) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(BUSBAR_NOT_FOUND,
                    info.getBusbarSectionId(), info.getEquipmentId(), modificationInfos.getVoltageLevelId()));
        }
    }

    private void checkConnectable(Network network, ConnectablePositionModificationInfos info) {
        Connectable<?> connectable = network.getConnectable(info.getEquipmentId());
        if (connectable == null) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(CONNECTABLE_NOT_FOUND, info.getEquipmentId()));
        }
        if (!(connectable instanceof Injection<?>) && !(connectable instanceof Branch<?>)) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(UNSUPPORTED_CONNECTABLE, connectable.getClass()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        for (ConnectablePositionModificationInfos info : modificationInfos.getFeederBaysAttributeList()) {
            Connectable<?> connectable = network.getConnectable(info.getEquipmentId());
            switch (connectable) {
                case Injection<?> injection -> modifyInjectionConnectablePosition(network, injection, info, subReportNode);
                case Branch<?> branch -> modifyBranchConnectablePosition(network, branch, info, subReportNode);
                default -> throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(UNSUPPORTED_CONNECTABLE, connectable.getClass()));
            }
        }
    }

    @Override
    public String getName() {
        return ModificationType.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS.name();
    }

    private void modifyInjectionConnectablePosition(Network network, Injection<?> injection, ConnectablePositionModificationInfos info, ReportNode subReportNode) {
        ConnectablePosition connectablePosition = injection.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder connectablePositionAdder = injection.newExtension(ConnectablePositionAdder.class);
        InjectionModificationInfos injectionModificationInfos = buildInjectionModificationInfos(info);
        ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, injection, injectionModificationInfos, subReportNode);
        moveVoltageLevelBusOrBusbarSection(network, injection, info, subReportNode);
    }

    private void modifyBranchConnectablePosition(Network network, Branch<?> branch, ConnectablePositionModificationInfos info, ReportNode subReportNode) {
        ConnectablePosition connectablePosition = branch.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder connectablePositionAdder = branch.newExtension(ConnectablePositionAdder.class);
        BranchModificationInfos branchModificationInfos = buildBranchModificationInfos(info);
        ModificationUtils.getInstance().modifyBranchConnectivityAttributes(connectablePosition, connectablePositionAdder, branch, branchModificationInfos, subReportNode);
        moveVoltageLevelBusOrBusbarSection(network, (Connectable<?>) branch, info, subReportNode);
    }

    private InjectionModificationInfos buildInjectionModificationInfos(ConnectablePositionModificationInfos info) {
        InjectionModificationInfos injectionInfos = new InjectionModificationInfos();
        injectionInfos.setEquipmentId(info.getEquipmentId());
        injectionInfos.setConnectionPosition(new AttributeModification<>(info.getConnectionPosition(), OperationType.SET));
        injectionInfos.setConnectionName(new AttributeModification<>(info.getConnectionName(), OperationType.SET));
        injectionInfos.setConnectionDirection(new AttributeModification<>(info.getConnectionDirection(), OperationType.SET));
        return injectionInfos;
    }

    private BranchModificationInfos buildBranchModificationInfos(ConnectablePositionModificationInfos info) {
        BranchModificationInfos branchInfos = new BranchModificationInfos();
        branchInfos.setEquipmentId(info.getEquipmentId());

        ThreeSides connectionSide = ThreeSides.valueOf(info.getConnectionSide());
        switch (connectionSide) {
            case ONE -> {
                branchInfos.setConnectionPosition1(new AttributeModification<>(info.getConnectionPosition(), OperationType.SET));
                branchInfos.setConnectionName1(new AttributeModification<>(info.getConnectionName(), OperationType.SET));
                branchInfos.setConnectionDirection1(new AttributeModification<>(info.getConnectionDirection(), OperationType.SET));
            }
            case TWO -> {
                branchInfos.setConnectionPosition2(new AttributeModification<>(info.getConnectionPosition(), OperationType.SET));
                branchInfos.setConnectionName2(new AttributeModification<>(info.getConnectionName(), OperationType.SET));
                branchInfos.setConnectionDirection2(new AttributeModification<>(info.getConnectionDirection(), OperationType.SET));
            }
            default -> throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(INVALID_CONNECTION_SIDE, info.getConnectionSide(), branchInfos.getEquipmentId()));
        }
        return branchInfos;
    }

    private void moveVoltageLevelBusOrBusbarSection(Network network, Connectable<?> connectable, ConnectablePositionModificationInfos info, ReportNode subReportNode) {
        Terminal terminal = getTerminal(network, info);
        String currentBusbarId = ModificationUtils.getInstance().getBusOrBusbarSection(terminal);
        String targetBusbarId = info.getBusbarSectionId();
        if (!currentBusbarId.equals(targetBusbarId)) {
            ModificationUtils.getInstance().modifyVoltageLevelBusOrBusBarSectionAttributes(
                    connectable, terminal,
                    new AttributeModification<>(modificationInfos.getVoltageLevelId(), OperationType.SET),
                    new AttributeModification<>(targetBusbarId, OperationType.SET),
                    subReportNode);
        }
    }

    public Terminal getTerminal(Network network, ConnectablePositionModificationInfos info) {
        Connectable<?> connectable = network.getConnectable(info.getEquipmentId());
        return switch (connectable) {
            case Injection<?> injection -> injection.getTerminal();
            case Branch<?> branch -> getTerminalFromBranch(branch, info);
            default -> throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(UNSUPPORTED_CONNECTABLE, connectable.getClass()));
        };
    }

    private Terminal getTerminalFromBranch(Branch<?> branch, ConnectablePositionModificationInfos info) {
        return switch (ThreeSides.valueOf(info.getConnectionSide())) {
            case ONE -> branch.getTerminal1();
            case TWO -> branch.getTerminal2();
            default -> throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(INVALID_CONNECTION_SIDE, info.getConnectionSide(), branch.getId()));
        };
    }
}
