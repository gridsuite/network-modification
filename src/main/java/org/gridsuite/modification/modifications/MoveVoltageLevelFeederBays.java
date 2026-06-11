/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.function.Consumer;

import static org.gridsuite.modification.NetworkModificationException.Type.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class MoveVoltageLevelFeederBays extends AbstractModification {
    private static final String VOLTAGE_LEVEL_NOT_FOUND = "Voltage level %s is not found";
    private static final String BUSBAR_NOT_FOUND = "Bus or busbar section %s where connectable %s is supposed to be is not found in voltage level %s";
    private static final String UNSUPPORTED_CONNECTABLE = "MoveVoltageLevelFeederBays is not implemented for %s";
    private static final String INVALID_CONNECTION_SIDE = "Invalid connection side: %s for branch %s";

    private final MoveVoltageLevelFeederBaysModel modificationModel;

    public MoveVoltageLevelFeederBays(MoveVoltageLevelFeederBaysModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        VoltageLevel voltageLevel = getVoltageLevelOrThrow(network, modificationModel.getVoltageLevelId());
        for (MoveFeederBayModel info : modificationModel.getFeederBays()) {
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

    private void checkBusOrBusbarSectionExist(VoltageLevel voltageLevel, MoveFeederBayModel info) {
        boolean busOrBusbarSectionExists = voltageLevel.getTopologyKind().equals(TopologyKind.NODE_BREAKER)
            ? voltageLevel.getNodeBreakerView().getBusbarSection(info.getBusbarSectionId()) != null
            : voltageLevel.getBusBreakerView().getBus(info.getBusbarSectionId()) != null;
        if (!busOrBusbarSectionExists) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(BUSBAR_NOT_FOUND,
                info.getBusbarSectionId(), info.getEquipmentId(), modificationModel.getVoltageLevelId()));
        }
    }

    private void checkConnectable(Network network, MoveFeederBayModel info) {
        Connectable<?> connectable = network.getConnectable(info.getEquipmentId());
        if (connectable instanceof BusbarSection || connectable instanceof ThreeWindingsTransformer) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(UNSUPPORTED_CONNECTABLE, connectable.getClass()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        for (MoveFeederBayModel info : modificationModel.getFeederBays()) {
            Connectable<?> connectable = network.getConnectable(info.getEquipmentId());
            if (connectable != null) {
                modifyConnectablePosition(network, connectable, info, subReportNode);
            } else {
                subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.moveFeederBaysConnectableNotFoundWarning")
                    .withUntypedValue("id", info.getEquipmentId())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .add();
            }
        }
    }

    @Override
    public String getName() {
        return ModificationType.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS.name();
    }

    private void modifyConnectablePosition(Network network, Connectable<?> connectable, MoveFeederBayModel newConnectablePositionModel, ReportNode subReportNode) {
        ConnectablePosition<?> oldConnectablePosition = (ConnectablePosition<?>) connectable.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<?> connectablePositionAdder = connectable.newExtension(ConnectablePositionAdder.class);

        switch (connectable) {
            case Injection<?> injection -> {
                InjectionModificationModel injectionModificationModel = buildInjectionModificationModel(newConnectablePositionModel);
                ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(oldConnectablePosition, connectablePositionAdder, injection, injectionModificationModel, subReportNode);
            }
            case Branch<?> branch -> {
                BranchModificationModel branchModificationModel = buildBranchModificationModel(newConnectablePositionModel);
                ModificationUtils.getInstance().modifyBranchConnectivityAttributes(oldConnectablePosition, connectablePositionAdder, branch, branchModificationModel, subReportNode);
            }
            default ->
                throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(UNSUPPORTED_CONNECTABLE, connectable.getClass()));
        }
        moveFeederBay(network, connectable, newConnectablePositionModel, subReportNode);
    }

    private InjectionModificationModel buildInjectionModificationModel(MoveFeederBayModel newConnectablePositionModel) {
        InjectionModificationModel injectionModel = new InjectionModificationModel();
        injectionModel.setEquipmentId(newConnectablePositionModel.getEquipmentId());
        setConnectionAttributes(injectionModel::setConnectionPosition,
            injectionModel::setConnectionName,
            injectionModel::setConnectionDirection,
            newConnectablePositionModel);
        return injectionModel;
    }

    private BranchModificationModel buildBranchModificationModel(MoveFeederBayModel info) {
        BranchModificationModel branchModel = new BranchModificationModel();
        branchModel.setEquipmentId(info.getEquipmentId());
        ThreeSides connectionSide = ThreeSides.valueOf(info.getConnectionSide());
        switch (connectionSide) {
            case ONE -> setConnectionAttributes(branchModel::setConnectionPosition1,
                branchModel::setConnectionName1,
                branchModel::setConnectionDirection1,
                info);
            case TWO -> setConnectionAttributes(branchModel::setConnectionPosition2,
                branchModel::setConnectionName2,
                branchModel::setConnectionDirection2,
                info);
            default ->
                throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(INVALID_CONNECTION_SIDE, info.getConnectionSide(), branchModel.getEquipmentId()));
        }
        return branchModel;
    }

    private void setConnectionAttributes(java.util.function.Consumer<AttributeModification<Integer>> setPosition,
                                         java.util.function.Consumer<AttributeModification<String>> setName,
                                         java.util.function.Consumer<AttributeModification<ConnectablePosition.Direction>> setDirection,
                                         MoveFeederBayModel info) {
        acceptIfNotNull(setPosition, info.getConnectionPosition());
        acceptIfNotNull(setName, info.getConnectionName());
        acceptIfNotNull(setDirection, info.getConnectionDirection());
    }

    private <T> void acceptIfNotNull(Consumer<AttributeModification<T>> setter, T value) {
        if (value != null) {
            setter.accept(new AttributeModification<>(value, OperationType.SET));
        }
    }

    private void moveFeederBay(Network network, Connectable<?> connectable, MoveFeederBayModel info, ReportNode subReportNode) {
        Terminal terminal = getTerminal(network, info);
        String currentBusbarId = ModificationUtils.getInstance().getBusOrBusbarSection(terminal);
        String targetBusbarId = info.getBusbarSectionId();
        if (!currentBusbarId.equals(targetBusbarId)) {
            ModificationUtils.getInstance().moveFeederBay(
                connectable, terminal,
                new AttributeModification<>(modificationModel.getVoltageLevelId(), OperationType.SET),
                new AttributeModification<>(targetBusbarId, OperationType.SET),
                subReportNode);
        }
    }

    public Terminal getTerminal(Network network, MoveFeederBayModel info) {
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
            default ->
                throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format(UNSUPPORTED_CONNECTABLE, connectable.getClass()));
        };
    }
}
