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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class ConnectablePositionModification extends AbstractModification {

    private final ConnectablePositionModificationInfos modificationInfos;

    public ConnectablePositionModification(ConnectablePositionModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public String getName() {
        return "ConnectablePositionModification";
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Connectable<?> connectable = network.getConnectable(modificationInfos.getConnectableId());
        ConnectablePosition connectablePosition = connectable.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder connectablePositionAdder = connectable.newExtension(ConnectablePositionAdder.class);
        if (connectable instanceof Injection<?> injection) {
            InjectionModificationInfos injectionModificationInfos = getInjectionModificationInfos();
            ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, injection,
                injectionModificationInfos, subReportNode);
        } else if (connectable instanceof Branch<?> branch) {
            BranchModificationInfos branchModificationInfos = getBranchModificationInfos(branch);
            ModificationUtils.getInstance().modifyBranchConnectivityAttributes(connectablePosition, connectablePositionAdder, branch,
                branchModificationInfos, subReportNode);
        }
    }

    private InjectionModificationInfos getInjectionModificationInfos() {
        InjectionModificationInfos injectionModificationInfos = new InjectionModificationInfos();
        injectionModificationInfos.setEquipmentId(modificationInfos.getConnectableId());
        injectionModificationInfos.setConnectionPosition(new AttributeModification<>(modificationInfos.getConnectionPosition(), OperationType.SET));
        injectionModificationInfos.setConnectionName(new AttributeModification<>(modificationInfos.getConnectionName(), OperationType.SET));
        injectionModificationInfos.setConnectionDirection(new AttributeModification<>(modificationInfos.getConnectionDirection(), OperationType.SET));
        return injectionModificationInfos;
    }

    private BranchModificationInfos getBranchModificationInfos(Branch<?> branch) {
        BranchModificationInfos branchModificationInfos = new BranchModificationInfos();
        branchModificationInfos.setEquipmentId(modificationInfos.getConnectableId());
        if (ModificationUtils.getInstance().getBusOrBusbarSection(branch.getTerminal1()).equals(modificationInfos.getBusbarSectionId())) {
            branchModificationInfos.setConnectionPosition1(new AttributeModification<>(modificationInfos.getConnectionPosition(), OperationType.SET));
            branchModificationInfos.setConnectionName1(new AttributeModification<>(modificationInfos.getConnectionName(), OperationType.SET));
            branchModificationInfos.setConnectionDirection1(new AttributeModification<>(modificationInfos.getConnectionDirection(), OperationType.SET));
        } else if (ModificationUtils.getInstance().getBusOrBusbarSection(branch.getTerminal2()).equals(modificationInfos.getBusbarSectionId())) {
            branchModificationInfos.setConnectionPosition2(new AttributeModification<>(modificationInfos.getConnectionPosition(), OperationType.SET));
            branchModificationInfos.setConnectionName2(new AttributeModification<>(modificationInfos.getConnectionName(), OperationType.SET));
            branchModificationInfos.setConnectionDirection2(new AttributeModification<>(modificationInfos.getConnectionDirection(), OperationType.SET));
        } else {
            System.out.println(ModificationUtils.getInstance().getBusOrBusbarSection(branch.getTerminal1()));
            System.out.println(ModificationUtils.getInstance().getBusOrBusbarSection(branch.getTerminal2()));
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format("the busbar id %s does not correspond to any of the busbar of %s", modificationInfos.getBusbarSectionId(), branch.getId()));
        }
        return branchModificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Connectable<?> connectable = network.getConnectable(modificationInfos.getConnectableId());
        if (connectable == null) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format("Connectable %s not found", modificationInfos.getConnectableId()));
        }
        if (!(connectable instanceof Injection<?>) && !(connectable instanceof Branch<?>)) {
            throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format("ConnectablePositionModification is not implemented for %s", connectable.getClass()));
        }
    }

    public Terminal getTerminal(Network network) {
        Connectable<?> connectable = network.getConnectable(modificationInfos.getConnectableId());
        switch (connectable) {
            case Injection<?> injection -> {
                return injection.getTerminal();
            }
            case Branch<?> branch -> {
                if (ModificationUtils.getInstance().getBusOrBusbarSection(branch.getTerminal1()).equals(modificationInfos.getBusbarSectionId())) {
                    return branch.getTerminal1();
                } else if (ModificationUtils.getInstance().getBusOrBusbarSection(branch.getTerminal2()).equals(modificationInfos.getBusbarSectionId())) {
                    return branch.getTerminal2();
                } else {
                    throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format("the busbar id %s does not correspond to any of the busbar of %s",
                        modificationInfos.getBusbarSectionId(), branch.getId()));
                }
            }
            default -> throw new NetworkModificationException(MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR, String.format("ConnectablePositionModification is not implemented for %s", connectable.getClass()));
        }
    }
}
