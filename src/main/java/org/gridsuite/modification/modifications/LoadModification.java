/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.RemoveFeederBay;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.LoadCreationInfos;
import org.gridsuite.modification.dto.LoadModificationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.Map;
import java.util.stream.Collectors;

import static org.gridsuite.modification.NetworkModificationException.Type.LOAD_NOT_FOUND;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class LoadModification extends AbstractModification {

    private final LoadModificationInfos modificationInfos;

    public LoadModification(LoadModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Load load = network.getLoad(modificationInfos.getEquipmentId());
        if (load == null) {
            throw new NetworkModificationException(LOAD_NOT_FOUND,
                    "Load " + modificationInfos.getEquipmentId() + " does not exist in network");
        }
        // check voltageLevel
        ModificationUtils.getInstance().checkVoltageLevelInjectionModification(network, modificationInfos, load);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Load load = network.getLoad(modificationInfos.getEquipmentId());
        // modify the load in the network
        modifyLoad(load, subReportNode);
    }

    @Override
    public String getName() {
        return "LoadModification";
    }

    private void modifyLoad(Load load, ReportNode subReportNode) {
        subReportNode.newReportNode()
            .withMessageTemplate("loadModification", "Load with id=${id} modified :")
            .withUntypedValue("id", modificationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
        modifyLoadVoltageLevelBusOrBusBarSectionAttributes(modificationInfos, load, subReportNode);
        ModificationUtils.getInstance().applyElementaryModifications(load::setName, () -> load.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");
        ModificationUtils.getInstance().applyElementaryModifications(load::setLoadType, load::getLoadType, modificationInfos.getLoadType(), subReportNode, "Type");
        modifyP0(load, modificationInfos.getP0(), subReportNode);
        modifyQ0(load, modificationInfos.getQ0(), subReportNode);
        modifyLoadConnectivityAttributes(modificationInfos, load, subReportNode);
        // properties
        PropertiesUtils.applyProperties(load, subReportNode, modificationInfos.getProperties(), "LoadProperties");
    }

    public static void modifyQ0(Load load, AttributeModification<Double> q0, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(load::setQ0, load::getQ0, q0, subReportNode, "Constant reactive power");
    }

    public static void modifyP0(Load load, AttributeModification<Double> p0, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(load::setP0, load::getP0, p0, subReportNode, "Constant active power");
    }

    private void modifyLoadVoltageLevelBusOrBusBarSectionAttributes(LoadModificationInfos modificationInfos,
                                                                         Load load, ReportNode subReportNode) {
        if (ModificationUtils.getInstance().isNotModificationVoltageLevelBusOrBusBarInfos(modificationInfos)) {
            return;
        }

        Network network = load.getNetwork();
        Map<String, String> properties = !load.hasProperty()
                ? null
                : load.getPropertyNames().stream().collect(Collectors.toMap(name -> name, load::getProperty));
        LoadCreationInfos loadCreationInfos = createLoadCreationInfos(modificationInfos, load, subReportNode);
        new RemoveFeederBay(load.getId()).apply(network, true, subReportNode);
        createLoad(loadCreationInfos, subReportNode, network);
        var newLoad = ModificationUtils.getInstance().getLoad(network, modificationInfos.getEquipmentId());
        if (properties != null) {
            properties.forEach(newLoad::setProperty);
        }
    }

    private void createLoad(LoadCreationInfos modificationInfos, ReportNode subReportNode, Network network) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LoadAdder loadAdder = ModificationUtils.getInstance().createLoadAdderInNodeBreaker(voltageLevel, modificationInfos);
            ModificationUtils.getInstance().createInjectionInNodeBreaker(voltageLevel, modificationInfos.getBusOrBusbarSectionId(), modificationInfos.getConnectionPosition(),
                    modificationInfos.getConnectionDirection(), modificationInfos.getConnectionName() != null ? modificationInfos.getConnectionName() : modificationInfos.getEquipmentId(),
                    network, loadAdder, subReportNode);
        } else {
            ModificationUtils.getInstance().createLoadInBusBreaker(voltageLevel, modificationInfos);
        }
    }

    private LoadCreationInfos createLoadCreationInfos(LoadModificationInfos modificationInfos, Load load, ReportNode subReportNode) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevelInfos(modificationInfos.getVoltageLevelId(), load.getTerminal(),
                load.getNetwork(), ModificationUtils.FeederSide.INJECTION_SINGLE_SIDE, subReportNode);
        String busOrBusbarSectionId = ModificationUtils.getInstance().getBusOrBusBarSectionInfos(modificationInfos.getBusOrBusbarSectionId(),
                load.getTerminal(), ModificationUtils.FeederSide.INJECTION_SINGLE_SIDE, subReportNode);
        return LoadCreationInfos.builder().equipmentId(modificationInfos.getEquipmentId())
                .equipmentName(modificationInfos.getEquipmentName() != null ? modificationInfos.getEquipmentName().getValue() : load.getNameOrId())
                .voltageLevelId(voltageLevel.getId())
                .busOrBusbarSectionId(busOrBusbarSectionId)
                .connectionName(load.getExtension(ConnectablePosition.class) != null && load.getExtension(ConnectablePosition.class).getFeeder() != null ?
                        load.getExtension(ConnectablePosition.class).getFeeder().getName().orElse(modificationInfos.getEquipmentId()) : modificationInfos.getEquipmentId())
                .connectionDirection(load.getExtension(ConnectablePosition.class) != null && load.getExtension(ConnectablePosition.class).getFeeder() != null ?
                        load.getExtension(ConnectablePosition.class).getFeeder().getDirection() : ConnectablePosition.Direction.UNDEFINED)
                .connectionPosition(null)
                .terminalConnected(modificationInfos.getTerminalConnected() != null ? modificationInfos.getTerminalConnected().getValue() : load.getTerminal().isConnected())
                .loadType(modificationInfos.getLoadType() != null ? modificationInfos.getLoadType().getValue() : load.getLoadType())
                .p0(modificationInfos.getP0() != null ? modificationInfos.getP0().getValue() : load.getP0())
                .q0(modificationInfos.getQ0() != null ? modificationInfos.getQ0().getValue() : load.getQ0())
                .build();
    }

    private ReportNode modifyLoadConnectivityAttributes(LoadModificationInfos modificationInfos,
                                                        Load load, ReportNode subReportNode) {
        ConnectablePosition<Load> connectablePosition = load.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Load> connectablePositionAdder = load.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, load, modificationInfos, subReportNode);
    }
}
