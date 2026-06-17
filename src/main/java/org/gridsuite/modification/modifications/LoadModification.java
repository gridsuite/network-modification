/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.LOAD_NOT_FOUND;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
@Getter
@Setter
public class LoadModification extends AbstractInjectionModification {

    private AttributeModification<LoadType> loadType;
    private AttributeModification<Double> p0;
    private AttributeModification<Double> q0;

    @Builder
    public LoadModification(String equipmentId, List<FreePropertyInfos> properties,
                            AttributeModification<String> equipmentName, AttributeModification<String> voltageLevelId,
                            AttributeModification<String> busOrBusbarSectionId,
                            AttributeModification<String> connectionName,
                            AttributeModification<ConnectablePosition.Direction> connectionDirection,
                            AttributeModification<Integer> connectionPosition,
                            AttributeModification<Boolean> terminalConnected,
                            AttributeModification<Double> pMeasurementValue,
                            AttributeModification<Boolean> pMeasurementValidity,
                            AttributeModification<Double> qMeasurementValue,
                            AttributeModification<Boolean> qMeasurementValidity,
                            AttributeModification<LoadType> loadType,
                            AttributeModification<Double> p0, AttributeModification<Double> q0) {
        super(equipmentId, properties, equipmentName, voltageLevelId, busOrBusbarSectionId, connectionName,
            connectionDirection, connectionPosition, terminalConnected, pMeasurementValue, pMeasurementValidity,
            qMeasurementValue, qMeasurementValidity);
        this.loadType = loadType;
        this.p0 = p0;
        this.q0 = q0;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Load load = network.getLoad(equipmentId);
        if (load == null) {
            throw new NetworkModificationException(LOAD_NOT_FOUND,
                    "Load " + equipmentId + " does not exist in network");
        }
        ModificationUtils.getInstance().checkVoltageLevelModification(network, voltageLevelId,
                busOrBusbarSectionId, load.getTerminal());
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Load load = network.getLoad(equipmentId);
        // modify the load in the network
        modifyLoad(load, subReportNode);
    }

    @Override
    public String getName() {
        return "LoadModification";
    }

    private void modifyLoad(Load load, ReportNode subReportNode) {
        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.loadModification")
            .withUntypedValue("id", equipmentId)
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        ModificationUtils.getInstance().applyElementaryModifications(load::setName, () -> load.getOptionalName().orElse("No value"), equipmentName, subReportNode, "Name");
        ModificationUtils.getInstance().applyElementaryModifications(load::setLoadType, load::getLoadType, loadType, subReportNode, "Type");
        modifyLoadVoltageLevelBusOrBusBarSectionAttributes(load, subReportNode);
        modifyLoadConnectivityAttributes(load, subReportNode);
        modifyP0(load, p0, subReportNode);
        modifyQ0(load, q0, subReportNode);
        // measurements
        updateMeasurements(load, subReportNode);
        // properties
        PropertiesUtils.applyProperties(load, subReportNode, properties, "network.modification.LoadProperties");
    }

    public static void modifyQ0(Load load, AttributeModification<Double> q0, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(load::setQ0, load::getQ0, q0, subReportNode, "Constant reactive power");
    }

    public static void modifyP0(Load load, AttributeModification<Double> p0, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(load::setP0, load::getP0, p0, subReportNode, "Constant active power");
    }

    private void modifyLoadVoltageLevelBusOrBusBarSectionAttributes(Load load, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                load, load.getTerminal(),
                voltageLevelId,
                busOrBusbarSectionId,
                subReportNode
        );
    }

    private ReportNode modifyLoadConnectivityAttributes(Load load, ReportNode subReportNode) {
        ConnectablePosition<Load> connectablePosition = load.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Load> connectablePositionAdder = load.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, load,
                equipmentId, voltageLevelId, busOrBusbarSectionId, connectionName, connectionDirection, connectionPosition, terminalConnected, subReportNode);
    }

}
