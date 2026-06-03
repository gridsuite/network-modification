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
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.AttributeModification;
import org.gridsuite.modification.model.LoadModificationModel;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.LOAD_NOT_FOUND;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class LoadModification extends AbstractInjectionModification {

    public LoadModification(LoadModificationModel modificationModel) {
        super(modificationModel);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Load load = network.getLoad(modificationModel.getEquipmentId());
        if (load == null) {
            throw new NetworkModificationException(LOAD_NOT_FOUND,
                    "Load " + modificationModel.getEquipmentId() + " does not exist in network");
        }
        ModificationUtils.getInstance().checkVoltageLevelModification(network, modificationModel.getVoltageLevelId(),
                modificationModel.getBusOrBusbarSectionId(), load.getTerminal());
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Load load = network.getLoad(modificationModel.getEquipmentId());
        // modify the load in the network
        modifyLoad(load, subReportNode);
    }

    @Override
    public String getName() {
        return "LoadModification";
    }

    private void modifyLoad(Load load, ReportNode subReportNode) {
        LoadModificationModel loadModificationModel = (LoadModificationModel) modificationModel;
        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.loadModification")
            .withUntypedValue("id", loadModificationModel.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        ModificationUtils.getInstance().applyElementaryModifications(load::setName, () -> load.getOptionalName().orElse("No value"), loadModificationModel.getEquipmentName(), subReportNode, "Name");
        ModificationUtils.getInstance().applyElementaryModifications(load::setLoadType, load::getLoadType, loadModificationModel.getLoadType(), subReportNode, "Type");
        modifyLoadVoltageLevelBusOrBusBarSectionAttributes(loadModificationModel, load, subReportNode);
        modifyLoadConnectivityAttributes(loadModificationModel, load, subReportNode);
        modifyP0(load, loadModificationModel.getP0(), subReportNode);
        modifyQ0(load, loadModificationModel.getQ0(), subReportNode);
        // measurements
        updateMeasurements(load, loadModificationModel, subReportNode);
        // properties
        PropertiesUtils.applyProperties(load, subReportNode, loadModificationModel.getProperties(), "network.modification.LoadProperties");
    }

    public static void modifyQ0(Load load, AttributeModification<Double> q0, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(load::setQ0, load::getQ0, q0, subReportNode, "Constant reactive power");
    }

    public static void modifyP0(Load load, AttributeModification<Double> p0, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(load::setP0, load::getP0, p0, subReportNode, "Constant active power");
    }

    private void modifyLoadVoltageLevelBusOrBusBarSectionAttributes(LoadModificationModel modificationModel,
                                                                    Load load, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                load, load.getTerminal(),
                modificationModel.getVoltageLevelId(),
                modificationModel.getBusOrBusbarSectionId(),
                subReportNode
        );
    }

    private ReportNode modifyLoadConnectivityAttributes(LoadModificationModel loadModificationModel,
                                                        Load load, ReportNode subReportNode) {
        ConnectablePosition<Load> connectablePosition = load.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Load> connectablePositionAdder = load.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, load, loadModificationModel, subReportNode);
    }

}
