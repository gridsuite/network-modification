/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS;
import static org.gridsuite.modification.utils.ModificationUtils.createBranchInNodeBreaker;

public class TwoWindingsTransformerCreation extends AbstractModification {

    private final TwoWindingsTransformerCreationInfos modificationInfos;

    public TwoWindingsTransformerCreation(TwoWindingsTransformerCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getTwoWindingsTransformer(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }
        ModificationUtils.getInstance().controlBranchCreation(network,
                modificationInfos.getVoltageLevelId1(), modificationInfos.getBusOrBusbarSectionId1(), modificationInfos.getConnectionPosition1(),
                modificationInfos.getVoltageLevelId2(), modificationInfos.getBusOrBusbarSectionId2(), modificationInfos.getConnectionPosition2());
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the 2wt in the network
        VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId1());
        VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId2());
        TwoWindingsTransformer twoWindingsTransformer;

        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER && voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            twoWindingsTransformer = create2WTInNodeBreaker(network, voltageLevel1, voltageLevel2, subReportNode);
        } else {
            // Create 2wt in bus/mixed breaker
            twoWindingsTransformer = create2WTInOtherBreaker(voltageLevel1, voltageLevel2, modificationInfos, true, true, subReportNode);
        }

        // Set permanent and temporary current limits
        List<OperationalLimitsGroupInfos> opLimitsGroupSide1 = modificationInfos.getOperationalLimitsGroups1();
        List<OperationalLimitsGroupInfos> opLimitsGroupSide2 = modificationInfos.getOperationalLimitsGroups2();
        if (!CollectionUtils.isEmpty(opLimitsGroupSide1)) {
            ModificationUtils.getInstance().setCurrentLimitsOnASide(opLimitsGroupSide1, twoWindingsTransformer, TwoSides.ONE, subReportNode);
        }
        if (!CollectionUtils.isEmpty(opLimitsGroupSide2)) {
            ModificationUtils.getInstance().setCurrentLimitsOnASide(opLimitsGroupSide2, twoWindingsTransformer, TwoSides.TWO, subReportNode);
        }
        if (modificationInfos.getSelectedOperationalLimitsGroup1() != null) {
            twoWindingsTransformer.setSelectedOperationalLimitsGroup1(modificationInfos.getSelectedOperationalLimitsGroup1());
            subReportNode.newReportNode()
                    .withMessageTemplate("limit set selected on side 1", "limit set selected on side 1 : ${selectedOperationalLimitsGroup1}")
                    .withUntypedValue("selectedOperationalLimitsGroup1", modificationInfos.getSelectedOperationalLimitsGroup1())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        if (modificationInfos.getSelectedOperationalLimitsGroup2() != null) {
            twoWindingsTransformer.setSelectedOperationalLimitsGroup2(modificationInfos.getSelectedOperationalLimitsGroup2());
            subReportNode.newReportNode()
                    .withMessageTemplate("limit set selected on side 2", "limit set selected on side 2 : ${selectedOperationalLimitsGroup2}")
                    .withUntypedValue("selectedOperationalLimitsGroup2", modificationInfos.getSelectedOperationalLimitsGroup2())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }

        ModificationUtils.getInstance().disconnectBranch(modificationInfos, network.getTwoWindingsTransformer(modificationInfos.getEquipmentId()), subReportNode);
        // properties
        PropertiesUtils.applyProperties(twoWindingsTransformer, subReportNode, modificationInfos.getProperties(), "TwoWindingsTransformerProperties");
    }

    @Override
    public String getName() {
        return "TwoWindingsTransformerCreation";
    }

    private TwoWindingsTransformer create2WTInNodeBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, ReportNode subReportNode) {
        var twoWindingsTransformerAdder = ModificationUtils.getInstance().createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, modificationInfos, false, false);
        createBranchInNodeBreaker(voltageLevel1, voltageLevel2, modificationInfos.getBusOrBusbarSectionId1(), modificationInfos.getBusOrBusbarSectionId2(),
                modificationInfos.getConnectionPosition1(), modificationInfos.getConnectionPosition2(), modificationInfos.getConnectionDirection1(),
                modificationInfos.getConnectionDirection2(), modificationInfos.getConnectionName1() != null ? modificationInfos.getConnectionName1() : modificationInfos.getEquipmentId(),
                modificationInfos.getConnectionName2() != null ? modificationInfos.getConnectionName2() : modificationInfos.getEquipmentId(),
                network, twoWindingsTransformerAdder, subReportNode);

        var twt = network.getTwoWindingsTransformer(modificationInfos.getEquipmentId());
        addTapChangersToTwoWindingsTransformer(modificationInfos, twt);

        return twt;
    }

    private void addTapChangersToTwoWindingsTransformer(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, com.powsybl.iidm.network.TwoWindingsTransformer twt) {
        if (twoWindingsTransformerCreationInfos.getRatioTapChanger() != null) {
            ModificationUtils.getInstance().addRatioTapChangersToTwoWindingsTransformer(twoWindingsTransformerCreationInfos, twt);
        }

        if (twoWindingsTransformerCreationInfos.getPhaseTapChanger() != null) {
            ModificationUtils.getInstance().addPhaseTapChangersToTwoWindingsTransformer(twoWindingsTransformerCreationInfos, twt);
        }
    }

    private TwoWindingsTransformer create2WTInOtherBreaker(VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, boolean withSwitch1, boolean withSwitch2, ReportNode subReportNode) {
        var twt = ModificationUtils.getInstance().createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, twoWindingsTransformerCreationInfos, withSwitch1, withSwitch2).add();
        addTapChangersToTwoWindingsTransformer(twoWindingsTransformerCreationInfos, twt);
        subReportNode.newReportNode()
                .withMessageTemplate("twoWindingsTransformerCreated", "New two windings transformer with id=${id} created")
                .withUntypedValue("id", twoWindingsTransformerCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        return twt;
    }

}
