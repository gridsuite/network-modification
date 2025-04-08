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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.utils.ModificationUtils.*;

public class TwoWindingsTransformerCreation extends AbstractModification {
    private static final String PHASE_TAP_CHANGER = "Phase Tap Changer";
    private static final String RATIO_TAP_CHANGER = "Ratio Tap Changer";
    private final TwoWindingsTransformerCreationInfos modificationInfos;

    public TwoWindingsTransformerCreation(TwoWindingsTransformerCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getTwoWindingsTransformer(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }
        String errorMessage = "Two windings transformer '" + modificationInfos.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().controlBranchCreation(network,
                modificationInfos.getVoltageLevelId1(), modificationInfos.getBusOrBusbarSectionId1(), modificationInfos.getConnectionPosition1(),
                modificationInfos.getVoltageLevelId2(), modificationInfos.getBusOrBusbarSectionId2(), modificationInfos.getConnectionPosition2());
        checkIsNotNegativeValue(errorMessage, modificationInfos.getR(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Resistance R");
        checkIsNotNegativeValue(errorMessage, modificationInfos.getG(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Conductance G");
        checkIsNotNegativeValue(errorMessage, modificationInfos.getRatedU1(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated Voltage on side 1");
        checkIsNotNegativeValue(errorMessage, modificationInfos.getRatedU2(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated Voltage on side 2");
        checkIsNotNegativeValue(errorMessage, modificationInfos.getRatedS(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated nominal power");
        if (modificationInfos.getRatioTapChanger() != null) {
            checkIsNotNegativeValue(errorMessage, modificationInfos.getRatioTapChanger().getTargetV(),
                CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Target voltage for ratio tap changer");
        }
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
            twoWindingsTransformer = create2WTInOtherBreaker(network, voltageLevel1, voltageLevel2, modificationInfos, true, true, subReportNode);
        }
        reportBranchCreationConnectivity(modificationInfos, subReportNode);
        ModificationUtils.getInstance().disconnectBranch(modificationInfos, network.getTwoWindingsTransformer(modificationInfos.getEquipmentId()), subReportNode);

        // Set permanent and temporary current limits
        setCurrentLimitsForSide(modificationInfos.getOperationalLimitsGroups1(), modificationInfos.getSelectedOperationalLimitsGroup1(), twoWindingsTransformer, TwoSides.ONE, subReportNode);
        setCurrentLimitsForSide(modificationInfos.getOperationalLimitsGroups2(), modificationInfos.getSelectedOperationalLimitsGroup2(), twoWindingsTransformer, TwoSides.TWO, subReportNode);

        // properties
        PropertiesUtils.applyProperties(twoWindingsTransformer, subReportNode, modificationInfos.getProperties(), "TwoWindingsTransformerProperties");
    }

    @Override
    public String getName() {
        return "TwoWindingsTransformerCreation";
    }

    private TwoWindingsTransformer create2WTInNodeBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, ReportNode subReportNode) {
        var twoWindingsTransformerAdder = createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, modificationInfos, false, false);
        createBranchInNodeBreaker(voltageLevel1, voltageLevel2, modificationInfos, network, twoWindingsTransformerAdder, subReportNode);
        var twt = network.getTwoWindingsTransformer(modificationInfos.getEquipmentId());
        addTapChangersToTwoWindingsTransformer(network, modificationInfos, twt, subReportNode);

        return twt;
    }

    private TwoWindingsTransformerAdder createTwoWindingsTransformerAdder(VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, boolean withSwitch1, boolean withSwitch2) {
        Optional<Substation> optS1 = voltageLevel1.getSubstation();
        Optional<Substation> optS2 = voltageLevel2.getSubstation();
        Substation s1 = optS1.orElse(null);
        Substation s2 = optS2.orElse(null);
        BranchAdder<TwoWindingsTransformer, TwoWindingsTransformerAdder> branchAdder;

        if (s1 != null) {
            branchAdder = s1.newTwoWindingsTransformer();
        } else if (s2 != null) {
            branchAdder = s2.newTwoWindingsTransformer();
        } else {
            throw new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_CREATION_ERROR, "The two windings transformer should belong to a substation");
        }
        // common settings
        TwoWindingsTransformerAdder twoWindingsTransformerAdder = branchAdder.setId(twoWindingsTransformerCreationInfos.getEquipmentId())
                .setName(twoWindingsTransformerCreationInfos.getEquipmentName())
                .setVoltageLevel1(twoWindingsTransformerCreationInfos.getVoltageLevelId1())
                .setVoltageLevel2(twoWindingsTransformerCreationInfos.getVoltageLevelId2())
                .setG(twoWindingsTransformerCreationInfos.getG())
                .setB(twoWindingsTransformerCreationInfos.getB())
                .setR(twoWindingsTransformerCreationInfos.getR())
                .setX(twoWindingsTransformerCreationInfos.getX())
                .setRatedU1(twoWindingsTransformerCreationInfos.getRatedU1())
                .setRatedU2(twoWindingsTransformerCreationInfos.getRatedU2());

        if (twoWindingsTransformerCreationInfos.getRatedS() != null) {
            twoWindingsTransformerAdder.setRatedS(twoWindingsTransformerCreationInfos.getRatedS());
        }

        // BranchAdder completion by topology
        ModificationUtils.getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel1, twoWindingsTransformerCreationInfos, TwoSides.ONE, withSwitch1);
        ModificationUtils.getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel2, twoWindingsTransformerCreationInfos, TwoSides.TWO, withSwitch2);

        return twoWindingsTransformerAdder;
    }

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, com.powsybl.iidm.network.TwoWindingsTransformer twt, ReportNode subReportNode) {
        if (twoWindingsTransformerCreationInfos.getRatioTapChanger() != null) {
            addRatioTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt, subReportNode);
        }

        if (twoWindingsTransformerCreationInfos.getPhaseTapChanger() != null) {
            addPhaseTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt, subReportNode);
        }
    }

    private void addPhaseTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, TwoWindingsTransformer twt, ReportNode subReportNode) {
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();
        PhaseTapChangerCreationInfos phaseTapChangerInfos = twoWindingsTransformerCreationInfos.getPhaseTapChanger();
        PhaseTapChangerAdder phaseTapChangerAdder = twt.newPhaseTapChanger();
        if (phaseTapChangerInfos.isRegulating()) {
            phaseTapChangerAdder.setRegulationValue(phaseTapChangerInfos.getRegulationValue())
                    .setTargetDeadband(phaseTapChangerInfos.getTargetDeadband() != null ? phaseTapChangerInfos.getTargetDeadband() : 0.);
        }
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
                phaseTapChangerInfos.getRegulatingTerminalId(),
                phaseTapChangerInfos.getRegulatingTerminalType(),
                phaseTapChangerInfos.getRegulatingTerminalVlId());
        if (terminal != null) {
            phaseTapChangerAdder.setRegulationTerminal(terminal);
            regulatedTerminalReports.add(ModificationUtils.getInstance().buildCreationReport(
                    phaseTapChangerInfos.getRegulatingTerminalVlId(),
                    "Voltage level"));
            regulatedTerminalReports.add(ModificationUtils.getInstance().buildCreationReport(
                    phaseTapChangerInfos.getRegulatingTerminalType() + ":"
                            + phaseTapChangerInfos.getRegulatingTerminalId(),
                    "Equipment"));
        }

        phaseTapChangerAdder.setRegulating(phaseTapChangerInfos.isRegulating())
                .setRegulationMode(phaseTapChangerInfos.getRegulationMode())
                .setLowTapPosition(phaseTapChangerInfos.getLowTapPosition())
                .setTapPosition(phaseTapChangerInfos.getTapPosition());

        if (phaseTapChangerInfos.getSteps() != null) {
            for (TapChangerStepCreationInfos step : phaseTapChangerInfos.getSteps()) {
                phaseTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).setAlpha(step.getAlpha()).endStep();
            }

            phaseTapChangerAdder.add();
            if (!regulatedTerminalReports.isEmpty()) {
                ReportNode ratioTapChangerReporter = subReportNode.newReportNode().withMessageTemplate("RatioTapChangerCreated", PHASE_TAP_CHANGER).add();
                ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, regulatedTerminalReports, "RegulatedTerminalCreated", "Regulated terminal");
            }
        }
    }

    private void addRatioTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, TwoWindingsTransformer twt, ReportNode subReportNode) {
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();
        RatioTapChangerCreationInfos ratioTapChangerInfos = twoWindingsTransformerCreationInfos.getRatioTapChanger();
        RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
                ratioTapChangerInfos.getRegulatingTerminalId(),
                ratioTapChangerInfos.getRegulatingTerminalType(),
                ratioTapChangerInfos.getRegulatingTerminalVlId());
        if (terminal != null) {
            regulatedTerminalReports.add(ModificationUtils.getInstance().buildCreationReport(
                    ratioTapChangerInfos.getRegulatingTerminalVlId(),
                    "Voltage level"));
            regulatedTerminalReports.add(ModificationUtils.getInstance().buildCreationReport(
                    ratioTapChangerInfos.getRegulatingTerminalType() + ":"
                            + ratioTapChangerInfos.getRegulatingTerminalId(),
                    "Equipment"));
        }

        ratioTapChangerAdder.setTargetV(ratioTapChangerInfos.getTargetV() != null ? ratioTapChangerInfos.getTargetV() : Double.NaN)
                .setTargetDeadband(ratioTapChangerInfos.getTargetDeadband() == null ? ratioTapChangerInfos.isRegulating() ? 0. : Double.NaN : ratioTapChangerInfos.getTargetDeadband())
                .setRegulationTerminal(terminal);

        ratioTapChangerAdder.setRegulating(ratioTapChangerInfos.isRegulating())
                .setLoadTapChangingCapabilities(ratioTapChangerInfos.isLoadTapChangingCapabilities());

        ratioTapChangerAdder.setLowTapPosition(ratioTapChangerInfos.getLowTapPosition())
                .setTapPosition(ratioTapChangerInfos.getTapPosition());

        if (ratioTapChangerInfos.getSteps() != null) {
            for (TapChangerStepCreationInfos step : ratioTapChangerInfos.getSteps()) {
                ratioTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).endStep();
            }
            ratioTapChangerAdder.add();
            if (!regulatedTerminalReports.isEmpty()) {
                ReportNode ratioTapChangerReporter = subReportNode.newReportNode().withMessageTemplate("RatioTapChangerCreated", RATIO_TAP_CHANGER).add();
                ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, regulatedTerminalReports, "RegulatedTerminalCreated", "Regulated terminal");
            }
        }
    }

    private TwoWindingsTransformer create2WTInOtherBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, boolean withSwitch1, boolean withSwitch2, ReportNode subReportNode) {
        var twt = createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, twoWindingsTransformerCreationInfos, withSwitch1, withSwitch2).add();
        addTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt, subReportNode);
        subReportNode.newReportNode()
                .withMessageTemplate("twoWindingsTransformerCreated", "New two windings transformer with id=${id} created")
                .withUntypedValue("id", twoWindingsTransformerCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        return twt;
    }

    private void setCurrentLimitsForSide(List<OperationalLimitsGroupInfos> operationalLimitsGroups, String selectedGroup, TwoWindingsTransformer transformer, TwoSides side, ReportNode subReportNode) {
        if (!CollectionUtils.isEmpty(operationalLimitsGroups)) {
            ModificationUtils.getInstance().setCurrentLimitsOnASide(operationalLimitsGroups, transformer, side, subReportNode);
        }
        if (selectedGroup != null) {
            if (side == TwoSides.ONE) {
                transformer.setSelectedOperationalLimitsGroup1(selectedGroup);
                subReportNode.newReportNode().withMessageTemplate(
                        "limit set selected on side 1",
                                "limit set selected on side 1 : ${selectedOperationalLimitsGroup1}")
                        .withUntypedValue("selectedOperationalLimitsGroup1", modificationInfos.getSelectedOperationalLimitsGroup1())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
            }
            if (side == TwoSides.TWO) {
                transformer.setSelectedOperationalLimitsGroup2(selectedGroup);
                subReportNode.newReportNode().withMessageTemplate(
                        "limit set selected on side 2",
                                "limit set selected on side 2 : ${selectedOperationalLimitsGroup2}")
                        .withUntypedValue("selectedOperationalLimitsGroup2", modificationInfos.getSelectedOperationalLimitsGroup2())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
            }
        }
    }
}
