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
import org.gridsuite.filter.utils.expertfilter.RatioRegulationModeType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE1;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE2;
import static org.gridsuite.modification.utils.ModificationUtils.*;

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
        String errorMessage = "Two windings transformer '" + modificationInfos.getEquipmentId() + "' : ";
        getInstance().controlBranchCreation(network,
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
        VoltageLevel voltageLevel1 = getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId1());
        VoltageLevel voltageLevel2 = getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId2());

        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER && voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            create2WTInNodeBreaker(network, voltageLevel1, voltageLevel2, subReportNode);
        } else {
            // Create 2wt in bus/mixed breaker
            create2WTInOtherBreaker(network, voltageLevel1, voltageLevel2, modificationInfos, true, true, subReportNode);
        }
        getInstance().disconnectBranch(modificationInfos, network.getTwoWindingsTransformer(modificationInfos.getEquipmentId()), subReportNode);
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.twoWindingsTransformerCreated")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    @Override
    public String getName() {
        return "TwoWindingsTransformerCreation";
    }

    private void create2WTInNodeBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, ReportNode subReportNode) {
        var twoWindingsTransformerAdder = createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, modificationInfos, false, false);
        createBranchInNodeBreaker(voltageLevel1, voltageLevel2, modificationInfos, network, twoWindingsTransformerAdder, subReportNode);
        var twoWindingsTransformer = network.getTwoWindingsTransformer(modificationInfos.getEquipmentId());
        completeTwoWindingsTransformerCreation(network, twoWindingsTransformer, modificationInfos, subReportNode);
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
        getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel1, twoWindingsTransformerCreationInfos, TwoSides.ONE, withSwitch1);
        getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel2, twoWindingsTransformerCreationInfos, TwoSides.TWO, withSwitch2);

        return twoWindingsTransformerAdder;
    }

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, TwoWindingsTransformer twt, ReportNode subReportNode) {
        if (twoWindingsTransformerCreationInfos.getRatioTapChanger() != null) {
            addRatioTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt, subReportNode);
        }

        if (twoWindingsTransformerCreationInfos.getPhaseTapChanger() != null) {
            addPhaseTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt, subReportNode);
        }
    }

    private void addPhaseTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, TwoWindingsTransformer twt, ReportNode subReportNode) {
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();
        List<ReportNode> regulationReports = new ArrayList<>();
        List<ReportNode> tapsReports = new ArrayList<>();

        PhaseTapChangerCreationInfos phaseTapChangerInfos = twoWindingsTransformerCreationInfos.getPhaseTapChanger();
        PhaseTapChangerAdder phaseTapChangerAdder = twt.newPhaseTapChanger();
        double targetDeadband = phaseTapChangerInfos.getTargetDeadband() != null ? phaseTapChangerInfos.getTargetDeadband() : 0.;
        if (phaseTapChangerInfos.isRegulating()) {
            phaseTapChangerAdder.setRegulationValue(phaseTapChangerInfos.getRegulationValue()).setTargetDeadband(targetDeadband);
            regulationReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getRegulationMode(), "Regulation mode"));
            regulationReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getRegulationValue(), "Regulation value"));
            regulationReports.add(getInstance().buildCreationReport(targetDeadband, "Target deadband"));
        }
        Terminal terminal = getInstance().getTerminalFromIdentifiable(network,
                phaseTapChangerInfos.getRegulatingTerminalId(),
                phaseTapChangerInfos.getRegulatingTerminalType(),
                phaseTapChangerInfos.getRegulatingTerminalVlId());
        if (terminal != null) {
            phaseTapChangerAdder.setRegulationTerminal(terminal);
            regulatedTerminalReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getRegulatingTerminalVlId(), "Voltage level"));
            regulatedTerminalReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getRegulatingTerminalType() + " : " + phaseTapChangerInfos.getRegulatingTerminalId(), "Equipment"));
        }

        phaseTapChangerAdder.setRegulating(phaseTapChangerInfos.isRegulating())
                .setRegulationMode(phaseTapChangerInfos.getRegulationMode())
                .setLowTapPosition(phaseTapChangerInfos.getLowTapPosition())
                .setTapPosition(phaseTapChangerInfos.getTapPosition());
        tapsReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getLowTapPosition(), "Low tap position"));
        tapsReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getTapPosition(), "Tap position"));
        tapsReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getSteps().size() - 1, "High tap position"));

        if (phaseTapChangerInfos.getSteps() != null) {
            for (TapChangerStepCreationInfos step : phaseTapChangerInfos.getSteps()) {
                phaseTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).setAlpha(step.getAlpha()).endStep();
            }

            phaseTapChangerAdder.add();
            ReportNode ratioTapChangerReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.PhaseTapChangerCreated").add();
            if (!regulationReports.isEmpty()) {
                getInstance().reportModifications(ratioTapChangerReporter, regulationReports, "network.modification.RegulatingCreated");
            }
            if (!regulatedTerminalReports.isEmpty()) {
                getInstance().reportModifications(ratioTapChangerReporter, regulatedTerminalReports, "network.modification.RegulatedTerminalCreated");
            }
            if (!tapsReports.isEmpty()) {
                getInstance().reportModifications(ratioTapChangerReporter, tapsReports, "network.modification.TapsCreated");
            }
        }
    }

    private void addRatioTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, TwoWindingsTransformer twt, ReportNode subReportNode) {
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();
        List<ReportNode> regulationReports = new ArrayList<>();
        List<ReportNode> tapsReports = new ArrayList<>();
        RatioTapChangerCreationInfos ratioTapChangerInfos = twoWindingsTransformerCreationInfos.getRatioTapChanger();
        RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();
        Terminal terminal = getInstance().getTerminalFromIdentifiable(network,
                ratioTapChangerInfos.getRegulatingTerminalId(),
                ratioTapChangerInfos.getRegulatingTerminalType(),
                ratioTapChangerInfos.getRegulatingTerminalVlId());
        if (terminal != null) {
            regulatedTerminalReports.add(getInstance().buildCreationReport(ratioTapChangerInfos.getRegulatingTerminalVlId(), "Voltage level"));
            regulatedTerminalReports.add(getInstance().buildCreationReport(ratioTapChangerInfos.getRegulatingTerminalType() + " : " + ratioTapChangerInfos.getRegulatingTerminalId(), "Equipment"));
        }

        Double targetDeadband = ratioTapChangerInfos.getTargetDeadband();
        if (targetDeadband == null) {
            targetDeadband = ratioTapChangerInfos.isRegulating() ? 0. : Double.NaN;
        }
        double targetV = ratioTapChangerInfos.getTargetV() != null ? ratioTapChangerInfos.getTargetV() : Double.NaN;
        ratioTapChangerAdder.setTargetV(targetV).setTargetDeadband(targetDeadband).setRegulationTerminal(terminal);
        ratioTapChangerAdder.setRegulating(ratioTapChangerInfos.isRegulating()).setLoadTapChangingCapabilities(ratioTapChangerInfos.isLoadTapChangingCapabilities());

        if (ratioTapChangerInfos.isRegulating()) {
            regulationReports.add(getInstance().buildCreationReport(RatioRegulationModeType.VOLTAGE_REGULATION.name(), "Regulation mode"));
            regulationReports.add(getInstance().buildCreationReport(targetV, "Target voltage"));
            regulationReports.add(getInstance().buildCreationReport(targetDeadband, "Target deadband"));
        }

        ratioTapChangerAdder.setLowTapPosition(ratioTapChangerInfos.getLowTapPosition()).setTapPosition(ratioTapChangerInfos.getTapPosition());
        tapsReports.add(getInstance().buildCreationReport(ratioTapChangerInfos.getLowTapPosition(), "Low tap position"));
        tapsReports.add(getInstance().buildCreationReport(ratioTapChangerInfos.getTapPosition(), "Tap position"));
        tapsReports.add(getInstance().buildCreationReport(ratioTapChangerInfos.getSteps().size() - 1, "High tap position"));

        if (ratioTapChangerInfos.getSteps() != null) {
            for (TapChangerStepCreationInfos step : ratioTapChangerInfos.getSteps()) {
                ratioTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).endStep();
            }
            ratioTapChangerAdder.add();
            ReportNode ratioTapChangerReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.RatioTapChangerCreated").add();
            if (!regulationReports.isEmpty()) {
                getInstance().reportModifications(ratioTapChangerReporter, regulationReports, "network.modification.RegulationCreated");
            }
            if (!regulatedTerminalReports.isEmpty()) {
                getInstance().reportModifications(ratioTapChangerReporter, regulatedTerminalReports, "network.modification.RegulatedTerminalCreated");
            }
            if (!tapsReports.isEmpty()) {
                getInstance().reportModifications(ratioTapChangerReporter, tapsReports, "network.modification.TapsCreated");
            }
        }
    }

    private void create2WTInOtherBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, boolean withSwitch1, boolean withSwitch2, ReportNode subReportNode) {
        var twoWindingsTransformer = createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, twoWindingsTransformerCreationInfos, withSwitch1, withSwitch2).add();
        completeTwoWindingsTransformerCreation(network, twoWindingsTransformer, modificationInfos, subReportNode);
    }

    private void setCurrentLimitsForSide(List<OperationalLimitsGroupInfos> operationalLimitsGroups, String selectedGroup, TwoWindingsTransformer transformer, TwoSides side, ReportNode limitsReporter) {
        if (!CollectionUtils.isEmpty(operationalLimitsGroups)) {
            getInstance().setCurrentLimitsOnASide(operationalLimitsGroups, transformer, side, limitsReporter);
        }
        if (selectedGroup != null) {
            if (side == TwoSides.ONE) {
                transformer.setSelectedOperationalLimitsGroup1(selectedGroup);
                limitsReporter.newReportNode().withMessageTemplate("network.modification.limitSetSelectedOnSide1")
                        .withUntypedValue("selectedOperationalLimitsGroup1", modificationInfos.getSelectedOperationalLimitsGroup1())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
            }
            if (side == TwoSides.TWO) {
                transformer.setSelectedOperationalLimitsGroup2(selectedGroup);
                limitsReporter.newReportNode().withMessageTemplate("network.modification.limitSetSelectedOnSide2")
                        .withUntypedValue("selectedOperationalLimitsGroup2", modificationInfos.getSelectedOperationalLimitsGroup2())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
            }
        }
    }

    private void completeTwoWindingsTransformerCreation(Network network,
                                                        TwoWindingsTransformer twoWindingsTransformer,
                                                        TwoWindingsTransformerCreationInfos modificationInfos,
                                                        ReportNode subReportNode) {
        // Connectivity
        reportBranchCreationConnectivity(modificationInfos, subReportNode);

        // properties
        PropertiesUtils.applyProperties(twoWindingsTransformer, subReportNode, modificationInfos.getProperties(), "network.modification.TwoWindingsTransformerProperties");

        // Set permanent and temporary current limits
        ReportNode limitsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.limitsCreated").add();
        List<OperationalLimitsGroupInfos> operationalLimitsGroups1 = ModificationUtils.getOperationalLimitsGroupsOnSide(modificationInfos.getOperationalLimitsGroups(), SIDE1);
        List<OperationalLimitsGroupInfos> operationalLimitsGroups2 = ModificationUtils.getOperationalLimitsGroupsOnSide(modificationInfos.getOperationalLimitsGroups(), SIDE2);
        setCurrentLimitsForSide(operationalLimitsGroups1, modificationInfos.getSelectedOperationalLimitsGroup1(), twoWindingsTransformer, TwoSides.ONE, limitsReporter);
        setCurrentLimitsForSide(operationalLimitsGroups2, modificationInfos.getSelectedOperationalLimitsGroup2(), twoWindingsTransformer, TwoSides.TWO, limitsReporter);

        // tap changer
        addTapChangersToTwoWindingsTransformer(network, modificationInfos, twoWindingsTransformer, subReportNode);
    }
}
