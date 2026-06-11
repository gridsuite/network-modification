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
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.powsybl.iidm.network.TwoSides.ONE;
import static com.powsybl.iidm.network.TwoSides.TWO;
import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability.SIDE1;
import static org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability.SIDE2;
import static org.gridsuite.modification.utils.ModificationUtils.*;

public class TwoWindingsTransformerCreation extends AbstractModification {
    private final TwoWindingsTransformerCreationModel modificationModel;

    public TwoWindingsTransformerCreation(TwoWindingsTransformerCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getTwoWindingsTransformer(modificationModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS, modificationModel.getEquipmentId());
        }
        String errorMessage = "Two windings transformer '" + modificationModel.getEquipmentId() + "' : ";
        getInstance().controlBranchCreation(network,
            modificationModel.getVoltageLevelId1(), modificationModel.getBusOrBusbarSectionId1(),
            modificationModel.getVoltageLevelId2(), modificationModel.getBusOrBusbarSectionId2());
        checkIsNotNegativeValue(errorMessage, modificationModel.getR(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Resistance R");
        checkIsNotNegativeValue(errorMessage, modificationModel.getG(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Conductance G");
        checkIsNotNegativeValue(errorMessage, modificationModel.getRatedU1(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated Voltage on side 1");
        checkIsNotNegativeValue(errorMessage, modificationModel.getRatedU2(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated Voltage on side 2");
        checkIsNotNegativeValue(errorMessage, modificationModel.getRatedS(), CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated nominal power");
        if (modificationModel.getRatioTapChanger() != null) {
            checkIsNotNegativeValue(errorMessage, modificationModel.getRatioTapChanger().getTargetV(),
                CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Target voltage for ratio tap changer");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the 2wt in the network
        VoltageLevel voltageLevel1 = getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId1());
        VoltageLevel voltageLevel2 = getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId2());

        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER && voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            create2WTInNodeBreaker(network, voltageLevel1, voltageLevel2, subReportNode);
        } else {
            // Create 2wt in bus/mixed breaker
            create2WTInOtherBreaker(network, voltageLevel1, voltageLevel2, modificationModel, true, true, subReportNode);
        }
        getInstance().disconnectBranch(modificationModel, network.getTwoWindingsTransformer(modificationModel.getEquipmentId()), subReportNode);
        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.twoWindingsTransformerCreated")
            .withUntypedValue("id", modificationModel.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
    }

    @Override
    public String getName() {
        return "TwoWindingsTransformerCreation";
    }

    private void create2WTInNodeBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, ReportNode subReportNode) {
        var twoWindingsTransformerAdder = createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, modificationModel, false, false);
        createBranchInNodeBreaker(voltageLevel1, voltageLevel2, modificationModel, network, twoWindingsTransformerAdder, subReportNode);
        var twoWindingsTransformer = network.getTwoWindingsTransformer(modificationModel.getEquipmentId());
        completeTwoWindingsTransformerCreation(network, twoWindingsTransformer, modificationModel, subReportNode);
    }

    private TwoWindingsTransformerAdder createTwoWindingsTransformerAdder(VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel, boolean withSwitch1, boolean withSwitch2) {
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
        TwoWindingsTransformerAdder twoWindingsTransformerAdder = branchAdder.setId(twoWindingsTransformerCreationModel.getEquipmentId())
            .setName(twoWindingsTransformerCreationModel.getEquipmentName())
            .setVoltageLevel1(twoWindingsTransformerCreationModel.getVoltageLevelId1())
            .setVoltageLevel2(twoWindingsTransformerCreationModel.getVoltageLevelId2())
            .setG(twoWindingsTransformerCreationModel.getG())
            .setB(twoWindingsTransformerCreationModel.getB())
            .setR(twoWindingsTransformerCreationModel.getR())
            .setX(twoWindingsTransformerCreationModel.getX())
            .setRatedU1(twoWindingsTransformerCreationModel.getRatedU1())
            .setRatedU2(twoWindingsTransformerCreationModel.getRatedU2());

        if (twoWindingsTransformerCreationModel.getRatedS() != null) {
            twoWindingsTransformerAdder.setRatedS(twoWindingsTransformerCreationModel.getRatedS());
        }

        // BranchAdder completion by topology
        getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel1, twoWindingsTransformerCreationModel, TwoSides.ONE, withSwitch1);
        getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel2, twoWindingsTransformerCreationModel, TwoSides.TWO, withSwitch2);

        return twoWindingsTransformerAdder;
    }

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel, TwoWindingsTransformer twt, ReportNode subReportNode) {
        if (twoWindingsTransformerCreationModel.getRatioTapChanger() != null) {
            addRatioTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationModel, twt, subReportNode);
        }

        if (twoWindingsTransformerCreationModel.getPhaseTapChanger() != null) {
            addPhaseTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationModel, twt, subReportNode);
        }
    }

    private void addPhaseTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel, TwoWindingsTransformer twt, ReportNode subReportNode) {
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();
        List<ReportNode> regulationReports = new ArrayList<>();
        List<ReportNode> tapsReports = new ArrayList<>();

        PhaseTapChangerCreationModel phaseTapChangerModel = twoWindingsTransformerCreationModel.getPhaseTapChanger();
        PhaseTapChangerAdder phaseTapChangerAdder = twt.newPhaseTapChanger();
        double targetDeadband = phaseTapChangerModel.getTargetDeadband() != null ? phaseTapChangerModel.getTargetDeadband() : 0.;
        if (phaseTapChangerModel.isRegulating()) {
            phaseTapChangerAdder.setRegulationMode(phaseTapChangerModel.getRegulationMode())
                .setRegulationValue(phaseTapChangerModel.getRegulationValue()).setTargetDeadband(targetDeadband);
            regulationReports.add(getInstance().buildCreationReport(phaseTapChangerModel.getRegulationMode(), "Regulation mode"));
            regulationReports.add(getInstance().buildCreationReport(phaseTapChangerModel.getRegulationValue(), "Regulation value"));
            regulationReports.add(getInstance().buildCreationReport(targetDeadband, "Target deadband"));
        }
        Terminal terminal = getInstance().getTerminalFromIdentifiable(network,
            phaseTapChangerModel.getTerminalRefConnectableId(),
            phaseTapChangerModel.getTerminalRefConnectableType(),
            phaseTapChangerModel.getTerminalRefConnectableVlId());
        if (terminal != null) {
            phaseTapChangerAdder.setRegulationTerminal(terminal);
            regulatedTerminalReports.add(getInstance().buildCreationReport(phaseTapChangerModel.getTerminalRefConnectableVlId(), "Voltage level"));
            regulatedTerminalReports.add(getInstance().buildCreationReport(phaseTapChangerModel.getTerminalRefConnectableType() + " : " + phaseTapChangerModel.getTerminalRefConnectableId(), "Equipment"));
        }

        phaseTapChangerAdder.setRegulating(phaseTapChangerModel.isRegulating())
            .setLowTapPosition(phaseTapChangerModel.getLowTapPosition())
            .setTapPosition(phaseTapChangerModel.getTapPosition());
        tapsReports.add(getInstance().buildCreationReport(phaseTapChangerModel.getLowTapPosition(), "Low tap position"));
        tapsReports.add(getInstance().buildCreationReport(phaseTapChangerModel.getTapPosition(), "Tap position"));
        tapsReports.add(getInstance().buildCreationReport(phaseTapChangerModel.getSteps().size() - 1, "High tap position"));

        if (phaseTapChangerModel.getSteps() != null) {
            for (TapChangerStepCreationModel step : phaseTapChangerModel.getSteps()) {
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

    private void addRatioTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel, TwoWindingsTransformer twt, ReportNode subReportNode) {
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();
        List<ReportNode> regulationReports = new ArrayList<>();
        List<ReportNode> tapsReports = new ArrayList<>();
        RatioTapChangerCreationModel ratioTapChangerModel = twoWindingsTransformerCreationModel.getRatioTapChanger();
        RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();
        Terminal terminal = getInstance().getTerminalFromIdentifiable(network,
            ratioTapChangerModel.getTerminalRefConnectableId(),
            ratioTapChangerModel.getTerminalRefConnectableType(),
            ratioTapChangerModel.getTerminalRefConnectableVlId());
        if (terminal != null) {
            regulatedTerminalReports.add(getInstance().buildCreationReport(ratioTapChangerModel.getTerminalRefConnectableVlId(), "Voltage level"));
            regulatedTerminalReports.add(getInstance().buildCreationReport(ratioTapChangerModel.getTerminalRefConnectableType() + " : " + ratioTapChangerModel.getTerminalRefConnectableId(), "Equipment"));
        }

        Double targetDeadband = ratioTapChangerModel.getTargetDeadband();
        if (targetDeadband == null) {
            targetDeadband = ratioTapChangerModel.isRegulating() ? 0. : Double.NaN;
        }
        double targetV = ratioTapChangerModel.getTargetV() != null ? ratioTapChangerModel.getTargetV() : Double.NaN;
        ratioTapChangerAdder.setTargetV(targetV).setTargetDeadband(targetDeadband).setRegulationTerminal(terminal);
        ratioTapChangerAdder.setRegulating(ratioTapChangerModel.isRegulating()).setLoadTapChangingCapabilities(ratioTapChangerModel.isLoadTapChangingCapabilities());

        if (ratioTapChangerModel.isRegulating()) {
            regulationReports.add(getInstance().buildCreationReport(RatioRegulationModeType.VOLTAGE_REGULATION.name(), "Regulation mode"));
            regulationReports.add(getInstance().buildCreationReport(targetV, "Target voltage"));
            regulationReports.add(getInstance().buildCreationReport(targetDeadband, "Target deadband"));
        }

        ratioTapChangerAdder.setLowTapPosition(ratioTapChangerModel.getLowTapPosition()).setTapPosition(ratioTapChangerModel.getTapPosition());
        tapsReports.add(getInstance().buildCreationReport(ratioTapChangerModel.getLowTapPosition(), "Low tap position"));
        tapsReports.add(getInstance().buildCreationReport(ratioTapChangerModel.getTapPosition(), "Tap position"));
        tapsReports.add(getInstance().buildCreationReport(ratioTapChangerModel.getSteps().size() - 1, "High tap position"));

        if (ratioTapChangerModel.getSteps() != null) {
            for (TapChangerStepCreationModel step : ratioTapChangerModel.getSteps()) {
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

    private void create2WTInOtherBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel, boolean withSwitch1, boolean withSwitch2, ReportNode subReportNode) {
        var twoWindingsTransformer = createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, twoWindingsTransformerCreationModel, withSwitch1, withSwitch2).add();
        completeTwoWindingsTransformerCreation(network, twoWindingsTransformer, modificationModel, subReportNode);
    }

    private void completeTwoWindingsTransformerCreation(Network network,
                                                        TwoWindingsTransformer twoWindingsTransformer,
                                                        TwoWindingsTransformerCreationModel modificationModel,
                                                        ReportNode subReportNode) {

        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.Characteristics").add();

        // Connectivity
        reportBranchCreationConnectivity(modificationModel, characteristicsReporter);

        // properties
        PropertiesUtils.applyProperties(twoWindingsTransformer, characteristicsReporter, modificationModel.getProperties(), "network.modification.TwoWindingsTransformerProperties");

        // Set permanent and temporary current limits
        ReportNode limitsReporter = null;
        List<OperationalLimitsGroupModel> operationalLimitsGroups1 = ModificationUtils.getOperationalLimitsGroupsOnSide(modificationModel.getOperationalLimitsGroups(), SIDE1);
        List<OperationalLimitsGroupModel> operationalLimitsGroups2 = ModificationUtils.getOperationalLimitsGroupsOnSide(modificationModel.getOperationalLimitsGroups(), SIDE2);

        List<ReportNode> limitSetsOnSideReportNodes = new ArrayList<>();
        if (!CollectionUtils.isEmpty(modificationModel.getOperationalLimitsGroups())) {
            limitsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.limitsCreated").add();
            ReportNode reportNode = limitsReporter.newReportNode()
                .withSeverity(TypedValue.INFO_SEVERITY)
                .withMessageTemplate("network.modification.LimitSets")
                .add();

            for (OperationalLimitsGroupModel olgModel : modificationModel.getOperationalLimitsGroups()) {
                ReportNode limitSetNode = reportNode.newReportNode()
                    .withMessageTemplate("network.modification.limitSetAdded")
                    .withUntypedValue("name", olgModel.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();

                if (olgModel.getApplicability() == OperationalLimitsGroupModel.Applicability.SIDE1 || olgModel.getApplicability() == OperationalLimitsGroupModel.Applicability.EQUIPMENT) {
                    ModificationUtils.getInstance().setCurrentLimitsOnASide(limitSetNode, olgModel, twoWindingsTransformer, ONE);
                }
                if (olgModel.getApplicability() == OperationalLimitsGroupModel.Applicability.SIDE2 || olgModel.getApplicability() == OperationalLimitsGroupModel.Applicability.EQUIPMENT) {
                    ModificationUtils.getInstance().setCurrentLimitsOnASide(limitSetNode, olgModel, twoWindingsTransformer, TWO);
                }
            }
        }

        if (modificationModel.getSelectedOperationalLimitsGroupId1() != null) {
            if (!ModificationUtils.hasLimitSet(operationalLimitsGroups1, modificationModel.getSelectedOperationalLimitsGroupId1())) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetAbsentOnSide1")
                    .withUntypedValue("selectedOperationalLimitsGroup", modificationModel.getSelectedOperationalLimitsGroupId1())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            } else {
                twoWindingsTransformer.setSelectedOperationalLimitsGroup1(modificationModel.getSelectedOperationalLimitsGroupId1());
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetSelectedOnSide1")
                    .withUntypedValue("selectedOperationalLimitsGroup1", modificationModel.getSelectedOperationalLimitsGroupId1())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }
        }
        if (modificationModel.getSelectedOperationalLimitsGroupId2() != null) {
            if (!ModificationUtils.hasLimitSet(operationalLimitsGroups2, modificationModel.getSelectedOperationalLimitsGroupId2())) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetAbsentOnSide2")
                    .withUntypedValue("selectedOperationalLimitsGroup", modificationModel.getSelectedOperationalLimitsGroupId2())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            } else {
                twoWindingsTransformer.setSelectedOperationalLimitsGroup2(modificationModel.getSelectedOperationalLimitsGroupId2());
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("network.modification.limitSetSelectedOnSide2")
                    .withUntypedValue("selectedOperationalLimitsGroup2", modificationModel.getSelectedOperationalLimitsGroupId2())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }
        }

        if (!limitSetsOnSideReportNodes.isEmpty()) {
            if (limitsReporter == null) {
                limitsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.limitsCreated").add();
            }
            ModificationUtils.getInstance().reportModifications(limitsReporter, limitSetsOnSideReportNodes,
                "network.modification.ActiveLimitSets");
        }
        // tap changer
        addTapChangersToTwoWindingsTransformer(network, modificationModel, twoWindingsTransformer, characteristicsReporter);
    }
}
