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
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.*;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.filter.utils.expertfilter.RatioRegulationModeType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.powsybl.iidm.network.TwoSides.ONE;
import static com.powsybl.iidm.network.TwoSides.TWO;
import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE1;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE2;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Getter
@Setter
public class TwoWindingsTransformerCreation extends AbstractBranchCreation {

    private double g;
    private double b;
    private double ratedU1;
    private double ratedU2;
    private Double ratedS;
    private RatioTapChangerCreationInfos ratioTapChanger;
    private PhaseTapChangerCreationInfos phaseTapChanger;

    @Builder
    public TwoWindingsTransformerCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName,
                                          String busOrBusbarSectionId2, double r, double x, String voltageLevelId1,
                                          String voltageLevelId2, String busOrBusbarSectionId1,
                                          List<OperationalLimitsGroupInfos> operationalLimitsGroups,
                                          String selectedOperationalLimitsGroupId1,
                                          String selectedOperationalLimitsGroupId2, String connectionName1,
                                          ConnectablePosition.Direction connectionDirection1, String connectionName2,
                                          ConnectablePosition.Direction connectionDirection2,
                                          Integer connectionPosition1,
                                          Integer connectionPosition2, boolean connected1, boolean connected2, double g,
                                          double b, double ratedU1, double ratedU2, Double ratedS,
                                          RatioTapChangerCreationInfos ratioTapChanger,
                                          PhaseTapChangerCreationInfos phaseTapChanger) {
        super(equipmentId, properties, equipmentName, busOrBusbarSectionId2, r, x, voltageLevelId1, voltageLevelId2,
            busOrBusbarSectionId1, operationalLimitsGroups, selectedOperationalLimitsGroupId1,
            selectedOperationalLimitsGroupId2, connectionName1, connectionDirection1, connectionName2,
            connectionDirection2,
            connectionPosition1, connectionPosition2, connected1, connected2);
        this.g = g;
        this.b = b;
        this.ratedU1 = ratedU1;
        this.ratedU2 = ratedU2;
        this.ratedS = ratedS;
        this.ratioTapChanger = ratioTapChanger;
        this.phaseTapChanger = phaseTapChanger;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getTwoWindingsTransformer(equipmentId) != null) {
            throw new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS, equipmentId);
        }
        String errorMessage = "Two windings transformer '" + equipmentId + "' : ";
        getInstance().controlBranchCreation(network,
                voltageLevelId1, busOrBusbarSectionId1,
                voltageLevelId2, busOrBusbarSectionId2);
        checkIsNotNegativeValue(errorMessage, r, CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Resistance R");
        checkIsNotNegativeValue(errorMessage, g, CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Conductance G");
        checkIsNotNegativeValue(errorMessage, ratedU1, CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated Voltage on side 1");
        checkIsNotNegativeValue(errorMessage, ratedU2, CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated Voltage on side 2");
        checkIsNotNegativeValue(errorMessage, ratedS, CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated nominal power");
        if (ratioTapChanger != null) {
            checkIsNotNegativeValue(errorMessage, ratioTapChanger.getTargetV(),
                CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Target voltage for ratio tap changer");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the 2wt in the network
        VoltageLevel voltageLevel1 = getInstance().getVoltageLevel(network, voltageLevelId1);
        VoltageLevel voltageLevel2 = getInstance().getVoltageLevel(network, voltageLevelId2);

        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER && voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            create2WTInNodeBreaker(network, voltageLevel1, voltageLevel2, subReportNode);
        } else {
            // Create 2wt in bus/mixed breaker
            create2WTInOtherBreaker(network, voltageLevel1, voltageLevel2, true, true, subReportNode);
        }
        getInstance().disconnectBranch(this, network.getTwoWindingsTransformer(equipmentId), subReportNode);
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.twoWindingsTransformerCreated")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    @Override
    public String getName() {
        return "TwoWindingsTransformerCreation";
    }

    private void create2WTInNodeBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, ReportNode subReportNode) {
        var twoWindingsTransformerAdder = createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, false, false);
        createBranchInNodeBreaker(voltageLevel1, voltageLevel2, this, network, twoWindingsTransformerAdder, subReportNode);
        var twoWindingsTransformer = network.getTwoWindingsTransformer(equipmentId);
        completeTwoWindingsTransformerCreation(network, twoWindingsTransformer, subReportNode);
    }

    private TwoWindingsTransformerAdder createTwoWindingsTransformerAdder(VoltageLevel voltageLevel1, VoltageLevel voltageLevel2,
            boolean withSwitch1, boolean withSwitch2) {
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
        TwoWindingsTransformerAdder twoWindingsTransformerAdder = branchAdder.setId(equipmentId)
                .setName(equipmentName)
                .setVoltageLevel1(voltageLevelId1)
                .setVoltageLevel2(voltageLevelId2)
                .setG(g)
                .setB(b)
                .setR(r)
                .setX(x)
                .setRatedU1(ratedU1)
                .setRatedU2(ratedU2);

        if (ratedS != null) {
            twoWindingsTransformerAdder.setRatedS(ratedS);
        }

        // BranchAdder completion by topology
        getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel1, this, TwoSides.ONE, withSwitch1);
        getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel2, this, TwoSides.TWO, withSwitch2);

        return twoWindingsTransformerAdder;
    }

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformer twt,
                                                        ReportNode subReportNode) {
        if (ratioTapChanger != null) {
            addRatioTapChangersToTwoWindingsTransformer(network, twt, subReportNode);
        }

        if (phaseTapChanger != null) {
            addPhaseTapChangersToTwoWindingsTransformer(network, twt, subReportNode);
        }
    }

    private void addPhaseTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformer twt,
            ReportNode subReportNode) {
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();
        List<ReportNode> regulationReports = new ArrayList<>();
        List<ReportNode> tapsReports = new ArrayList<>();

        PhaseTapChangerCreationInfos phaseTapChangerInfos = phaseTapChanger;
        PhaseTapChangerAdder phaseTapChangerAdder = twt.newPhaseTapChanger();
        double targetDeadband = phaseTapChangerInfos.getTargetDeadband() != null ? phaseTapChangerInfos.getTargetDeadband() : 0.;
        if (phaseTapChangerInfos.isRegulating()) {
            phaseTapChangerAdder.setRegulationMode(phaseTapChangerInfos.getRegulationMode())
                    .setRegulationValue(phaseTapChangerInfos.getRegulationValue()).setTargetDeadband(targetDeadband);
            regulationReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getRegulationMode(), "Regulation mode"));
            regulationReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getRegulationValue(), "Regulation value"));
            regulationReports.add(getInstance().buildCreationReport(targetDeadband, "Target deadband"));
        }
        Terminal terminal = getInstance().getTerminalFromIdentifiable(network,
                phaseTapChangerInfos.getTerminalRefConnectableId(),
                phaseTapChangerInfos.getTerminalRefConnectableType(),
                phaseTapChangerInfos.getTerminalRefConnectableVlId());
        if (terminal != null) {
            phaseTapChangerAdder.setRegulationTerminal(terminal);
            regulatedTerminalReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getTerminalRefConnectableVlId(), "Voltage level"));
            regulatedTerminalReports.add(getInstance().buildCreationReport(phaseTapChangerInfos.getTerminalRefConnectableType() + " : " + phaseTapChangerInfos.getTerminalRefConnectableId(),
                    "Equipment"));
        }

        phaseTapChangerAdder.setRegulating(phaseTapChangerInfos.isRegulating())
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

    private void addRatioTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformer twt,
            ReportNode subReportNode) {
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();
        List<ReportNode> regulationReports = new ArrayList<>();
        List<ReportNode> tapsReports = new ArrayList<>();
        RatioTapChangerCreationInfos ratioTapChangerInfos = ratioTapChanger;
        RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();
        Terminal terminal = getInstance().getTerminalFromIdentifiable(network,
                ratioTapChangerInfos.getTerminalRefConnectableId(),
                ratioTapChangerInfos.getTerminalRefConnectableType(),
                ratioTapChangerInfos.getTerminalRefConnectableVlId());
        if (terminal != null) {
            regulatedTerminalReports.add(getInstance().buildCreationReport(ratioTapChangerInfos.getTerminalRefConnectableVlId(), "Voltage level"));
            regulatedTerminalReports.add(getInstance().buildCreationReport(ratioTapChangerInfos.getTerminalRefConnectableType() + " : " + ratioTapChangerInfos.getTerminalRefConnectableId(),
                    "Equipment"));
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

    private void create2WTInOtherBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2,
            boolean withSwitch1, boolean withSwitch2, ReportNode subReportNode) {
        var twoWindingsTransformer = createTwoWindingsTransformerAdder(voltageLevel1, voltageLevel2, withSwitch1, withSwitch2).add();
        completeTwoWindingsTransformerCreation(network, twoWindingsTransformer, subReportNode);
    }

    private void completeTwoWindingsTransformerCreation(Network network,
                                                        TwoWindingsTransformer twoWindingsTransformer,
                                                        ReportNode subReportNode) {

        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.Characteristics").add();

        // Connectivity
        reportBranchCreationConnectivity(this, characteristicsReporter);

        // properties
        PropertiesUtils.applyProperties(twoWindingsTransformer, characteristicsReporter, properties, "network.modification.TwoWindingsTransformerProperties");

        // Set permanent and temporary current limits
        ReportNode limitsReporter = null;
        List<OperationalLimitsGroupInfos> operationalLimitsGroups1 = ModificationUtils.getOperationalLimitsGroupsOnSide(operationalLimitsGroups, SIDE1);
        List<OperationalLimitsGroupInfos> operationalLimitsGroups2 = ModificationUtils.getOperationalLimitsGroupsOnSide(operationalLimitsGroups, SIDE2);

        List<ReportNode> limitSetsOnSideReportNodes = new ArrayList<>();
        if (!CollectionUtils.isEmpty(operationalLimitsGroups)) {
            limitsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.limitsCreated").add();
            ReportNode reportNode = limitsReporter.newReportNode()
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .withMessageTemplate("network.modification.LimitSets")
                    .add();

            for (OperationalLimitsGroupInfos olgInfos : operationalLimitsGroups) {
                ReportNode limitSetNode = reportNode.newReportNode()
                        .withMessageTemplate("network.modification.limitSetAdded")
                        .withUntypedValue("name", olgInfos.getId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();

                if (olgInfos.getApplicability() == OperationalLimitsGroupInfos.Applicability.SIDE1 || olgInfos.getApplicability() == OperationalLimitsGroupInfos.Applicability.EQUIPMENT) {
                    ModificationUtils.getInstance().setCurrentLimitsOnASide(limitSetNode, olgInfos, twoWindingsTransformer, ONE);
                }
                if (olgInfos.getApplicability() == OperationalLimitsGroupInfos.Applicability.SIDE2 || olgInfos.getApplicability() == OperationalLimitsGroupInfos.Applicability.EQUIPMENT) {
                    ModificationUtils.getInstance().setCurrentLimitsOnASide(limitSetNode, olgInfos, twoWindingsTransformer, TWO);
                }
            }
        }

        if (selectedOperationalLimitsGroupId1 != null) {
            if (!ModificationUtils.hasLimitSet(operationalLimitsGroups1, selectedOperationalLimitsGroupId1)) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetAbsentOnSide1")
                        .withUntypedValue("selectedOperationalLimitsGroup", selectedOperationalLimitsGroupId1)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else {
                twoWindingsTransformer.setSelectedOperationalLimitsGroup1(selectedOperationalLimitsGroupId1);
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetSelectedOnSide1")
                        .withUntypedValue("selectedOperationalLimitsGroup1", selectedOperationalLimitsGroupId1)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        }
        if (selectedOperationalLimitsGroupId2 != null) {
            if (!ModificationUtils.hasLimitSet(operationalLimitsGroups2, selectedOperationalLimitsGroupId2)) {
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetAbsentOnSide2")
                        .withUntypedValue("selectedOperationalLimitsGroup", selectedOperationalLimitsGroupId2)
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else {
                twoWindingsTransformer.setSelectedOperationalLimitsGroup2(selectedOperationalLimitsGroupId2);
                limitSetsOnSideReportNodes.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("network.modification.limitSetSelectedOnSide2")
                        .withUntypedValue("selectedOperationalLimitsGroup2", selectedOperationalLimitsGroupId2)
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
        addTapChangersToTwoWindingsTransformer(network, twoWindingsTransformer, characteristicsReporter);
    }

}
