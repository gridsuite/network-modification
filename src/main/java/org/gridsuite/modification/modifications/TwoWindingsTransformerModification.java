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
import com.powsybl.iidm.network.extensions.TwoWindingsTransformerToBeEstimated;
import com.powsybl.iidm.network.extensions.TwoWindingsTransformerToBeEstimatedAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public class TwoWindingsTransformerModification extends AbstractBranchModification {

    public static final String MAGNETIZING_CONDUCTANCE_FIELD_NAME = "Magnetizing conductance";
    private static final String TARGET_DEADBAND = "Target deadband";

    public TwoWindingsTransformerModification(TwoWindingsTransformerModificationInfos modificationInfos) {
        super(modificationInfos);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        String errorMessage = "Two windings transformer '" + modificationInfos.getEquipmentId() + "' : ";
        TwoWindingsTransformer transformer = network.getTwoWindingsTransformer(modificationInfos.getEquipmentId());
        if (transformer == null) {
            throw new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_NOT_FOUND, errorMessage + "it does not exist in the network");
        }
        ModificationUtils.getInstance().checkVoltageLevelModification(network, modificationInfos.getVoltageLevelId1(),
                modificationInfos.getBusOrBusbarSectionId1(), transformer.getTerminal1());
        ModificationUtils.getInstance().checkVoltageLevelModification(network, modificationInfos.getVoltageLevelId2(),
                modificationInfos.getBusOrBusbarSectionId2(), transformer.getTerminal2());
        TwoWindingsTransformerModificationInfos twtModificationInfos = (TwoWindingsTransformerModificationInfos) modificationInfos;
        checkAndModifyTapChanger(network, twtModificationInfos.getRatioTapChanger(), transformer.getRatioTapChanger(), errorMessage);
        checkAndModifyTapChanger(network, twtModificationInfos.getPhaseTapChanger(), transformer.getPhaseTapChanger(), errorMessage);
        if (twtModificationInfos.getR() != null) {
            checkIsNotNegativeValue(errorMessage, twtModificationInfos.getR().getValue(), MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, "Resistance R");
        }
        if (twtModificationInfos.getG() != null) {
            checkIsNotNegativeValue(errorMessage, twtModificationInfos.getG().getValue(), MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, "Conductance G");
        }
        if (twtModificationInfos.getRatedU1() != null) {
            checkIsNotNegativeValue(errorMessage, twtModificationInfos.getRatedU1().getValue(), MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated Voltage on side 1");
        }
        if (twtModificationInfos.getRatedU2() != null) {
            checkIsNotNegativeValue(errorMessage, twtModificationInfos.getRatedU2().getValue(), MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated Voltage on side 2");
        }
        if (twtModificationInfos.getRatedS() != null) {
            checkIsNotNegativeValue(errorMessage, twtModificationInfos.getRatedS().getValue(), MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, "Rated nominal power");
        }
        if (twtModificationInfos.getRatioTapChanger() != null && twtModificationInfos.getRatioTapChanger().getTargetV() != null) {
            checkIsNotNegativeValue(errorMessage, twtModificationInfos.getRatioTapChanger().getTargetV().getValue(),
                MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, "Target voltage for ratio tap changer");
        }
    }

    private void checkAndModifyTapChanger(Network network, TapChangerModificationInfos tapChangerModificationInfos, TapChanger tapChanger, String errorMessage) {
        if (tapChanger != null && tapChangerModificationInfos != null) {
            checkTapChangerModification(network, tapChangerModificationInfos, tapChanger, errorMessage);
        }
    }

    private void checkTapChangerModification(Network network, TapChangerModificationInfos tapChangerModificationInfos, TapChanger tapChanger, String errorMessage) {
        ModificationUtils.getInstance().checkEnableRegulation(tapChangerModificationInfos.getRegulationType(),
            tapChangerModificationInfos.getRegulatingTerminalId(),
            tapChangerModificationInfos.getRegulatingTerminalType(),
            tapChangerModificationInfos.getRegulatingTerminalVlId(),
            null,
            tapChanger.getRegulationTerminal(),
            network,
            MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, errorMessage);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        TwoWindingsTransformer twoWindingsTransformer = network.getTwoWindingsTransformer(modificationInfos.getEquipmentId());
        // modify the 2wt in the network
        modifyTwoWindingsTransformer(twoWindingsTransformer, modificationInfos, subReportNode, network);
    }

    @Override
    public String getName() {
        return "TwoWindingsTransformerModification";
    }

    private void modifyTwoWindingsTransformer(TwoWindingsTransformer twoWindingsTransformer, BranchModificationInfos twoWindingsTransformerModificationInfos, ReportNode subReportNode, Network network) {
        modifyBranch(twoWindingsTransformer, twoWindingsTransformerModificationInfos, subReportNode, "network.modification.twoWindingsTransformerModification.modified");
        updateStateEstimationData((TwoWindingsTransformerModificationInfos) twoWindingsTransformerModificationInfos, twoWindingsTransformer, subReportNode);
        addTapChangersToTwoWindingsTransformer(network, (TwoWindingsTransformerModificationInfos) twoWindingsTransformerModificationInfos, twoWindingsTransformer, subReportNode);
        PropertiesUtils.applyProperties(twoWindingsTransformer, subReportNode, twoWindingsTransformerModificationInfos.getProperties(), "network.modification.TwoWindingsTransformerProperties");
    }

    @Override
    protected void modifyCharacteristics(Branch<?> branch, BranchModificationInfos branchModificationInfos, ReportNode subReportNode) {
        TwoWindingsTransformer twoWindingsTransformer = (TwoWindingsTransformer) branch;
        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.characteristics").add();

        // Branch specific fields
        modifyR(twoWindingsTransformer, branchModificationInfos.getR(), characteristicsReporter);
        modifyX(twoWindingsTransformer, branchModificationInfos.getX(), characteristicsReporter);

        // Transformer specific fields
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) branchModificationInfos;
        modifyTransformerFields(twoWindingsTransformer,
                twoWindingsTransformerModificationInfos.getG(),
                twoWindingsTransformerModificationInfos.getB(),
                twoWindingsTransformerModificationInfos.getRatedS(),
                twoWindingsTransformerModificationInfos.getRatedU1(),
                twoWindingsTransformerModificationInfos.getRatedU2(),
                characteristicsReporter);
    }

    public static void modifyTransformerFields(TwoWindingsTransformer transformer,
                                               AttributeModification<Double> modifG,
                                               AttributeModification<Double> modifB,
                                               AttributeModification<Double> modifRatedS,
                                               AttributeModification<Double> modifRatedU1,
                                               AttributeModification<Double> modifRatedU2,
                                               ReportNode reportNode) {
        modifyG(transformer, modifG, reportNode);
        modifyB(transformer, modifB, reportNode);
        modifyRatedS(transformer, modifRatedS, reportNode);
        modifyRatedU1(transformer, modifRatedU1, reportNode);
        modifyRatedU2(transformer, modifRatedU2, reportNode);
    }

    public static void modifyRatedU2(TwoWindingsTransformer transformer, AttributeModification<Double> modifRatedU2, ReportNode reportNode) {
        if (modifRatedU2 != null && modifRatedU2.getValue() != null) {
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(transformer.getRatedU2(),
                        modifRatedU2.getValue(), "Rated Voltage (Side 2)"));
            }
            transformer.setRatedU2(modifRatedU2.getValue());
        }
    }

    public static void modifyRatedU1(TwoWindingsTransformer transformer, AttributeModification<Double> modifRatedU1, ReportNode reportNode) {
        if (modifRatedU1 != null && modifRatedU1.getValue() != null) {
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(transformer.getRatedU1(),
                        modifRatedU1.getValue(), "Rated Voltage (Side 1)"));
            }
            transformer.setRatedU1(modifRatedU1.getValue());
        }
    }

    public static void modifyRatedS(TwoWindingsTransformer transformer, AttributeModification<Double> modifRatedS, ReportNode reportNode) {
        if (modifRatedS != null && modifRatedS.getValue() != null) {
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(transformer.getRatedS(),
                                modifRatedS.getValue(), "Rated nominal power"));
            }
            transformer.setRatedS(modifRatedS.getValue());
        }
    }

    public static void modifyB(TwoWindingsTransformer transformer, AttributeModification<Double> modifB, ReportNode reportNode) {
        if (modifB != null && modifB.getValue() != null) {
            // convert reported value from siemens to microsiemens
            if (reportNode != null) {
                double oldMagnetizingSusceptanceToReport = transformer.getB() * Math.pow(10, 6);
                double newMagnetizingSusceptanceToReport = modifB.getValue() * Math.pow(10, 6);
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(oldMagnetizingSusceptanceToReport,
                                newMagnetizingSusceptanceToReport, "Magnetizing susceptance"));
            }
            transformer.setB(modifB.getValue());
        }
    }

    public static void modifyG(TwoWindingsTransformer transformer, AttributeModification<Double> modifG, ReportNode reportNode) {
        if (modifG != null && modifG.getValue() != null) {
            // convert reported value from siemens to microsiemens
            if (reportNode != null) {
                double oldMagnetizingConductanceToReport = transformer.getG() * Math.pow(10, 6);
                double newMagnetizingConductanceToReport = modifG.getValue() * Math.pow(10, 6);
                ReportNode gReportNode = ModificationUtils.getInstance().buildModificationReport(
                        oldMagnetizingConductanceToReport,
                        newMagnetizingConductanceToReport,
                        MAGNETIZING_CONDUCTANCE_FIELD_NAME);
                insertReportNode(reportNode, gReportNode);
            }
            transformer.setG(modifG.getValue());
        }
    }

    public static void modifyX(TwoWindingsTransformer twt, AttributeModification<Double> modifX, ReportNode reportNode) {
        if (modifX != null && modifX.getValue() != null) {
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(twt.getX(),
                        modifX.getValue(), "Series reactance"));
            }
            twt.setX(modifX.getValue());
        }
    }

    public static void modifyR(TwoWindingsTransformer twt, AttributeModification<Double> modifR, ReportNode reportNode) {
        if (modifR != null && modifR.getValue() != null) {
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(twt.getR(),
                        modifR.getValue(), "Series resistance"));
            }
            twt.setR(modifR.getValue());
        }
    }

    private void updateStateEstimationData(TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt, ReportNode subReportNode) {
        // Common part for Branch measurements
        ReportNode estimSubReportNode = updateMeasurements(twt, twoWindingsTransformerModificationInfos, subReportNode);

        // toBeEstimated part specific to 2WT
        Boolean rtcToBeEstim = twoWindingsTransformerModificationInfos.getRatioTapChangerToBeEstimated() != null ? twoWindingsTransformerModificationInfos.getRatioTapChangerToBeEstimated().getValue() : null;
        Boolean ptcToBeEstim = twoWindingsTransformerModificationInfos.getPhaseTapChangerToBeEstimated() != null ? twoWindingsTransformerModificationInfos.getPhaseTapChangerToBeEstimated().getValue() : null;
        if (rtcToBeEstim == null && ptcToBeEstim == null) {
            return;
        }
        if (estimSubReportNode == null) {
            estimSubReportNode = subReportNode.newReportNode().withMessageTemplate("network.modification.StateEstimationData").add();
        }
        TwoWindingsTransformerToBeEstimated toBeEstimated = twt.getExtension(TwoWindingsTransformerToBeEstimated.class);
        if (toBeEstimated == null) {
            TwoWindingsTransformerToBeEstimatedAdder toBeEstimatedAdder = twt.newExtension(TwoWindingsTransformerToBeEstimatedAdder.class);
            toBeEstimated = toBeEstimatedAdder.add();
        }
        List<ReportNode> reports = new ArrayList<>();
        if (rtcToBeEstim != null) {
            boolean oldValue = toBeEstimated.shouldEstimateRatioTapChanger();
            toBeEstimated.shouldEstimateRatioTapChanger(rtcToBeEstim);
            reports.add(ModificationUtils.buildModificationReport(oldValue, rtcToBeEstim, "Ratio tap changer to be estimated", TypedValue.INFO_SEVERITY));
        }
        if (ptcToBeEstim != null) {
            boolean oldValue = toBeEstimated.shouldEstimatePhaseTapChanger();
            toBeEstimated.shouldEstimatePhaseTapChanger(ptcToBeEstim);
            reports.add(ModificationUtils.buildModificationReport(oldValue, ptcToBeEstim, "Phase tap changer to be estimated", TypedValue.INFO_SEVERITY));
        }
        ModificationUtils.getInstance().reportModifications(estimSubReportNode, reports, "network.modification.twtToBeEstimated");
    }

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt, ReportNode subReportNode) {
        if (twt.hasRatioTapChanger() && twoWindingsTransformerModificationInfos.getRatioTapChanger().getEnabled() != null && Boolean.FALSE.equals(twoWindingsTransformerModificationInfos.getRatioTapChanger().getEnabled().getValue())) {
            twt.getRatioTapChanger().remove();
            subReportNode.newReportNode()
                .withMessageTemplate("network.modification.RatioTapChangerRemoved")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        } else if (ratioTapChangerModified(twoWindingsTransformerModificationInfos.getRatioTapChanger())) {
            processRatioTapChanger(network, twoWindingsTransformerModificationInfos, twt, subReportNode, twt.hasRatioTapChanger());
        }

        if (twt.hasPhaseTapChanger() && twoWindingsTransformerModificationInfos.getPhaseTapChanger().getEnabled() != null && Boolean.FALSE.equals(twoWindingsTransformerModificationInfos.getPhaseTapChanger().getEnabled().getValue())) {
            twt.getPhaseTapChanger().remove();
            subReportNode.newReportNode()
                .withMessageTemplate("network.modification.PhaseTapChangerRemoved")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        } else if (phaseTapChangerModified(twoWindingsTransformerModificationInfos.getPhaseTapChanger())) {
            processPhaseTapChanger(network, twoWindingsTransformerModificationInfos, twt, subReportNode, twt.hasPhaseTapChanger());
        }
    }

    private void processPhaseTapChanger(Network network,
            TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos,
            TwoWindingsTransformer twt,
            ReportNode subReportNode,
            boolean isModification) {
        PhaseTapChanger phaseTapChanger = isModification ? twt.getPhaseTapChanger() : null;
        PhaseTapChangerAdder phaseTapChangerAdder = isModification ? null : twt.newPhaseTapChanger();
        PhaseTapChangerModificationInfos phaseTapChangerInfos = twoWindingsTransformerModificationInfos
                .getPhaseTapChanger();
        List<ReportNode> regulationReports = new ArrayList<>();
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();

        processPhaseTapRegulation(phaseTapChanger, phaseTapChangerAdder, isModification, phaseTapChangerInfos.getRegulationMode(),
            phaseTapChangerInfos.getRegulationValue(), phaseTapChangerInfos.getTargetDeadband(), phaseTapChangerInfos.getRegulating(), regulationReports);

        processRegulatingTerminal(phaseTapChangerInfos, phaseTapChanger, phaseTapChangerAdder, regulatedTerminalReports,
                network,
                twt, isModification);

        List<ReportNode> positionsAndStepsReports = new ArrayList<>();
        processTapChangerPositionsAndSteps(phaseTapChanger, phaseTapChangerAdder, isModification, phaseTapChangerInfos.getLowTapPosition(), phaseTapChangerInfos.getTapPosition(), phaseTapChangerInfos.getSteps(), positionsAndStepsReports);

        if (!isModification) {
            phaseTapChangerAdder.add();
        }

        if (!regulationReports.isEmpty() || !positionsAndStepsReports.isEmpty()) {
            ReportNode phaseTapChangerSubreporter = ModificationUtils.getInstance().reportModifications(subReportNode,
                regulationReports, "network.modification.PHASE");
            if (phaseTapChangerSubreporter == null) {
                phaseTapChangerSubreporter = subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.PHASE")
                        .add();
            }
            ModificationUtils.getInstance().reportModifications(phaseTapChangerSubreporter, positionsAndStepsReports,
                    "network.modification.phaseTapChangerPositionsAndStepsModification");
        }
    }

    public static void processPhaseTapRegulation(PhaseTapChanger phaseTapChanger,
                                                 PhaseTapChangerAdder phaseTapChangerAdder,
                                                 boolean isModification,
                                                 AttributeModification<PhaseTapChanger.RegulationMode> regulationModeModification,
                                                 AttributeModification<Double> regulationValueModification,
                                                 AttributeModification<Double> targetDeadbandModification,
                                                 AttributeModification<Boolean> regulatingModification,
                                                 List<ReportNode> regulationReports) throws NetworkModificationException {

        // checks will be done in powsybl
        AttributeModification<Double> finalTargetDeadbandModification = targetDeadbandModification;
        if (regulatingModification != null && regulatingModification.getValue()) {
            if (!isModification) {
                // creation
                if (targetDeadbandModification == null) {
                    finalTargetDeadbandModification = new AttributeModification<>(0.0, OperationType.SET);
                }
                if (regulationValueModification == null) {
                    throw new NetworkModificationException(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Regulation value is missing when creating tap phase changer with regulation enabled");
                }
                if (regulationModeModification == null) {
                    throw new NetworkModificationException(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Regulation mode is missing when creating tap phase changer with regulation enabled");
                }

            } else {
                if (targetDeadbandModification == null && Double.isNaN(phaseTapChanger.getTargetDeadband())) {
                    finalTargetDeadbandModification = new AttributeModification<>(0.0, OperationType.SET);
                }

                if (regulationValueModification == null && Double.isNaN(phaseTapChanger.getRegulationValue())) {
                    throw new NetworkModificationException(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, "Regulation value is missing when modifying, phase tap changer can not regulate");
                }
                if (regulationModeModification == null && phaseTapChanger.getRegulationMode() == null) {
                    throw new NetworkModificationException(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, "Regulation mode is missing when modifying, phase tap changer can not regulate");
                }
            }
        }
        setPhaseTapChangerRegulationAttributes(phaseTapChanger, phaseTapChangerAdder, isModification,
            regulationModeModification, regulationValueModification, finalTargetDeadbandModification, regulatingModification, regulationReports);

    }

    private static void setPhaseTapChangerRegulationAttributes(PhaseTapChanger phaseTapChanger,
                                                        PhaseTapChangerAdder phaseTapChangerAdder,
                                                        boolean isModification,
                                                        AttributeModification<PhaseTapChanger.RegulationMode> regulationModeModification,
                                                        AttributeModification<Double> regulationValueModification,
                                                        AttributeModification<Double> targetDeadbandModification,
                                                        AttributeModification<Boolean> regulatingModification,
                                                        List<ReportNode> regulationReports) {
        // the order is important if regulation mode is set and regulation value or target dead band is null it will crash
        PhaseTapChanger.RegulationMode regulationMode = regulationModeModification == null ? null : regulationModeModification.getValue();
        String fieldName = (regulationMode == PhaseTapChanger.RegulationMode.CURRENT_LIMITER) ? "Value" : "Flow set point";
        // Regulation value
        ReportNode regulationValueReportNode = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
            isModification ? phaseTapChanger::setRegulationValue : phaseTapChangerAdder::setRegulationValue,
            isModification ? phaseTapChanger::getRegulationValue : () -> null,
            regulationValueModification, fieldName);
        if (regulationReports != null && regulationValueReportNode != null) {
            regulationReports.add(regulationValueReportNode);
        }
        // targetDeadBand
        ReportNode targetDeadbandReportNode = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
            isModification ? phaseTapChanger::setTargetDeadband : phaseTapChangerAdder::setTargetDeadband,
            isModification ? phaseTapChanger::getTargetDeadband : () -> null,
            targetDeadbandModification, TARGET_DEADBAND);
        if (regulationReports != null && targetDeadbandReportNode != null) {
            regulationReports.add(targetDeadbandReportNode);
        }

        // RegulationMode
        ReportNode regulationReportNode = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
            isModification ? phaseTapChanger::setRegulationMode : phaseTapChangerAdder::setRegulationMode,
            isModification ? phaseTapChanger::getRegulationMode : () -> null,
            regulationModeModification, "Regulation mode");
        if (regulationReports != null && regulationReportNode != null) {
            regulationReports.add(regulationReportNode);
        }

        // Regulating
        ReportNode regulatingReportNode = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
            isModification ? phaseTapChanger::setRegulating : phaseTapChangerAdder::setRegulating,
            isModification ? phaseTapChanger::isRegulating : () -> null,
            regulatingModification, "Phase tap regulating");
        if (regulationReports != null && regulatingReportNode != null) {
            regulationReports.add(regulatingReportNode);
        }
    }

    private void processRatioTapChanger(Network network,
            TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos,
            TwoWindingsTransformer twt,
            ReportNode subReporter,
            boolean isModification) {
        RatioTapChangerModificationInfos ratioTapChangerInfos = twoWindingsTransformerModificationInfos
                .getRatioTapChanger();
        RatioTapChanger ratioTapChanger = isModification ? twt.getRatioTapChanger() : null;
        RatioTapChangerAdder ratioTapChangerAdder = isModification ? null : twt.newRatioTapChanger();
        List<ReportNode> ratioTapChangerReports = new ArrayList<>();
        ReportNode tapChangingReport = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? ratioTapChanger::setLoadTapChangingCapabilities
                        : ratioTapChangerAdder::setLoadTapChangingCapabilities,
                isModification ? ratioTapChanger::hasLoadTapChangingCapabilities : () -> null,
                ratioTapChangerInfos.getLoadTapChangingCapabilities(), "Load tap changing capabilities");
        if (tapChangingReport != null) {
            ratioTapChangerReports.add(tapChangingReport);
        }

        List<ReportNode> regulationReports = new ArrayList<>();
        List<ReportNode> regulatedTerminalReports = new ArrayList<>();
        processRatioVoltageRegulation(ratioTapChangerInfos, twt, ratioTapChanger, ratioTapChangerAdder, regulationReports, regulatedTerminalReports, network,
                isModification);
        // regulating must be set after target value, regulating mode and regulating terminal are set
        processRegulating(ratioTapChangerInfos, ratioTapChanger, ratioTapChangerAdder, regulationReports, isModification);

        List<ReportNode> positionsAndStepsReports = new ArrayList<>();
        processTapChangerPositionsAndSteps(ratioTapChanger, ratioTapChangerAdder, isModification, ratioTapChangerInfos.getLowTapPosition(), ratioTapChangerInfos.getTapPosition(), ratioTapChangerInfos.getSteps(), positionsAndStepsReports
        );

        if (!isModification) {
            ratioTapChangerAdder.add();
        }

        if (!ratioTapChangerReports.isEmpty() || !regulationReports.isEmpty() || !regulatedTerminalReports.isEmpty()
                || !positionsAndStepsReports.isEmpty()) {
            ReportNode ratioTapChangerReporter = ModificationUtils.getInstance().reportModifications(subReporter,
                    ratioTapChangerReports, "network.modification.RATIO");
            if (ratioTapChangerReporter == null) {
                ratioTapChangerReporter = subReporter.newReportNode()
                        .withMessageTemplate("network.modification.RATIO")
                        .add();
            }
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, regulationReports,
                    "network.modification.ratioTapChangerRegulationModification");
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, regulatedTerminalReports,
                    "network.modification.ratioTapChangerTerminalRegulatedModification");
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, positionsAndStepsReports,
                    "network.modification.ratioTapChangerPositionsAndStepsModification");
        }
    }

    private void processRegulating(RatioTapChangerModificationInfos ratioTapChangerInfos,
            RatioTapChanger ratioTapChanger, RatioTapChangerAdder ratioTapChangerAdder,
            List<ReportNode> regulationReports, boolean isModification) {
        // if regulating and targetDeadband is null then it is set by default to 0
        Double targetDeadBandInfo = ratioTapChangerInfos.getTargetDeadband() != null ? ratioTapChangerInfos.getTargetDeadband().getValue() : null;
        boolean targetDeadBandIsNull = isModification && Double.isNaN(ratioTapChanger.getTargetDeadband()) && targetDeadBandInfo == null ||
            !isModification && targetDeadBandInfo == null;

        if (targetDeadBandIsNull) {
            ReportNode targetDeadbandReportNode = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? ratioTapChanger::setTargetDeadband
                    : ratioTapChangerAdder::setTargetDeadband,
                isModification ? ratioTapChanger::getTargetDeadband : () -> null,
                AttributeModification.toAttributeModification(0d, OperationType.SET), TARGET_DEADBAND);
            regulationReports.add(targetDeadbandReportNode);
        }
        Boolean isRegulating = ratioTapChangerInfos.getRegulating() != null ? ratioTapChangerInfos.getRegulating().getValue() : null;
        // need to set regulation mode before setting regulating
        if (isRegulating != null) {
            if (isRegulating && ratioTapChanger.getRegulationMode() != RatioTapChanger.RegulationMode.VOLTAGE) {
                RatioTapChanger.RegulationMode regulationMode = RatioTapChanger.RegulationMode.VOLTAGE;
                AttributeModification<RatioTapChanger.RegulationMode> regulationModeModification = AttributeModification.toAttributeModification(regulationMode, OperationType.SET);
                ReportNode regulationModeReport = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                        isModification ? ratioTapChanger::setRegulationMode : ratioTapChangerAdder::setRegulationMode,
                        isModification ? ratioTapChanger::getRegulationMode : () -> null, regulationModeModification, "Regulation mode : " + regulationMode);
                if (regulationModeReport != null) {
                    regulationReports.add(regulationModeReport);
                }
            }
            ReportNode voltageRegulationReport = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? ratioTapChanger::setRegulating
                    : ratioTapChangerAdder::setRegulating,
                isModification ? ratioTapChanger::isRegulating : () -> null,
                ratioTapChangerInfos.getRegulating(), isRegulating ? "Regulation" : "Fixed ratio");
            if (voltageRegulationReport != null) {
                regulationReports.add(voltageRegulationReport);
            }
        }
    }

    private void processRatioVoltageRegulation(RatioTapChangerModificationInfos ratioTapChangerInfos,
            TwoWindingsTransformer twt,
            RatioTapChanger ratioTapChanger,
            RatioTapChangerAdder ratioTapChangerAdder,
            List<ReportNode> regulationReports,
            List<ReportNode> regulatedTerminalReports,
            Network network,
            boolean isModification) {
        modifyTargets(ratioTapChanger, ratioTapChangerAdder, isModification, ratioTapChangerInfos.getTargetV(), ratioTapChangerInfos.getTargetDeadband(), regulationReports);

        processRegulatingTerminal(ratioTapChangerInfos, ratioTapChanger, ratioTapChangerAdder, regulatedTerminalReports,
                    network, twt, isModification);
    }

    public static void modifyTargets(RatioTapChanger ratioTapChanger, RatioTapChangerAdder ratioTapChangerAdder, boolean isModification, AttributeModification<Double> targetV,
                                     AttributeModification<Double> targetDeadband, List<ReportNode> regulationReports) {
        ReportNode targetVoltageReportNode = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? ratioTapChanger::setTargetV
                        : ratioTapChangerAdder::setTargetV,
                isModification ? ratioTapChanger::getTargetV : () -> null,
                targetV, "Target voltage");

        ReportNode targetDeadbandReportNode = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? ratioTapChanger::setTargetDeadband
                        : ratioTapChangerAdder::setTargetDeadband,
                isModification ? ratioTapChanger::getTargetDeadband : () -> null,
                targetDeadband, TARGET_DEADBAND);

        if (regulationReports != null) {
            if (targetVoltageReportNode != null) {
                regulationReports.add(targetVoltageReportNode);
            }
            if (targetDeadbandReportNode != null) {
                regulationReports.add(targetDeadbandReportNode);
            }
        }
    }

    private void processRegulatingTerminal(TapChangerModificationInfos tapChangerModificationInfos,
            TapChanger<?, ?, ?, ?> tapChanger,
            TapChangerAdder<?, ?, ?, ?, ?, ?> tapChangerAdder,
            List<ReportNode> regulatedTerminalReports,
            Network network,
            TwoWindingsTransformer twt,
            boolean isModification) {
        String oldVoltageLevel = null;
        String oldEquipment = null;

        if (isModification && tapChanger.getRegulationTerminal() != null) {
            oldVoltageLevel = tapChanger.getRegulationTerminal().getVoltageLevel().getId();
            oldEquipment = tapChanger.getRegulationTerminal().getConnectable().getType()
                    .name() + ":"
                    + tapChanger.getRegulationTerminal().getConnectable().getId();
        }

        if (tapChangerModificationInfos.getRegulationSide() != null
                && tapChangerModificationInfos.getRegulationSide().getValue() != null) {
            Terminal terminal = tapChangerModificationInfos.getRegulationSide().getValue() == RegulationSide.SIDE1
                    ? twt.getTerminal1()
                    : twt.getTerminal2();
            setRegulatingTerminalInfos(tapChangerModificationInfos, terminal);
        }

        if (tapChangerModificationInfos.getRegulatingTerminalId() != null
                && tapChangerModificationInfos.getRegulatingTerminalType() != null
                && tapChangerModificationInfos.getRegulatingTerminalVlId() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
                    tapChangerModificationInfos.getRegulatingTerminalId().getValue(),
                    tapChangerModificationInfos.getRegulatingTerminalType().getValue(),
                    tapChangerModificationInfos.getRegulatingTerminalVlId().getValue());
            if (isModification) {
                tapChanger.setRegulationTerminal(terminal);
            } else {
                tapChangerAdder.setRegulationTerminal(terminal);
            }
            regulatedTerminalReports
                    .add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                            tapChangerModificationInfos.getRegulatingTerminalVlId().getValue(),
                            "Voltage level"));
            regulatedTerminalReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    tapChangerModificationInfos.getRegulatingTerminalType().getValue() + " : "
                            + tapChangerModificationInfos.getRegulatingTerminalId().getValue(),
                    "Equipment"));
        }
    }

    private void setRegulatingTerminalInfos(TapChangerModificationInfos tapChangerModificationInfos, Terminal terminal) {
        tapChangerModificationInfos.setRegulatingTerminalVlId(new AttributeModification<>(terminal.getVoltageLevel().getId(), OperationType.SET));
        tapChangerModificationInfos.setRegulatingTerminalId(new AttributeModification<>(terminal.getConnectable().getId(), OperationType.SET));
        tapChangerModificationInfos.setRegulatingTerminalType(new AttributeModification<>(terminal.getConnectable().getType().name(), OperationType.SET));
    }

    private static void processTapchangerSteps(List<ReportNode> tapChangerStepsReports,
                                        TapChangerAdder<?, ?, ?, ?, ?, ?> tapChangerAdder,
                                        TapChangerStepsReplacer<?, ?> tapChangerStepReplacer,
                                        boolean isModification,
                                        List<TapChangerStepCreationInfos> modifSteps) {
        if (tapChangerStepsReports != null) {
            tapChangerStepsReports.add(ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.tapChangerStepsModification")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        for (TapChangerStepCreationInfos step : modifSteps) {
            if (tapChangerStepsReports != null) {
                addStepAttributeReports(tapChangerStepsReports, step);
            }
            if (tapChangerStepReplacer instanceof RatioTapChangerStepsReplacer || tapChangerAdder instanceof RatioTapChangerAdder) {
                processRatioTapChangerStep(tapChangerAdder, tapChangerStepReplacer, isModification, step);
            } else {
                processPhaseTapChangerStep(tapChangerStepsReports, (PhaseTapChangerAdder) tapChangerAdder, (PhaseTapChangerStepsReplacer) tapChangerStepReplacer, isModification, step);
            }
        }
        if (isModification) {
            tapChangerStepReplacer.replaceSteps();
        }
    }

    private static void processPhaseTapChangerStep(List<ReportNode> tapChangerStepsReports, PhaseTapChangerAdder tapChangerAdder, PhaseTapChangerStepsReplacer tapChangerStepReplacer, boolean isModification, TapChangerStepCreationInfos step) {
        if (tapChangerStepsReports != null) {
            addStepAttributeReport(tapChangerStepsReports, "network.modification.newStepAlpha", String.valueOf(step.getAlpha()));
        }
        if (isModification) {
            tapChangerStepReplacer.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG())
                    .setB(step.getB()).setRho(step.getRho()).setAlpha(step.getAlpha()).endStep();
        } else {
            tapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG())
                    .setB(step.getB()).setRho(step.getRho()).setAlpha(step.getAlpha()).endStep();
        }
    }

    private static void processRatioTapChangerStep(TapChangerAdder<?, ?, ?, ?, ?, ?> tapChangerAdder, TapChangerStepsReplacer<?, ?> tapChangerStepReplacer, boolean isModification, TapChangerStepCreationInfos step) {
        if (isModification) {
            tapChangerStepReplacer.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG())
                    .setB(step.getB()).setRho(step.getRho()).endStep();
        } else {
            tapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG())
                    .setB(step.getB()).setRho(step.getRho()).endStep();
        }
    }

    private static void addStepAttributeReports(List<ReportNode> tapChangerStepsReports, TapChangerStepCreationInfos step) {
        addStepAttributeReport(tapChangerStepsReports, "network.modification.newStepIndex", String.valueOf(step.getIndex()));
        addStepAttributeReport(tapChangerStepsReports, "network.modification.newStepResistance", String.valueOf(step.getR()));
        addStepAttributeReport(tapChangerStepsReports, "network.modification.newStepReactance", String.valueOf(step.getX()));
        addStepAttributeReport(tapChangerStepsReports, "network.modification.newStepConductance", String.valueOf(step.getG()));
        addStepAttributeReport(tapChangerStepsReports, "network.modification.newStepSusceptance", String.valueOf(step.getB()));
        addStepAttributeReport(tapChangerStepsReports, "network.modification.newStepRatio", String.valueOf(step.getRho()));
    }

    public static void processTapChangerPositionsAndSteps(TapChanger<?, ?, ?, ?> tapChanger,
                                                   TapChangerAdder<?, ?, ?, ?, ?, ?> tapChangerAdder,
                                                   boolean isModification,
                                                   AttributeModification<Integer> modifyLowTapPosition,
                                                   AttributeModification<Integer> modifyTapPosition,
                                                   List<TapChangerStepCreationInfos> modifySteps,
                                                   List<ReportNode> tapChangerReports) {

        // Add steps (it can change the max position)
        if (modifySteps != null) {
            processTapchangerSteps(tapChangerReports,
                tapChangerAdder, isModification ? tapChanger.stepsReplacer() : null, isModification, modifySteps);
        }

        ReportNode lowTapPositionReportNode = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? tapChanger::setLowTapPosition
                        : tapChangerAdder::setLowTapPosition,
                isModification ? tapChanger::getLowTapPosition : () -> null,
                modifyLowTapPosition, "Low tap position");

        // must be done after setting the low position and the steps
        ReportNode tapPositionReportNode = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? tapChanger::setTapPosition
                        : tapChangerAdder::setTapPosition,
                isModification ? tapChanger::getTapPosition : () -> null,
                modifyTapPosition, "Tap position");

        if (tapChangerReports != null) {
            if (lowTapPositionReportNode != null) {
                tapChangerReports.add(lowTapPositionReportNode);
            }
            if (tapPositionReportNode != null) {
                tapChangerReports.add(tapPositionReportNode);
            }
        }
    }

    private static void addStepAttributeReport(List<ReportNode> tapChangerStepsReports, String key, String value) {
        tapChangerStepsReports.add(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate(key)
                .withUntypedValue("value", value)
                .withUntypedValue("delta", "Δ") // Workaround to use non-ISO-8859-1 characters in the internationalization file
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private boolean ratioTapChangerModified(RatioTapChangerModificationInfos ratioTapChangerModificationInfos) {
        return ratioTapChangerModificationInfos != null && (
                ratioTapChangerModificationInfos.getLoadTapChangingCapabilities() != null
                && ratioTapChangerModificationInfos.getLoadTapChangingCapabilities().getValue() != null
                || ratioTapChangerModificationInfos.getTargetV() != null
                && ratioTapChangerModificationInfos.getTargetV().getValue() != null
                || commonTapChangerAttributesModified(ratioTapChangerModificationInfos));
    }

    private boolean phaseTapChangerModified(PhaseTapChangerModificationInfos phaseTapChangerModificationInfos) {
        return phaseTapChangerModificationInfos != null && (
                phaseTapChangerModificationInfos.getRegulationMode() != null
                && phaseTapChangerModificationInfos.getRegulationMode().getValue() != null
                || phaseTapChangerModificationInfos.getRegulationValue() != null
                && phaseTapChangerModificationInfos.getRegulationValue().getValue() != null
                || commonTapChangerAttributesModified(phaseTapChangerModificationInfos));
    }

    private boolean commonTapChangerAttributesModified(TapChangerModificationInfos tapChangerModificationInfos) {
        return tapChangerModificationInfos != null && (
                tapChangerModificationInfos.getRegulating() != null
                && tapChangerModificationInfos.getRegulating().getValue() != null
                || tapChangerModificationInfos.getRegulationType() != null
                && tapChangerModificationInfos.getRegulationType().getValue() != null
                || tapChangerModificationInfos.getRegulationSide() != null
                && tapChangerModificationInfos.getRegulationSide().getValue() != null
                || tapChangerModificationInfos.getRegulatingTerminalId() != null
                && tapChangerModificationInfos.getRegulatingTerminalId().getValue() != null
                || tapChangerModificationInfos.getRegulatingTerminalType() != null
                && tapChangerModificationInfos.getRegulatingTerminalType().getValue() != null
                || tapChangerModificationInfos.getRegulatingTerminalVlId() != null
                && tapChangerModificationInfos.getRegulatingTerminalVlId().getValue() != null
                || tapChangerModificationInfos.getTargetDeadband() != null
                && tapChangerModificationInfos.getTargetDeadband().getValue() != null
                || positionsAndStepsModified(tapChangerModificationInfos));
    }

    private boolean positionsAndStepsModified(TapChangerModificationInfos tapChangerModificationInfos) {
        return tapChangerModificationInfos.getTapPosition() != null
            && tapChangerModificationInfos.getTapPosition().getValue() != null
            || tapChangerModificationInfos.getLowTapPosition() != null
            && tapChangerModificationInfos.getLowTapPosition().getValue() != null
            || tapChangerModificationInfos.getSteps() != null;
    }

    @Override
    protected boolean characteristicsModified(BranchModificationInfos branchModificationInfos) {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) branchModificationInfos;
        return super.characteristicsModified(branchModificationInfos)
            || twoWindingsTransformerModificationInfos.getG() != null
            && twoWindingsTransformerModificationInfos.getG().getValue() != null
            || twoWindingsTransformerModificationInfos.getB() != null
            && twoWindingsTransformerModificationInfos.getB().getValue() != null
            || twoWindingsTransformerModificationInfos.getRatedU1() != null
            && twoWindingsTransformerModificationInfos.getRatedU1().getValue() != null
            || twoWindingsTransformerModificationInfos.getRatedU2() != null
            && twoWindingsTransformerModificationInfos.getRatedU2().getValue() != null
            || twoWindingsTransformerModificationInfos.getRatedS() != null
            && twoWindingsTransformerModificationInfos.getRatedS().getValue() != null;
    }

}
