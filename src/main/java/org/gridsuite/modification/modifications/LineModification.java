/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.AttributeModification;
import org.gridsuite.modification.model.BranchModificationModel;
import org.gridsuite.modification.model.LineModificationModel;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_LINE_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class LineModification extends AbstractBranchModification {

    private static final String MAGNETIZING_SUSCEPTANCE_MESSAGE = "Magnetizing susceptance";
    private static final String MAGNETIZING_CONDUCTANCE_MESSAGE = "Magnetizing conductance";
    public static final String ERROR_MESSAGE = "Line '%s' : ";

    public LineModification(LineModificationModel modificationModel) {
        super(modificationModel);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Line line = network.getLine(modificationModel.getEquipmentId());
        String errorMessage = "Line '" + modificationModel.getEquipmentId() + "' : ";
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, errorMessage + "does not exist in network");
        }
        ModificationUtils.getInstance().checkVoltageLevelModification(network, modificationModel.getVoltageLevelId1(),
                modificationModel.getBusOrBusbarSectionId1(), line.getTerminal1());
        ModificationUtils.getInstance().checkVoltageLevelModification(network, modificationModel.getVoltageLevelId2(),
                modificationModel.getBusOrBusbarSectionId2(), line.getTerminal2());
        LineModificationModel lineModificationModel = (LineModificationModel) modificationModel;
        if (lineModificationModel.getR() != null) {
            checkIsNotNegativeValue(errorMessage, lineModificationModel.getR().getValue(), MODIFY_LINE_ERROR, "Resistance R");
        }
        if (lineModificationModel.getG1() != null) {
            checkIsNotNegativeValue(errorMessage, lineModificationModel.getG1().getValue(), MODIFY_LINE_ERROR, "Conductance on side 1 G1");
        }
        if (lineModificationModel.getG2() != null) {
            checkIsNotNegativeValue(errorMessage, lineModificationModel.getG2().getValue(), MODIFY_LINE_ERROR, "Conductance on side 2 G2");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Line line = network.getLine(modificationModel.getEquipmentId());
        // modify the line in the network
        modifyLine(line, modificationModel, subReportNode);
    }

    @Override
    public String getName() {
        return "LineModification";
    }

    private void modifyLine(Line line, BranchModificationModel lineModificationModel, ReportNode subReportNode) {
        modifyBranch(line, lineModificationModel, subReportNode, "network.modification.lineModification");
        updateMeasurements(line, lineModificationModel, subReportNode);
        PropertiesUtils.applyProperties(line, subReportNode, modificationModel.getProperties(), "network.modification.LineProperties");
    }

    @Override
    protected void modifyCharacteristics(Branch<?> branch, BranchModificationModel branchModificationModel, ReportNode subReportNode) {
        Line line = (Line) branch;
        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.characteristics").add();
        modifyR(line, branchModificationModel.getR(), characteristicsReporter);
        modifyX(line, branchModificationModel.getX(), characteristicsReporter);

        LineModificationModel lineModificationModel = (LineModificationModel) branchModificationModel;
        modifySide1Characteristics(line, lineModificationModel, characteristicsReporter);
        modifySide2Characteristics(line, lineModificationModel, characteristicsReporter);

    }

    private void modifySide1Characteristics(Line line, LineModificationModel lineModificationModel,
                                            ReportNode characteristicsReportNode) {
        if (lineModificationModel.getG1() != null && lineModificationModel.getG1().getValue() != null
            || lineModificationModel.getB1() != null && lineModificationModel.getB1().getValue() != null) {
            ReportNode side1ReportNode = characteristicsReportNode.newReportNode().withMessageTemplate("network.modification.side1Characteristics").add();
            modifyG1(line, lineModificationModel.getG1(), side1ReportNode);
            modifyB1(line, lineModificationModel.getB1(), side1ReportNode);
        }
    }

    private void modifySide2Characteristics(Line line, LineModificationModel lineModificationModel,
                                            ReportNode characteristicsReportNode) {
        if (lineModificationModel.getG2() != null && lineModificationModel.getG2().getValue() != null
            || lineModificationModel.getB2() != null && lineModificationModel.getB2().getValue() != null) {
            ReportNode side2Reporter = characteristicsReportNode.newReportNode().withMessageTemplate("network.modification.side2Characteristics").add();
            modifyG2(line, lineModificationModel.getG2(), side2Reporter);
            modifyB2(line, lineModificationModel.getB2(), side2Reporter);
        }
    }

    @Override
    protected boolean characteristicsModified(BranchModificationModel branchModificationModel) {
        LineModificationModel lineModificationModel = (LineModificationModel) branchModificationModel;
        return super.characteristicsModified(branchModificationModel)
            || lineModificationModel.getG1() != null
            && lineModificationModel.getG1().getValue() != null
            || lineModificationModel.getB1() != null
            && lineModificationModel.getB1().getValue() != null
            || lineModificationModel.getG2() != null
            && lineModificationModel.getG2().getValue() != null
            || lineModificationModel.getB2() != null
            && lineModificationModel.getB2().getValue() != null;
    }

    public static void modifyX(Line line, AttributeModification<Double> modifX, ReportNode reportNode) {
        if (modifX != null && modifX.getValue() != null) {
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(line.getX(),
                    modifX.getValue(), "Series reactance"));
            }
            line.setX(modifX.getValue());
        }
    }

    public static void modifyR(Line line, AttributeModification<Double> modifR, ReportNode reportNode) {
        if (modifR != null && modifR.getValue() != null) {
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(line.getR(),
                    modifR.getValue(), "Series resistance"));
            }
            line.setR(modifR.getValue());
        }
    }

    public static void modifyG1(Line line, AttributeModification<Double> modifG1, ReportNode reportNode) {
        if (modifG1 != null && modifG1.getValue() != null) {
            if (reportNode != null) {
                ReportNode gReportNode = ModificationUtils.getInstance().buildModificationReport(line.getG1(),
                    modifG1.getValue(),
                    MAGNETIZING_CONDUCTANCE_MESSAGE);
                insertReportNode(reportNode, gReportNode);
            }
            line.setG1(modifG1.getValue());
        }
    }

    public static void modifyG2(Line line, AttributeModification<Double> modifG2, ReportNode reportNode) {
        if (modifG2 != null && modifG2.getValue() != null) {
            if (reportNode != null) {
                ReportNode gReportNode = ModificationUtils.getInstance().buildModificationReport(line.getG2(),
                    modifG2.getValue(),
                    MAGNETIZING_CONDUCTANCE_MESSAGE);
                insertReportNode(reportNode, gReportNode);
            }
            line.setG2(modifG2.getValue());
        }
    }

    public static void modifyB1(Line line, AttributeModification<Double> modifB1, ReportNode reportNode) {
        if (modifB1 != null && modifB1.getValue() != null) {
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(line.getB1(),
                    modifB1.getValue(), MAGNETIZING_SUSCEPTANCE_MESSAGE));
            }
            line.setB1(modifB1.getValue());
        }
    }

    public static void modifyB2(Line line, AttributeModification<Double> modifB2, ReportNode reportNode) {
        if (modifB2 != null && modifB2.getValue() != null) {
            if (reportNode != null) {
                insertReportNode(reportNode, ModificationUtils.getInstance().buildModificationReport(line.getB2(),
                    modifB2.getValue(), MAGNETIZING_SUSCEPTANCE_MESSAGE));
            }
            line.setB2(modifB2.getValue());
        }
    }

}
