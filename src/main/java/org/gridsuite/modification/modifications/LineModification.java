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
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.BranchModificationInfos;
import org.gridsuite.modification.dto.LineModificationInfos;
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

    public LineModification(LineModificationInfos modificationInfos) {
        super(modificationInfos);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Line line = network.getLine(modificationInfos.getEquipmentId());
        String errorMessage = "Line '" + modificationInfos.getEquipmentId() + "' : ";
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, errorMessage + "does not exist in network");
        }
        LineModificationInfos lineModificationInfos = (LineModificationInfos) modificationInfos;
        if (lineModificationInfos.getR() != null) {
            checkIsNotNegativeValue(errorMessage, lineModificationInfos.getR().getValue(), MODIFY_LINE_ERROR, "Resistance R");
        }
        if (lineModificationInfos.getG1() != null) {
            checkIsNotNegativeValue(errorMessage, lineModificationInfos.getG1().getValue(), MODIFY_LINE_ERROR, "Conductance on side 1 G1");
        }
        if (lineModificationInfos.getG2() != null) {
            checkIsNotNegativeValue(errorMessage, lineModificationInfos.getG2().getValue(), MODIFY_LINE_ERROR, "Conductance on side 2 G2");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Line line = network.getLine(modificationInfos.getEquipmentId());
        // modify the line in the network
        modifyLine(line, modificationInfos, subReportNode);
    }

    @Override
    public String getName() {
        return "LineModification";
    }

    private void modifyLine(Line line, BranchModificationInfos lineModificationInfos, ReportNode subReportNode) {
        modifyBranch(line, lineModificationInfos, subReportNode, "network.modification.lineModification");
        updateMeasurements(line, lineModificationInfos, subReportNode);
        PropertiesUtils.applyProperties(line, subReportNode, modificationInfos.getProperties(), "network.modification.LineProperties");
    }

    @Override
    protected void modifyCharacteristics(Branch<?> branch, BranchModificationInfos branchModificationInfos, ReportNode subReportNode) {
        Line line = (Line) branch;
        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.characteristics").add();
        modifyR(line, branchModificationInfos.getR(), characteristicsReporter);
        modifyX(line, branchModificationInfos.getX(), characteristicsReporter);

        LineModificationInfos lineModificationInfos = (LineModificationInfos) branchModificationInfos;
        modifySide1Characteristics(line, lineModificationInfos, characteristicsReporter);
        modifySide2Characteristics(line, lineModificationInfos, characteristicsReporter);

    }

    private void modifySide1Characteristics(Line line, LineModificationInfos lineModificationInfos,
                                            ReportNode characteristicsReportNode) {
        if (lineModificationInfos.getG1() != null && lineModificationInfos.getG1().getValue() != null
            || lineModificationInfos.getB1() != null && lineModificationInfos.getB1().getValue() != null) {
            ReportNode side1ReportNode = characteristicsReportNode.newReportNode().withMessageTemplate("network.modification.side1Characteristics").add();
            modifyG1(line, lineModificationInfos.getG1(), side1ReportNode);
            modifyB1(line, lineModificationInfos.getB1(), side1ReportNode);
        }
    }

    private void modifySide2Characteristics(Line line, LineModificationInfos lineModificationInfos,
                                            ReportNode characteristicsReportNode) {
        if (lineModificationInfos.getG2() != null && lineModificationInfos.getG2().getValue() != null
            || lineModificationInfos.getB2() != null && lineModificationInfos.getB2().getValue() != null) {
            ReportNode side2Reporter = characteristicsReportNode.newReportNode().withMessageTemplate("network.modification.side2Characteristics").add();
            modifyG2(line, lineModificationInfos.getG2(), side2Reporter);
            modifyB2(line, lineModificationInfos.getB2(), side2Reporter);
        }
    }

    @Override
    protected boolean characteristicsModified(BranchModificationInfos branchModificationInfos) {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) branchModificationInfos;
        return super.characteristicsModified(branchModificationInfos)
            || lineModificationInfos.getG1() != null
            && lineModificationInfos.getG1().getValue() != null
            || lineModificationInfos.getB1() != null
            && lineModificationInfos.getB1().getValue() != null
            || lineModificationInfos.getG2() != null
            && lineModificationInfos.getG2().getValue() != null
            || lineModificationInfos.getB2() != null
            && lineModificationInfos.getB2().getValue() != null;
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
