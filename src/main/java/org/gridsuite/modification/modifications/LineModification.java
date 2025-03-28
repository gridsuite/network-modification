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
        modifyBranch(line, lineModificationInfos, subReportNode, "lineModification", "Line with id=${id} modified :");
        updateMeasurements(line, lineModificationInfos, subReportNode);
        PropertiesUtils.applyProperties(line, subReportNode, modificationInfos.getProperties(), "LineProperties");
    }

    @Override
    protected void modifyCharacteristics(Branch<?> branch, BranchModificationInfos branchModificationInfos, ReportNode subReportNode) {
        Line line = (Line) branch;
        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("characteristics", "Characteristics").add();
        if (branchModificationInfos.getR() != null && branchModificationInfos.getR().getValue() != null) {
            insertReportNode(characteristicsReporter, ModificationUtils.getInstance().buildModificationReport(line.getR(),
                    branchModificationInfos.getR().getValue(), "Series resistance", 1));
            line.setR(branchModificationInfos.getR().getValue());
        }
        if (branchModificationInfos.getX() != null && branchModificationInfos.getX().getValue() != null) {
            insertReportNode(characteristicsReporter, ModificationUtils.getInstance().buildModificationReport(line.getX(),
                    branchModificationInfos.getX().getValue(), "Series reactance", 1));
            line.setX(branchModificationInfos.getX().getValue());
        }

        LineModificationInfos lineModificationInfos = (LineModificationInfos) branchModificationInfos;
        modifySide1Characteristics(line, lineModificationInfos, characteristicsReporter);
        modifySide2Characteristics(line, lineModificationInfos, characteristicsReporter);

    }

    private void modifySide1Characteristics(Line line, LineModificationInfos lineModificationInfos,
            ReportNode characteristicsReportNode) {
        if (lineModificationInfos.getG1() != null && lineModificationInfos.getG1().getValue() != null
                || lineModificationInfos.getB1() != null && lineModificationInfos.getB1().getValue() != null) {
            ReportNode side1ReportNode = characteristicsReportNode.newReportNode().withMessageTemplate("side1Characteristics", "Side 1").add();
            if (lineModificationInfos.getG1() != null && lineModificationInfos.getG1().getValue() != null) {
                //convert reported value from siemens to microsiemens
                double shuntConductance1ToReport = lineModificationInfos.getG1().getValue() * Math.pow(10, 6);
                double oldShuntConductance1ToReport = line.getG1() * Math.pow(10, 6);
                insertReportNode(side1ReportNode, ModificationUtils.getInstance().buildModificationReport(oldShuntConductance1ToReport,
                        shuntConductance1ToReport, "Shunt conductance", 2));
                line.setG1(lineModificationInfos.getG1().getValue());
            }
            if (lineModificationInfos.getB1() != null && lineModificationInfos.getB1().getValue() != null) {
                //convert reported value from siemens to microsiemens
                double shuntSusceptance1ToReport = lineModificationInfos.getB1().getValue() * Math.pow(10, 6);
                double oldShuntSusceptance1ToReport = line.getB1() * Math.pow(10, 6);
                insertReportNode(side1ReportNode, ModificationUtils.getInstance().buildModificationReport(oldShuntSusceptance1ToReport,
                        shuntSusceptance1ToReport, "Shunt susceptance", 2));
                line.setB1(lineModificationInfos.getB1().getValue());
            }
        }
    }

    private void modifySide2Characteristics(Line line, LineModificationInfos lineModificationInfos,
            ReportNode characteristicsReportNode) {
        if (lineModificationInfos.getG2() != null && lineModificationInfos.getG2().getValue() != null
                || lineModificationInfos.getB2() != null && lineModificationInfos.getB2().getValue() != null) {
            ReportNode side2Reporter = characteristicsReportNode.newReportNode().withMessageTemplate("side2Characteristics", "Side 2").add();
            if (lineModificationInfos.getG2() != null && lineModificationInfos.getG2().getValue() != null) {
                // convert reported value from siemens to microsiemens
                double shuntConductance2ToReport = lineModificationInfos.getG2().getValue() * Math.pow(10, 6);
                double oldShuntConductance2ToReport = line.getG2() * Math.pow(10, 6);
                insertReportNode(side2Reporter, ModificationUtils.getInstance().buildModificationReport(oldShuntConductance2ToReport,
                        shuntConductance2ToReport, "Shunt conductance", 2));
                line.setG2(lineModificationInfos.getG2().getValue());
            }
            if (lineModificationInfos.getB2() != null && lineModificationInfos.getB2().getValue() != null) {
                // convert reported value from siemens to microsiemens
                double shuntSusceptance2ToReport = lineModificationInfos.getB2().getValue() * Math.pow(10, 6);
                double oldShuntSusceptance2ToReport = line.getB2() * Math.pow(10, 6);
                insertReportNode(side2Reporter, ModificationUtils.getInstance().buildModificationReport(oldShuntSusceptance2ToReport,
                        shuntSusceptance2ToReport, "Shunt susceptance", 2));
                line.setB2(lineModificationInfos.getB2().getValue());
            }
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

}
