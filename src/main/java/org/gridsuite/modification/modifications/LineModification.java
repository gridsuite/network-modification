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
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.OperationalLimitsGroupModificationInfos;
import org.gridsuite.modification.dto.OperationalLimitsGroupsModificationType;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_LINE_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
@Getter
@Setter
public class LineModification extends AbstractBranchModification {

    private static final String MAGNETIZING_SUSCEPTANCE_MESSAGE = "Magnetizing susceptance";
    private static final String MAGNETIZING_CONDUCTANCE_MESSAGE = "Magnetizing conductance";
    public static final String ERROR_MESSAGE = "Line '%s' : ";
    private AttributeModification<Double> g1;
    private AttributeModification<Double> b1;
    private AttributeModification<Double> g2;
    private AttributeModification<Double> b2;

    @Builder
    public LineModification(String equipmentId, List<FreePropertyInfos> properties,
                            AttributeModification<String> equipmentName, AttributeModification<Double> r,
                            AttributeModification<Double> x,
                            OperationalLimitsGroupsModificationType operationalLimitsGroupsModificationType,
                            Boolean enableOLGModification,
                            List<OperationalLimitsGroupModificationInfos> operationalLimitsGroups,
                            AttributeModification<String> selectedOperationalLimitsGroupId1,
                            AttributeModification<String> selectedOperationalLimitsGroupId2,
                            AttributeModification<String> voltageLevelId1,
                            AttributeModification<String> voltageLevelId2,
                            AttributeModification<String> busOrBusbarSectionId1,
                            AttributeModification<String> busOrBusbarSectionId2,
                            AttributeModification<String> connectionName1,
                            AttributeModification<String> connectionName2,
                            AttributeModification<ConnectablePosition.Direction> connectionDirection1,
                            AttributeModification<ConnectablePosition.Direction> connectionDirection2,
                            AttributeModification<Integer> connectionPosition1,
                            AttributeModification<Integer> connectionPosition2,
                            AttributeModification<Boolean> terminal1Connected,
                            AttributeModification<Boolean> terminal2Connected,
                            AttributeModification<Double> p1MeasurementValue,
                            AttributeModification<Boolean> p1MeasurementValidity,
                            AttributeModification<Double> p2MeasurementValue,
                            AttributeModification<Boolean> p2MeasurementValidity,
                            AttributeModification<Double> q1MeasurementValue,
                            AttributeModification<Boolean> q1MeasurementValidity,
                            AttributeModification<Double> q2MeasurementValue,
                            AttributeModification<Boolean> q2MeasurementValidity, AttributeModification<Double> g1,
                            AttributeModification<Double> b1, AttributeModification<Double> g2,
                            AttributeModification<Double> b2) {
        super(equipmentId, properties, equipmentName, r, x, operationalLimitsGroupsModificationType,
            enableOLGModification,
            operationalLimitsGroups, selectedOperationalLimitsGroupId1, selectedOperationalLimitsGroupId2,
            voltageLevelId1,
            voltageLevelId2, busOrBusbarSectionId1, busOrBusbarSectionId2, connectionName1, connectionName2,
            connectionDirection1, connectionDirection2, connectionPosition1, connectionPosition2, terminal1Connected,
            terminal2Connected, p1MeasurementValue, p1MeasurementValidity, p2MeasurementValue, p2MeasurementValidity,
            q1MeasurementValue, q1MeasurementValidity, q2MeasurementValue, q2MeasurementValidity);
        this.g1 = g1;
        this.b1 = b1;
        this.g2 = g2;
        this.b2 = b2;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Line line = network.getLine(equipmentId);
        String errorMessage = "Line '" + equipmentId + "' : ";
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, errorMessage + "does not exist in network");
        }
        ModificationUtils.getInstance().checkVoltageLevelModification(network, voltageLevelId1,
                busOrBusbarSectionId1, line.getTerminal1());
        ModificationUtils.getInstance().checkVoltageLevelModification(network, voltageLevelId2,
                busOrBusbarSectionId2, line.getTerminal2());
        if (r != null) {
            checkIsNotNegativeValue(errorMessage, r.getValue(), MODIFY_LINE_ERROR, "Resistance R");
        }
        if (g1 != null) {
            checkIsNotNegativeValue(errorMessage, g1.getValue(), MODIFY_LINE_ERROR, "Conductance on side 1 G1");
        }
        if (g2 != null) {
            checkIsNotNegativeValue(errorMessage, g2.getValue(), MODIFY_LINE_ERROR, "Conductance on side 2 G2");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Line line = network.getLine(equipmentId);
        // modify the line in the network
        modifyLine(line, subReportNode);
    }

    @Override
    public String getName() {
        return "LineModification";
    }

    private void modifyLine(Line line, ReportNode subReportNode) {
        modifyBranch(line, subReportNode, "network.modification.lineModification");
        updateMeasurements(line, subReportNode);
        PropertiesUtils.applyProperties(line, subReportNode, properties, "network.modification.LineProperties");
    }

    @Override
    protected void modifyCharacteristics(Branch<?> branch, ReportNode subReportNode) {
        Line line = (Line) branch;
        ReportNode characteristicsReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.characteristics").add();
        modifyR(line, r, characteristicsReporter);
        modifyX(line, x, characteristicsReporter);

        modifySide1Characteristics(line, characteristicsReporter);
        modifySide2Characteristics(line, characteristicsReporter);

    }

    private void modifySide1Characteristics(Line line, ReportNode characteristicsReportNode) {
        if (g1 != null && g1.getValue() != null
            || b1 != null && b1.getValue() != null) {
            ReportNode side1ReportNode = characteristicsReportNode.newReportNode().withMessageTemplate("network.modification.side1Characteristics").add();
            modifyG1(line, g1, side1ReportNode);
            modifyB1(line, b1, side1ReportNode);
        }
    }

    private void modifySide2Characteristics(Line line, ReportNode characteristicsReportNode) {
        if (g2 != null && g2.getValue() != null
            || b2 != null && b2.getValue() != null) {
            ReportNode side2Reporter = characteristicsReportNode.newReportNode().withMessageTemplate("network.modification.side2Characteristics").add();
            modifyG2(line, g2, side2Reporter);
            modifyB2(line, b2, side2Reporter);
        }
    }

    @Override
    protected boolean characteristicsModified() {
        return super.characteristicsModified()
            || g1 != null
            && g1.getValue() != null
            || b1 != null
            && b1.getValue() != null
            || g2 != null
            && g2.getValue() != null
            || b2 != null
            && b2.getValue() != null;
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
