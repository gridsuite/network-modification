/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.AttributeModification;
import org.gridsuite.modification.model.ShuntCompensatorModificationModel;
import org.gridsuite.modification.model.constants.ShuntCompensatorType;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_SHUNT_COMPENSATOR_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.SHUNT_COMPENSATOR_NOT_FOUND;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class ShuntCompensatorModification extends AbstractInjectionModification {
    private static final String SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE = "Switched-on Q at nominal voltage";

    public ShuntCompensatorModification(ShuntCompensatorModificationModel shuntCompensatorModificationModel) {
        super(shuntCompensatorModificationModel);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        ShuntCompensatorModificationModel shuntCompensatorModificationModel = (ShuntCompensatorModificationModel) modificationModel;
        ShuntCompensator shuntCompensator = network.getShuntCompensator(shuntCompensatorModificationModel.getEquipmentId());
        if (shuntCompensator == null) {
            throw new NetworkModificationException(SHUNT_COMPENSATOR_NOT_FOUND,
                    String.format("Shunt compensator %s does not exist in network", shuntCompensatorModificationModel.getEquipmentId()));
        }
        ModificationUtils.getInstance().checkVoltageLevelModification(network, shuntCompensatorModificationModel.getVoltageLevelId(),
                shuntCompensatorModificationModel.getBusOrBusbarSectionId(), shuntCompensator.getTerminal());
        int maximumSectionCount = shuntCompensatorModificationModel.getMaximumSectionCount() != null
                ? shuntCompensatorModificationModel.getMaximumSectionCount().getValue()
                : shuntCompensator.getMaximumSectionCount();

        int sectionCount = shuntCompensatorModificationModel.getSectionCount() != null
                ? shuntCompensatorModificationModel.getSectionCount().getValue()
                : shuntCompensator.getSectionCount();

        if (shuntCompensatorModificationModel.getMaximumSectionCount() != null && shuntCompensatorModificationModel.getMaximumSectionCount().getValue() < 1) {
            throw new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR, "Maximum section count should be greater or equal to 1");
        }

        if (sectionCount < 0 || maximumSectionCount < 1 || sectionCount > maximumSectionCount) {
            throw new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR, String.format("Section count should be between 0 and Maximum section count (%d), actual : %d", maximumSectionCount, sectionCount));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ShuntCompensatorModificationModel shuntCompensatorModificationModel = (ShuntCompensatorModificationModel) modificationModel;
        ShuntCompensator shuntCompensator = network.getShuntCompensator(shuntCompensatorModificationModel.getEquipmentId());
        VoltageLevel voltageLevel = shuntCompensator.getTerminal().getVoltageLevel();

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.shuntCompensatorModification.withId")
                .withUntypedValue("id", shuntCompensatorModificationModel.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        ModificationUtils.getInstance().applyElementaryModifications(shuntCompensator::setName, () -> shuntCompensator.getOptionalName().orElse("No value"), modificationModel.getEquipmentName(), subReportNode, "Name");

        if (shuntCompensator.getModelType() == ShuntCompensatorModelType.LINEAR) {
            applyModificationOnLinearModel(subReportNode, shuntCompensator, voltageLevel);
        }
        modifyShuntCompensatorVoltageLevelBusOrBusBarSectionAttributes(shuntCompensatorModificationModel, shuntCompensator, subReportNode);
        modifyShuntCompensatorConnectivityAttributes(shuntCompensatorModificationModel, shuntCompensator, subReportNode);
        updateMeasurements(shuntCompensator, shuntCompensatorModificationModel, subReportNode);
        PropertiesUtils.applyProperties(shuntCompensator, subReportNode, shuntCompensatorModificationModel.getProperties(), "network.modification.ShuntCompensatorProperties");
    }

    @Override
    public String getName() {
        return "ShuntCompensatorModification";
    }

    public static void modifyMaximumSectionCount(AttributeModification<Integer> maximumSectionCountModif,
                                                 AttributeModification<Double> maxSusceptance,
                                                 AttributeModification<Double> maxQAtNominalV,
                                                 List<ReportNode> reports,
                                                 ShuntCompensator shuntCompensator,
                                                 ShuntCompensatorLinearModel model) {
        if (maximumSectionCountModif != null) {
            var maximumSectionCount = maximumSectionCountModif.getValue();
            if (maxSusceptance == null && maxQAtNominalV == null) {
                model.setBPerSection((model.getBPerSection() * shuntCompensator.getMaximumSectionCount()) / maximumSectionCount);
            }
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensator.getMaximumSectionCount(), maximumSectionCount, "Maximum section count"));
            }
            model.setMaximumSectionCount(maximumSectionCount);
        }
    }

    public static void modifySectionCount(AttributeModification<Integer> sectionCount, List<ReportNode> reports, ShuntCompensator shuntCompensator) {
        if (sectionCount != null) {
            var newSectionCount = sectionCount.getValue();
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensator.getSectionCount(), newSectionCount, "Section count"));
            }
            shuntCompensator.setSectionCount(newSectionCount);
        }
    }

    private void applyModificationOnLinearModel(ReportNode subReportNode, ShuntCompensator shuntCompensator, VoltageLevel voltageLevel) {
        ShuntCompensatorModificationModel shuntCompensatorModificationModel = (ShuntCompensatorModificationModel) modificationModel;
        List<ReportNode> reports = new ArrayList<>();
        ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        var shuntCompensatorType = model.getBPerSection() > 0 ? ShuntCompensatorType.CAPACITOR : ShuntCompensatorType.REACTOR;
        double oldSusceptancePerSection = model.getBPerSection();
        double oldQAtNominalV = Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * oldSusceptancePerSection);
        double oldMaxQAtNominalV = oldQAtNominalV * shuntCompensator.getMaximumSectionCount();
        double oldSwitchedOnSusceptance = oldSusceptancePerSection * shuntCompensator.getSectionCount();
        double oldSwitchedOnQAtNominalV = oldQAtNominalV * shuntCompensator.getSectionCount();

        if (shuntCompensatorModificationModel.getShuntCompensatorType() != null || shuntCompensatorModificationModel.getMaxQAtNominalV() != null) {
            shuntCompensatorModificationModel.setMaxSusceptance(null);
        }

        // due to cross validation between maximum section count and section count, we need to modify section count first
        // when maximum section count old value is greater than the new one
        if (shuntCompensatorModificationModel.getMaximumSectionCount() != null && shuntCompensatorModificationModel.getMaximumSectionCount().getValue() < shuntCompensator.getMaximumSectionCount()) {
            modifySectionCount(shuntCompensatorModificationModel.getSectionCount(), reports, shuntCompensator);
            modifyMaximumSectionCount(shuntCompensatorModificationModel.getMaximumSectionCount(),
                    shuntCompensatorModificationModel.getMaxSusceptance(),
                    shuntCompensatorModificationModel.getMaxQAtNominalV(),
                    reports, shuntCompensator, model);
        } else {
            modifyMaximumSectionCount(shuntCompensatorModificationModel.getMaximumSectionCount(),
                    shuntCompensatorModificationModel.getMaxSusceptance(),
                    shuntCompensatorModificationModel.getMaxQAtNominalV(),
                    reports, shuntCompensator, model);
            modifySectionCount(shuntCompensatorModificationModel.getSectionCount(), reports, shuntCompensator);
        }

        int maximumSectionCount = shuntCompensatorModificationModel.getMaximumSectionCount() != null ? shuntCompensatorModificationModel.getMaximumSectionCount().getValue() : shuntCompensator.getMaximumSectionCount();
        int sectionCount = shuntCompensatorModificationModel.getSectionCount() != null ? shuntCompensatorModificationModel.getSectionCount().getValue() : shuntCompensator.getSectionCount();

        if (shuntCompensatorModificationModel.getShuntCompensatorType() != null) {
            reports.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensatorType, shuntCompensatorModificationModel.getShuntCompensatorType().getValue(), "Type"));
            shuntCompensatorType = shuntCompensatorModificationModel.getShuntCompensatorType().getValue();
            if (shuntCompensatorModificationModel.getMaxQAtNominalV() == null) {
                // we retrieve the absolute value of susceptance per section, then we determine the sign using the type
                double bPerSectionAbsoluteValue = Math.abs(oldSusceptancePerSection);
                double newBPerSection = shuntCompensatorType == ShuntCompensatorType.CAPACITOR ? bPerSectionAbsoluteValue : -bPerSectionAbsoluteValue;
                model.setBPerSection(newBPerSection);
            }
        }

        if (shuntCompensatorModificationModel.getMaxQAtNominalV() != null) {
            modifyMaximumQAtNominalVoltage(shuntCompensatorModificationModel.getMaxQAtNominalV(), voltageLevel, maximumSectionCount, reports, model, shuntCompensatorType);
        }
        if (shuntCompensatorModificationModel.getMaxSusceptance() != null) {
            modifyMaxSusceptance(shuntCompensatorModificationModel.getMaxSusceptance(), maximumSectionCount, reports, model);
        }

        reportSwitchedOnAndPerSectionValues(reports, oldQAtNominalV, oldSwitchedOnQAtNominalV, oldSusceptancePerSection, oldSwitchedOnSusceptance, oldMaxQAtNominalV, sectionCount, maximumSectionCount);

        reports.forEach(report -> insertReportNode(subReportNode, report));
    }

    public static void modifyMaxSusceptance(AttributeModification<Double> maxSusceptance,
                                            int maximumSectionCount,
                                            List<ReportNode> reports,
                                            ShuntCompensatorLinearModel model) {
        double newSusceptancePerSection = maxSusceptance.getValue() / maximumSectionCount;
        if (reports != null) {
            double oldSusceptancePerSection = model.getBPerSection();
            double oldMaxSusceptance = oldSusceptancePerSection * maximumSectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldMaxSusceptance, maxSusceptance.getValue(), "Maximal susceptance available"));
        }
        model.setBPerSection(newSusceptancePerSection);
    }

    public static void modifyMaximumQAtNominalVoltage(AttributeModification<Double> maxQAtNominalV,
                                                       VoltageLevel voltageLevel,
                                                       int maximumSectionCount,
                                                       List<ReportNode> reports,
                                                       ShuntCompensatorLinearModel model,
                                                       ShuntCompensatorType shuntCompensatorType) {
        if (maxQAtNominalV.getValue() < 0) {
            throw new NetworkModificationException(NetworkModificationException.Type.MODIFY_SHUNT_COMPENSATOR_ERROR,
                    "Qmax at nominal voltage should be greater or equal to 0");
        }
        double newQatNominalV = maxQAtNominalV.getValue() / maximumSectionCount;
        double newSusceptancePerSection = newQatNominalV / Math.pow(voltageLevel.getNominalV(), 2);
        if (reports != null) {
            double oldSusceptancePerSection = model.getBPerSection();
            double oldQAtNominalV = Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * oldSusceptancePerSection);
            double oldMaxQAtNominalV = oldQAtNominalV * maximumSectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldMaxQAtNominalV, maxQAtNominalV.getValue(), "Qmax available at nominal voltage"));
        }

        model.setBPerSection(shuntCompensatorType == ShuntCompensatorType.CAPACITOR ? newSusceptancePerSection : -newSusceptancePerSection);
    }

    private void reportSwitchedOnAndPerSectionValues(List<ReportNode> reports, double oldQAtNominalV, double oldSwitchedOnQAtNominalV, double oldSusceptancePerSection, double oldSwitchedOnSusceptance, double oldMaxQAtNominalV, int sectionCount, int maximumSectionCount) {
        ShuntCompensatorModificationModel shuntCompensatorModificationModel = (ShuntCompensatorModificationModel) modificationModel;
        if (shuntCompensatorModificationModel.getMaxQAtNominalV() != null) {
            double newQatNominalV = shuntCompensatorModificationModel.getMaxQAtNominalV().getValue() / maximumSectionCount;
            double newSwitchedOnQAtNominalV = newQatNominalV * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldQAtNominalV, newQatNominalV, "Q at nominal voltage per section"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnQAtNominalV, newSwitchedOnQAtNominalV, SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE));
        } else if (shuntCompensatorModificationModel.getMaxSusceptance() != null) {
            double newSusceptancePerSection = shuntCompensatorModificationModel.getMaxSusceptance().getValue() / maximumSectionCount;
            double newSwitchedOnSusceptance = newSusceptancePerSection * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSusceptancePerSection, newSusceptancePerSection, "Susceptance per section"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnSusceptance, newSwitchedOnSusceptance, "Switched-on susceptance"));
        } else if (shuntCompensatorModificationModel.getMaximumSectionCount() != null) {
            double newQatNominalV = oldMaxQAtNominalV / maximumSectionCount;
            double newSwitchedOnQAtNominalV = newQatNominalV * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldQAtNominalV, newQatNominalV, "Q at nominal voltage per section"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnQAtNominalV, newSwitchedOnQAtNominalV, SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE));
        } else if (shuntCompensatorModificationModel.getSectionCount() != null) {
            double newSwitchedOnQAtNominalV = oldQAtNominalV * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnQAtNominalV, newSwitchedOnQAtNominalV, SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE));
        }
    }

    private void modifyShuntCompensatorVoltageLevelBusOrBusBarSectionAttributes(ShuntCompensatorModificationModel modificationModel,
                                                                                ShuntCompensator shuntCompensator, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                shuntCompensator, shuntCompensator.getTerminal(),
                modificationModel.getVoltageLevelId(),
                modificationModel.getBusOrBusbarSectionId(),
                subReportNode
        );
    }

    private ReportNode modifyShuntCompensatorConnectivityAttributes(ShuntCompensatorModificationModel modificationModel,
                                                        ShuntCompensator shuntCompensator, ReportNode subReportNode) {
        ConnectablePosition<ShuntCompensator> connectablePosition = shuntCompensator.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<ShuntCompensator> connectablePositionAdder = shuntCompensator.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, shuntCompensator, modificationModel, subReportNode);
    }
}
