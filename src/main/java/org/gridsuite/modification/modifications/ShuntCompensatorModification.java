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
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.dto.ShuntCompensatorType;
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

public class ShuntCompensatorModification extends AbstractModification {
    private static final String SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE = "Switched-on Q at nominal voltage";

    private final ShuntCompensatorModificationInfos modificationInfos;

    public ShuntCompensatorModification(ShuntCompensatorModificationInfos shuntCompensatorModificationInfos) {
        this.modificationInfos = shuntCompensatorModificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        ShuntCompensator shuntCompensator = network.getShuntCompensator(modificationInfos.getEquipmentId());
        if (shuntCompensator == null) {
            throw new NetworkModificationException(SHUNT_COMPENSATOR_NOT_FOUND,
                    String.format("Shunt compensator %s does not exist in network", modificationInfos.getEquipmentId()));
        }

        int maximumSectionCount = modificationInfos.getMaximumSectionCount() != null
                ? modificationInfos.getMaximumSectionCount().getValue()
                : shuntCompensator.getMaximumSectionCount();

        int sectionCount = modificationInfos.getSectionCount() != null
                ? modificationInfos.getSectionCount().getValue()
                : shuntCompensator.getSectionCount();

        if (modificationInfos.getMaximumSectionCount() != null && modificationInfos.getMaximumSectionCount().getValue() < 1) {
            throw new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR, "Maximum section count should be greater or equal to 1");
        }

        if (sectionCount < 0 || maximumSectionCount < 1 || sectionCount > maximumSectionCount) {
            throw new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR, String.format("Section count should be between 0 and Maximum section count (%d), actual : %d", maximumSectionCount, sectionCount));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ShuntCompensator shuntCompensator = network.getShuntCompensator(modificationInfos.getEquipmentId());
        VoltageLevel voltageLevel = shuntCompensator.getTerminal().getVoltageLevel();

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.shuntCompensatorModification.withId")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        ModificationUtils.getInstance().applyElementaryModifications(shuntCompensator::setName, () -> shuntCompensator.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");

        if (shuntCompensator.getModelType() == ShuntCompensatorModelType.LINEAR) {
            applyModificationOnLinearModel(subReportNode, shuntCompensator, voltageLevel);
        }
        modifyShuntCompensatorConnectivityAttributes(modificationInfos, shuntCompensator, subReportNode);
        PropertiesUtils.applyProperties(shuntCompensator, subReportNode, modificationInfos.getProperties(), "network.modification.ShuntCompensatorProperties");
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
        List<ReportNode> reports = new ArrayList<>();
        ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        var shuntCompensatorType = model.getBPerSection() > 0 ? ShuntCompensatorType.CAPACITOR : ShuntCompensatorType.REACTOR;
        double oldSusceptancePerSection = model.getBPerSection();
        double oldQAtNominalV = Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * oldSusceptancePerSection);
        double oldMaxQAtNominalV = oldQAtNominalV * shuntCompensator.getMaximumSectionCount();
        double oldSwitchedOnSusceptance = oldSusceptancePerSection * shuntCompensator.getSectionCount();
        double oldSwitchedOnQAtNominalV = oldQAtNominalV * shuntCompensator.getSectionCount();

        if (modificationInfos.getShuntCompensatorType() != null || modificationInfos.getMaxQAtNominalV() != null) {
            modificationInfos.setMaxSusceptance(null);
        }

        // due to cross validation between maximum section count and section count, we need to modify section count first
        // when maximum section count old value is greater than the new one
        if (modificationInfos.getMaximumSectionCount() != null && modificationInfos.getMaximumSectionCount().getValue() < shuntCompensator.getMaximumSectionCount()) {
            modifySectionCount(modificationInfos.getSectionCount(), reports, shuntCompensator);
            modifyMaximumSectionCount(modificationInfos.getMaximumSectionCount(),
                    modificationInfos.getMaxSusceptance(),
                    modificationInfos.getMaxQAtNominalV(),
                    reports, shuntCompensator, model);
        } else {
            modifyMaximumSectionCount(modificationInfos.getMaximumSectionCount(),
                    modificationInfos.getMaxSusceptance(),
                    modificationInfos.getMaxQAtNominalV(),
                    reports, shuntCompensator, model);
            modifySectionCount(modificationInfos.getSectionCount(), reports, shuntCompensator);
        }

        int maximumSectionCount = modificationInfos.getMaximumSectionCount() != null ? modificationInfos.getMaximumSectionCount().getValue() : shuntCompensator.getMaximumSectionCount();
        int sectionCount = modificationInfos.getSectionCount() != null ? modificationInfos.getSectionCount().getValue() : shuntCompensator.getSectionCount();

        if (modificationInfos.getShuntCompensatorType() != null) {
            reports.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensatorType, modificationInfos.getShuntCompensatorType().getValue(), "Type"));
            shuntCompensatorType = modificationInfos.getShuntCompensatorType().getValue();
            if (modificationInfos.getMaxQAtNominalV() == null) {
                // we retrieve the absolute value of susceptance per section, then we determine the sign using the type
                double bPerSectionAbsoluteValue = Math.abs(oldSusceptancePerSection);
                double newBPerSection = shuntCompensatorType == ShuntCompensatorType.CAPACITOR ? bPerSectionAbsoluteValue : -bPerSectionAbsoluteValue;
                model.setBPerSection(newBPerSection);
            }
        }

        if (modificationInfos.getMaxQAtNominalV() != null) {
            modifyMaximumQAtNominalVoltage(modificationInfos.getMaxQAtNominalV(), voltageLevel, maximumSectionCount, reports, model, shuntCompensatorType);
        }
        if (modificationInfos.getMaxSusceptance() != null) {
            modifyMaxSusceptance(modificationInfos.getMaxSusceptance(), maximumSectionCount, reports, model);
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
        if (modificationInfos.getMaxQAtNominalV() != null) {
            double newQatNominalV = modificationInfos.getMaxQAtNominalV().getValue() / maximumSectionCount;
            double newSwitchedOnQAtNominalV = newQatNominalV * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldQAtNominalV, newQatNominalV, "Q at nominal voltage per section"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnQAtNominalV, newSwitchedOnQAtNominalV, SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE));
        } else if (modificationInfos.getMaxSusceptance() != null) {
            double newSusceptancePerSection = modificationInfos.getMaxSusceptance().getValue() / maximumSectionCount;
            double newSwitchedOnSusceptance = newSusceptancePerSection * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSusceptancePerSection, newSusceptancePerSection, "Susceptance per section"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnSusceptance, newSwitchedOnSusceptance, "Switched-on susceptance"));
        } else if (modificationInfos.getMaximumSectionCount() != null) {
            double newQatNominalV = oldMaxQAtNominalV / maximumSectionCount;
            double newSwitchedOnQAtNominalV = newQatNominalV * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldQAtNominalV, newQatNominalV, "Q at nominal voltage per section"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnQAtNominalV, newSwitchedOnQAtNominalV, SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE));
        } else if (modificationInfos.getSectionCount() != null) {
            double newSwitchedOnQAtNominalV = oldQAtNominalV * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnQAtNominalV, newSwitchedOnQAtNominalV, SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE));
        }
    }

    private ReportNode modifyShuntCompensatorConnectivityAttributes(ShuntCompensatorModificationInfos modificationInfos,
                                                        ShuntCompensator shuntCompensator, ReportNode subReportNode) {
        ConnectablePosition<ShuntCompensator> connectablePosition = shuntCompensator.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<ShuntCompensator> connectablePositionAdder = shuntCompensator.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, shuntCompensator, modificationInfos, subReportNode);
    }
}
