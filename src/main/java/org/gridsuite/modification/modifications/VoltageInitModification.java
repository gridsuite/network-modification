/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportConstants;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import lombok.AllArgsConstructor;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.NetworkModificationException.Type;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;

import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@AllArgsConstructor
public class VoltageInitModification extends AbstractModification {
    private final VoltageInitModificationInfos voltageInitModificationInfos;

    private static final String GENERATORS_KEY = "network.modification.GeneratorsModifications";
    private static final String TWO_WINDINGS_TRANSFORMERS_KEY = "network.modification.2WindingsTransformersModifications";
    private static final String THREE_WINDINGS_TRANSFORMERS_KEY = "network.modification.3WindingsTransformersModifications";
    private static final String STATIC_VAR_COMPENSATORS_KEY = "network.modification.StaticVarCompensatorsModifications";
    private static final String VSC_CONVERTER_STATIONS_KEY = "network.modification.VscConverterStationsModifications";
    private static final String SHUNT_COMPENSATORS_KEY = "network.modification.ShuntCompensatorsModifications";

    private static final String VOLTAGE_SET_POINT = "Voltage set point";
    private static final String VOLTAGE_MAGNITUDE = "Voltage magnitude";
    private static final String VOLTAGE_ANGLE = "Voltage angle";
    private static final String REACTIVE_POWER_SET_POINT = "Reactive power set point";
    private static final String SECTION_COUNT = "Section count";
    private static final String COUNT = "count";

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (voltageInitModificationInfos == null) {
            throw new NetworkModificationException(Type.VOLTAGE_INIT_MODIFICATION_ERROR, "No voltage init modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // apply generators modifications
        applyGeneratorModification(network, subReportNode);

        // apply transformers modifications
        applyTransformerModification(network, subReportNode);

        // apply static var compensators modifications
        applyStaticVarCompensatorModification(network, subReportNode);

        // apply shunt compensators modifications
        applyShuntCompensatorModification(network, subReportNode);

        // apply vsc converter stations modifications
        applyVscConverterStationModification(network, subReportNode);

        // apply buses modifications
        applyBusModification(network, subReportNode);
    }

    @Override
    public String getName() {
        return "VoltageInitModification";
    }

    private void applyBusModification(Network network, ReportNode subReportNode) {
        int modificationsCount = 0;
        List<ReportNode> reports = new ArrayList<>();
        for (VoltageInitBusModificationInfos m : voltageInitModificationInfos.getBuses()) {
            String voltageLevelId = m.getVoltageLevelId();
            Bus bus;
            if (voltageLevelId != null) {
                final VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
                bus = voltageLevel.getBusView().getBus(m.getBusId());
            } else {
                bus = network.getBusView().getBus(m.getBusId());
            }
            if (bus == null) {
                reports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.busNotFound")
                        .withUntypedValue("id", m.getBusId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else if (m.getV() != null || m.getAngle() != null) {
                modificationsCount++;
                reports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.busModification")
                        .withUntypedValue("id", m.getBusId())
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build());
                if (m.getV() != null) {
                    final double oldV = bus.getV();
                    bus.setV(m.getV());
                    reports.add(ModificationUtils.buildModificationReport(oldV, m.getV(), VOLTAGE_MAGNITUDE,
                            TypedValue.DETAIL_SEVERITY));
                }
                if (m.getAngle() != null) {
                    final double oldAngle = bus.getAngle();
                    bus.setAngle(m.getAngle());
                    reports.add(ModificationUtils.buildModificationReport(oldAngle, m.getAngle(), VOLTAGE_ANGLE,
                            TypedValue.DETAIL_SEVERITY));
                }
            }
        }
        if (!reports.isEmpty()) {
            ReportNode busesReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.BusesModifications").add();
            reports.forEach(report -> insertReportNode(busesReporter, report));
        }
        if (modificationsCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.busModificationsResume")
                    .withUntypedValue(COUNT, modificationsCount)
                    .withTypedValue(ReportConstants.SEVERITY_KEY, TypedValue.INFO_SEVERITY.toString(), TypedValue.SEVERITY)
                    .add();
        }
    }

    private void applyGeneratorModification(Network network, ReportNode subReportNode) {
        int modificationsCount = 0;
        List<ReportNode> reports = new ArrayList<>();
        for (final VoltageInitGeneratorModificationInfos m : voltageInitModificationInfos.getGenerators()) {
            final Generator generator = network.getGenerator(m.getGeneratorId());
            if (generator == null) {
                reports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.generatorNotFound")
                        .withUntypedValue("id", m.getGeneratorId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else if (m.getTargetV() != null || m.getTargetQ() != null) {
                modificationsCount++;
                reports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.generatorModification")
                        .withUntypedValue("id", m.getGeneratorId())
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build());
                if (m.getTargetV() != null) {
                    final double oldTargetV = generator.getTargetV();
                    generator.setTargetV(m.getTargetV());
                    reports.add(ModificationUtils.buildModificationReport(oldTargetV, m.getTargetV(), VOLTAGE_SET_POINT, TypedValue.DETAIL_SEVERITY));
                }
                if (m.getTargetQ() != null) {
                    final double oldTargetQ = generator.getTargetQ();
                    generator.setTargetQ(m.getTargetQ());
                    reports.add(ModificationUtils.buildModificationReport(oldTargetQ, m.getTargetQ(), REACTIVE_POWER_SET_POINT, TypedValue.DETAIL_SEVERITY));
                }
            }
        }
        if (!reports.isEmpty()) {
            ReportNode generatorsReportNode = subReportNode.newReportNode().withMessageTemplate(GENERATORS_KEY).add();
            reports.forEach(report -> insertReportNode(generatorsReportNode, report));
        }
        if (modificationsCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.generatorModificationsResume")
                    .withUntypedValue(COUNT, modificationsCount)
                    .withTypedValue(ReportConstants.SEVERITY_KEY, TypedValue.INFO_SEVERITY.toString(), TypedValue.SEVERITY)
                    .add();
        }
    }

    private void applyTransformerModification(Network network, ReportNode subReportNode) {
        int modificationsCount = 0;
        List<ReportNode> reports2WT = new ArrayList<>();
        List<ReportNode> reports3WT = new ArrayList<>();
        for (final VoltageInitTransformerModificationInfos t : voltageInitModificationInfos.getTransformers()) {
            if (t.getLegSide() != null) {
                final ThreeWindingsTransformer threeWindingsTransformer = network.getThreeWindingsTransformer(t.getTransformerId());
                if (threeWindingsTransformer == null) {
                    reports3WT.add(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.3WindingsTransformerNotFound")
                            .withUntypedValue("id", t.getTransformerId())
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .build());
                } else if (threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger() == null) {
                    reports3WT.add(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.3WindingsTransformerRatioTapChangerNotFound")
                            .withUntypedValue("id", t.getTransformerId())
                            .withUntypedValue("leg", t.getLegSide().name())
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .build());
                } else if (t.getRatioTapChangerPosition() != null || t.getRatioTapChangerTargetV() != null) {
                    modificationsCount++;
                    reports3WT.add(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.3WindingsTransformerModification")
                            .withUntypedValue("id", t.getTransformerId())
                            .withSeverity(TypedValue.DETAIL_SEVERITY)
                            .build());
                    if (t.getRatioTapChangerPosition() != null) {
                        final int oldTapPosition = threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().getTapPosition();
                        threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
                        reports3WT.add(ModificationUtils.buildModificationReport(oldTapPosition, t.getRatioTapChangerPosition(), "Leg " + t.getLegSide().name() + " ratio tap changer position", TypedValue.DETAIL_SEVERITY));
                    }
                    if (t.getRatioTapChangerTargetV() != null) {
                        final double oldTapTargetV = threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().getTargetV();
                        threeWindingsTransformer.getLeg(t.getLegSide()).getRatioTapChanger().setTargetV(t.getRatioTapChangerTargetV());
                        reports3WT.add(ModificationUtils.buildModificationReport(oldTapTargetV, t.getRatioTapChangerTargetV(), "Leg " + t.getLegSide().name() + " ratio tap changer target voltage", TypedValue.DETAIL_SEVERITY));
                    }
                }
            } else {
                final TwoWindingsTransformer twoWindingsTransformer = network.getTwoWindingsTransformer(t.getTransformerId());
                if (twoWindingsTransformer == null) {
                    reports2WT.add(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.2WindingsTransformerNotFound")
                            .withUntypedValue("id", t.getTransformerId())
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .build());
                } else if (twoWindingsTransformer.getRatioTapChanger() == null) {
                    reports2WT.add(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.2WindingsTransformerRatioTapChangerNotFound")
                            .withUntypedValue("id", t.getTransformerId())
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .build());
                } else if (t.getRatioTapChangerPosition() != null || t.getRatioTapChangerTargetV() != null) {
                    modificationsCount++;
                    reports2WT.add(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.2WindingsTransformerModification")
                            .withUntypedValue("id", t.getTransformerId())
                            .withSeverity(TypedValue.DETAIL_SEVERITY)
                            .build());
                    if (t.getRatioTapChangerPosition() != null) {
                        final int oldTapPosition = twoWindingsTransformer.getRatioTapChanger().getTapPosition();
                        twoWindingsTransformer.getRatioTapChanger().setTapPosition(t.getRatioTapChangerPosition());
                        reports2WT.add(ModificationUtils.buildModificationReport(oldTapPosition, t.getRatioTapChangerPosition(), "Ratio tap changer position", TypedValue.DETAIL_SEVERITY));
                    }
                    if (t.getRatioTapChangerTargetV() != null) {
                        final double oldTapTargetV = twoWindingsTransformer.getRatioTapChanger().getTargetV();
                        twoWindingsTransformer.getRatioTapChanger().setTargetV(t.getRatioTapChangerTargetV());
                        reports2WT.add(ModificationUtils.buildModificationReport(oldTapTargetV, t.getRatioTapChangerTargetV(), "Ratio tap changer target voltage", TypedValue.DETAIL_SEVERITY));
                    }
                }
            }
        }
        if (!reports2WT.isEmpty()) {
            ReportNode twoWindingsTransformerReportNode = subReportNode.newReportNode().withMessageTemplate(TWO_WINDINGS_TRANSFORMERS_KEY).add();
            reports2WT.forEach(report -> insertReportNode(twoWindingsTransformerReportNode, report));
        }
        if (!reports3WT.isEmpty()) {
            ReportNode threeWindingsTransformerReporter = subReportNode.newReportNode().withMessageTemplate(THREE_WINDINGS_TRANSFORMERS_KEY).add();
            reports3WT.forEach(report -> insertReportNode(threeWindingsTransformerReporter, report));
        }
        if (modificationsCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.windingsTransformerModificationsResume")
                    .withUntypedValue(COUNT, modificationsCount)
                    .withTypedValue(ReportConstants.SEVERITY_KEY, TypedValue.INFO_SEVERITY.toString(), TypedValue.SEVERITY)
                    .add();
        }
    }

    private void applyStaticVarCompensatorModification(Network network, ReportNode subReportNode) {
        int modificationsCount = 0;
        List<ReportNode> reports = new ArrayList<>();
        for (VoltageInitStaticVarCompensatorModificationInfos s : voltageInitModificationInfos.getStaticVarCompensators()) {
            final StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(s.getStaticVarCompensatorId());
            if (staticVarCompensator == null) {
                reports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.staticVarCompensatorNotFound")
                        .withUntypedValue("id", s.getStaticVarCompensatorId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else if (s.getVoltageSetpoint() != null || s.getReactivePowerSetpoint() != null) {
                modificationsCount++;
                reports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.staticVarCompensatorModification")
                        .withUntypedValue("id", s.getStaticVarCompensatorId())
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build());
                if (s.getVoltageSetpoint() != null) {
                    final double oldTargetV = staticVarCompensator.getVoltageSetpoint();
                    staticVarCompensator.setVoltageSetpoint(s.getVoltageSetpoint());
                    reports.add(ModificationUtils.buildModificationReport(oldTargetV, s.getVoltageSetpoint(), VOLTAGE_SET_POINT, TypedValue.DETAIL_SEVERITY));
                }
                if (s.getReactivePowerSetpoint() != null) {
                    final double oldTargetQ = staticVarCompensator.getReactivePowerSetpoint();
                    staticVarCompensator.setReactivePowerSetpoint(s.getReactivePowerSetpoint());
                    reports.add(ModificationUtils.buildModificationReport(oldTargetQ, s.getReactivePowerSetpoint(), REACTIVE_POWER_SET_POINT, TypedValue.DETAIL_SEVERITY));
                }
            }
        }
        if (!reports.isEmpty()) {
            ReportNode staticVarsReportNode = subReportNode.newReportNode().withMessageTemplate(STATIC_VAR_COMPENSATORS_KEY).add();
            reports.forEach(report -> insertReportNode(staticVarsReportNode, report));
        }
        if (modificationsCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.svcModificationsResume")
                    .withUntypedValue(COUNT, modificationsCount)
                    .withTypedValue(ReportConstants.SEVERITY_KEY, TypedValue.INFO_SEVERITY.toString(), TypedValue.SEVERITY)
                    .add();
        }
    }

    private void applyVscConverterStationModification(Network network, ReportNode subReportNode) {
        int modificationsCount = 0;
        List<ReportNode> reports = new ArrayList<>();
        for (VoltageInitVscConverterStationModificationInfos v : voltageInitModificationInfos.getVscConverterStations()) {
            final VscConverterStation vscConverterStation = network.getVscConverterStation(v.getVscConverterStationId());
            if (vscConverterStation == null) {
                reports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.vscConverterStationNotFound")
                        .withUntypedValue("id", v.getVscConverterStationId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else if (v.getVoltageSetpoint() != null || v.getReactivePowerSetpoint() != null) {
                modificationsCount++;
                reports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.vscConverterStationModification")
                        .withUntypedValue("id", v.getVscConverterStationId())
                        .withSeverity(TypedValue.DETAIL_SEVERITY)
                        .build());
                if (v.getVoltageSetpoint() != null) {
                    final double oldTargetV = vscConverterStation.getVoltageSetpoint();
                    vscConverterStation.setVoltageSetpoint(v.getVoltageSetpoint());
                    reports.add(ModificationUtils.buildModificationReport(oldTargetV, v.getVoltageSetpoint(), VOLTAGE_SET_POINT, TypedValue.DETAIL_SEVERITY));
                }
                if (v.getReactivePowerSetpoint() != null) {
                    final double oldTargetQ = vscConverterStation.getReactivePowerSetpoint();
                    vscConverterStation.setReactivePowerSetpoint(v.getReactivePowerSetpoint());
                    reports.add(ModificationUtils.buildModificationReport(oldTargetQ, v.getReactivePowerSetpoint(), REACTIVE_POWER_SET_POINT, TypedValue.DETAIL_SEVERITY));
                }
            }
        }
        if (!reports.isEmpty()) {
            ReportNode vscConverterStationsReporter = subReportNode.newReportNode().withMessageTemplate(VSC_CONVERTER_STATIONS_KEY).add();
            reports.forEach(report -> insertReportNode(vscConverterStationsReporter, report));
        }
        if (modificationsCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.vscModificationsResume")
                    .withUntypedValue(COUNT, modificationsCount)
                    .withTypedValue(ReportConstants.SEVERITY_KEY, TypedValue.INFO_SEVERITY.toString(), TypedValue.SEVERITY)
                    .add();
        }
    }

    private void applyShuntCompensatorModification(Network network, ReportNode subReportNode) {
        int modificationsCount = 0;
        List<ReportNode> reports = new ArrayList<>();
        for (VoltageInitShuntCompensatorModificationInfos m : voltageInitModificationInfos.getShuntCompensators()) {
            final ShuntCompensator shuntCompensator = network.getShuntCompensator(m.getShuntCompensatorId());
            if (shuntCompensator == null) {
                reports.add(ReportNode.newRootReportNode()
                        .withAllResourceBundlesFromClasspath()
                        .withMessageTemplate("network.modification.shuntCompensatorNotFound")
                        .withUntypedValue("id", m.getShuntCompensatorId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else if (m.getSectionCount() != null || m.getConnect() != null || m.getTargetV() != null) {
                List<ReportNode> reportsShunt = new ArrayList<>();
                final int currentSectionCount = shuntCompensator.getSectionCount();
                final Terminal shuntCompensatorTerminal = shuntCompensator.getTerminal();
                if (shuntCompensatorTerminal.isConnected()) {  // shunt compensator is connected
                    if (m.getSectionCount() == null) {
                        reportsShunt.add(ReportNode.newRootReportNode()
                                .withAllResourceBundlesFromClasspath()
                                .withMessageTemplate("network.modification.shuntCompensatorSectionCountUndefined")
                                .withSeverity(TypedValue.WARN_SEVERITY)
                                .build());
                    } else {
                        if (m.getSectionCount() == 0) {
                            shuntCompensatorTerminal.disconnect();
                            reportsShunt.add(ReportNode.newRootReportNode()
                                    .withAllResourceBundlesFromClasspath()
                                    .withMessageTemplate("network.modification.shuntCompensatorDisconnected")
                                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                                    .build());
                        }
                        if (m.getSectionCount() != currentSectionCount) {
                            shuntCompensator.setSectionCount(m.getSectionCount());
                            reportsShunt.add(ModificationUtils.buildModificationReport(currentSectionCount, m.getSectionCount(), SECTION_COUNT, TypedValue.DETAIL_SEVERITY));
                        }
                    }
                } else {  // shunt compensator is disconnected
                    if (m.getConnect() == null) {
                        reportsShunt.add(ReportNode.newRootReportNode()
                                .withAllResourceBundlesFromClasspath()
                                .withMessageTemplate("network.modification.shuntCompensatorConnectUndefined")
                                .withSeverity(TypedValue.WARN_SEVERITY)
                                .build());
                    } else {
                        if (Boolean.TRUE.equals(m.getConnect())) {
                            shuntCompensatorTerminal.connect();
                            reportsShunt.add(ReportNode.newRootReportNode()
                                    .withAllResourceBundlesFromClasspath()
                                    .withMessageTemplate("network.modification.shuntCompensatorReconnected")
                                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                                    .build());
                        }
                        if (m.getSectionCount() != currentSectionCount) {
                            shuntCompensator.setSectionCount(m.getSectionCount());
                            reportsShunt.add(ModificationUtils.buildModificationReport(currentSectionCount, m.getSectionCount(), SECTION_COUNT, TypedValue.DETAIL_SEVERITY));
                        }
                    }
                }
                if (m.getTargetV() != null) {
                    final double oldTargetV = shuntCompensator.getTargetV();
                    shuntCompensator.setTargetV(m.getTargetV());
                    reportsShunt.add(ModificationUtils.buildModificationReport(oldTargetV, m.getTargetV(), VOLTAGE_SET_POINT, TypedValue.DETAIL_SEVERITY));
                }
                if (!reportsShunt.isEmpty()) {
                    modificationsCount++;
                    reports.add(ReportNode.newRootReportNode()
                            .withAllResourceBundlesFromClasspath()
                            .withMessageTemplate("network.modification.shuntCompensatorModification")
                            .withUntypedValue("id", m.getShuntCompensatorId())
                            .withSeverity(TypedValue.DETAIL_SEVERITY)
                            .build());
                    reports.addAll(reportsShunt);
                }
            }
        }
        if (!reports.isEmpty()) {
            ReportNode shuntCompensatorsReporter = subReportNode.newReportNode().withMessageTemplate(SHUNT_COMPENSATORS_KEY).add();
            reports.forEach(report -> insertReportNode(shuntCompensatorsReporter, report));
        }
        if (modificationsCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.shuntCompensatorModificationsResume")
                    .withUntypedValue(COUNT, modificationsCount)
                    .withTypedValue(ReportConstants.SEVERITY_KEY, TypedValue.INFO_SEVERITY.toString(), TypedValue.SEVERITY)
                    .add();
        }
    }
}
