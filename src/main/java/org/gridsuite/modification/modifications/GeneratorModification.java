/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class GeneratorModification extends AbstractModification {

    private static final String LIMITS = "network.modification.limits";
    private static final String ACTIVE_LIMITS = "network.modification.ActiveLimits";
    private static final String SETPOINTS = "network.modification.Setpoints";
    private static final String TARGET_VOLTAGE = "Target Voltage";
    public static final String ERROR_MESSAGE = "Generator '%s' : ";

    private final GeneratorModificationInfos modificationInfos;

    public GeneratorModification(GeneratorModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(MODIFY_GENERATOR_ERROR, "Missing required attributes to modify the equipment");
        }
        Generator generator = ModificationUtils.getInstance().getGenerator(network, modificationInfos.getEquipmentId());
        // check min max reactive limits
        String errorMessage = "Generator '" + modificationInfos.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().checkVoltageLevelModification(network, modificationInfos.getVoltageLevelId(),
                modificationInfos.getBusOrBusbarSectionId(), generator.getTerminal());
        ModificationUtils.getInstance().checkReactiveLimit(generator, modificationInfos.getMinQ(), modificationInfos.getMaxQ(),
                modificationInfos.getReactiveCapabilityCurvePoints(), MODIFY_GENERATOR_ERROR, errorMessage);
        // check regulated terminal
        ModificationUtils.getInstance().checkEnableRegulation(
            modificationInfos.getVoltageRegulationType(),
            modificationInfos.getRegulatingTerminalId(),
            modificationInfos.getRegulatingTerminalType(),
            modificationInfos.getRegulatingTerminalVlId(),
            generator.getTerminal(),
            generator.getRegulatingTerminal(),
            network,
            MODIFY_GENERATOR_ERROR,
            errorMessage);
        checkActivePowerZeroOrBetweenMinAndMaxActivePowerGenerator(modificationInfos, generator, MODIFY_GENERATOR_ERROR, errorMessage);
        if (modificationInfos.getRatedS() != null) {
            checkIsNotNegativeValue(errorMessage, modificationInfos.getRatedS().getValue(), MODIFY_GENERATOR_ERROR, "Rated apparent power");
        }
        if (modificationInfos.getDroop() != null) {
            checkIsPercentage(errorMessage, modificationInfos.getDroop().getValue(), MODIFY_GENERATOR_ERROR, "Droop");
        }
        if (modificationInfos.getTargetV() != null) {
            checkIsNotNegativeValue(errorMessage, modificationInfos.getTargetV().getValue(), MODIFY_GENERATOR_ERROR, TARGET_VOLTAGE);
        }
    }

    private void checkActivePowerZeroOrBetweenMinAndMaxActivePowerGenerator(GeneratorModificationInfos modificationInfos, Generator generator, NetworkModificationException.Type exceptionType, String errorMessage) {
        ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                modificationInfos.getTargetP(),
                modificationInfos.getMinP(),
                modificationInfos.getMaxP(),
                generator.getMinP(),
                generator.getMaxP(),
                generator.getTargetP(),
                exceptionType,
                errorMessage
        );
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Generator generator = ModificationUtils.getInstance().getGenerator(network, modificationInfos.getEquipmentId());
        // modify the generator in the network
        modifyGenerator(generator, modificationInfos, subReportNode);
    }

    @Override
    public String getName() {
        return "GeneratorModification";
    }

    private void modifyGenerator(Generator generator, GeneratorModificationInfos modificationInfos, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.generatorModification")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(generator::setName, () -> generator.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");
        }
        ModificationUtils.getInstance().applyElementaryModifications(generator::setEnergySource, generator::getEnergySource, modificationInfos.getEnergySource(), subReportNode, "Energy source");
        modifyGeneratorVoltageLevelBusOrBusBarSectionAttributes(modificationInfos, generator, subReportNode);
        modifyGeneratorConnectivityAttributes(modificationInfos, generator, subReportNode);
        modifyGeneratorLimitsAttributes(modificationInfos, generator, subReportNode);
        modifyGeneratorSetpointsAttributes(modificationInfos, generator, subReportNode);
        ModificationUtils.getInstance().modifyShortCircuitExtension(modificationInfos.getDirectTransX(),
                modificationInfos.getStepUpTransformerX(),
                generator.getExtension(GeneratorShortCircuit.class),
                () -> generator.newExtension(GeneratorShortCircuitAdder.class),
                subReportNode);
        modifyGeneratorStartUpAttributes(modificationInfos, generator, subReportNode);
        PropertiesUtils.applyProperties(generator, subReportNode, modificationInfos.getProperties(), "network.modification.GeneratorProperties");
    }

    private void modifyGeneratorReactiveCapabilityCurvePoints(GeneratorModificationInfos modificationInfos,
                                                              Generator generator, ReportNode subReportNode, ReportNode subReportNodeLimits) {
        ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurvePointsInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = generator.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? generator.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReportNode, subReportNodeLimits);
    }

    public static ReportNode modifyGeneratorActiveLimitsAttributes(AttributeModification<Double> maxP,
                                                                   AttributeModification<Double> minP,
                                                                   AttributeModification<Double> ratedS,
                                                                   Generator generator,
                                                                   ReportNode subReportNode) {
        ReportNode subReporterLimits = null;
        ReportNode reportMaxActivePower;
        ReportNode reportMinActivePower;

        if (maxP != null && maxP.getValue() > generator.getMinP()) {
            reportMaxActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMaxP, generator::getMaxP, maxP, "Max active power");
            reportMinActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMinP, generator::getMinP, minP, "Min active power");

        } else {
            reportMinActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMinP, generator::getMinP, minP, "Min active power");
            reportMaxActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMaxP, generator::getMaxP, maxP, "Max active power");
        }
        ReportNode reportRatedNominalPower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setRatedS, generator::getRatedS, ratedS, "Rated nominal power");
        if (subReportNode != null && (reportMaxActivePower != null || reportMinActivePower != null || reportRatedNominalPower != null)) {
            subReporterLimits = subReportNode.newReportNode().withMessageTemplate(LIMITS).add();
            ReportNode subReporterActiveLimits = subReporterLimits.newReportNode().withMessageTemplate(ACTIVE_LIMITS).add();
            if (reportMaxActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMaxActivePower);
            }
            if (reportMinActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMinActivePower);
            }
            if (reportRatedNominalPower != null) {
                insertReportNode(subReporterActiveLimits, reportRatedNominalPower);
            }
        }
        return subReporterLimits;
    }

    private void modifyGeneratorReactiveLimitsAttributes(GeneratorModificationInfos modificationInfos,
                                                         Generator generator, ReportNode subReportNode, ReportNode subReportNodeLimits) {
        // if reactive capability curve is true and there was modifications on the
        // reactive capability curve points,
        // then we have to apply the reactive capability curve modifications
        // else if reactive capability curve is false we have to apply the min and max
        // reactive limits modifications
        if (modificationInfos.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                    && modificationInfos.getReactiveCapabilityCurvePoints() != null
                    && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyGeneratorReactiveCapabilityCurvePoints(modificationInfos, generator, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(modificationInfos.getMinQ(), modificationInfos.getMaxQ(), generator, subReportNode, subReportNodeLimits);
            }
        }
    }

    private ReportNode modifyGeneratorActivePowerControlAttributes(GeneratorModificationInfos modificationInfos,
                                                                 Generator generator, ReportNode subReportNode, ReportNode subReportNodeSetpoints) {
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        ActivePowerControlAdder<Generator> activePowerControlAdder = generator.newExtension(ActivePowerControlAdder.class);

        return ModificationUtils.getInstance().modifyActivePowerControlAttributes(activePowerControl, activePowerControlAdder,
            modificationInfos.getParticipate(), modificationInfos.getDroop(), subReportNode, subReportNodeSetpoints,
            MODIFY_GENERATOR_ERROR, String.format(ERROR_MESSAGE, modificationInfos.getEquipmentId()));
    }

    private void modifyGeneratorStartUpAttributes(GeneratorModificationInfos modificationInfos, Generator generator,
                                                  ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        modifyGeneratorStartUpAttributes(modificationInfos.getPlannedActivePowerSetPoint(),
                modificationInfos.getMarginalCost(),
                modificationInfos.getPlannedOutageRate(),
                modificationInfos.getForcedOutageRate(),
                generator,
                subReportNode,
                reports);
    }

    public static void modifyGeneratorStartUpAttributes(AttributeModification<Double> plannedActivePowerSetPoint,
                                                        AttributeModification<Double> marginalCost,
                                                        AttributeModification<Double> plannedOutageRate,
                                                        AttributeModification<Double> forcedOutageRate,
                                                        Generator generator,
                                                        ReportNode subReportNode,
                                                        List<ReportNode> reports) {
        GeneratorStartupAdder generatorStartupAdder = generator.newExtension(GeneratorStartupAdder.class);
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        boolean plannedActivePowerSetPointUpdated = addPlannedActivePowerSetPoint(plannedActivePowerSetPoint,
                generatorStartupAdder,
                generatorStartup,
                reports);
        boolean marginalCostUpdated = addMarginalCost(marginalCost, generatorStartupAdder, generatorStartup, reports);
        boolean plannedOutageRateUpdated = addPlannedOutageRate(plannedOutageRate, generatorStartupAdder, generatorStartup, reports);
        boolean forcedOutageRateUpdated = addForcedOutageRate(forcedOutageRate, generatorStartupAdder, generatorStartup, reports);

        if (plannedActivePowerSetPointUpdated ||
                marginalCostUpdated ||
                plannedOutageRateUpdated ||
                forcedOutageRateUpdated) {
            generatorStartupAdder.add();
            if (subReportNode != null) {
                ModificationUtils.getInstance().reportModifications(subReportNode, reports, "network.modification.startUpAttributesModified");
            }
        }
    }

    private static boolean addForcedOutageRate(AttributeModification<Double> forcedOutageRate, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<ReportNode> reports) {
        Double oldForcedOutageRate = generatorStartup != null ? generatorStartup.getForcedOutageRate() : Double.NaN;
        if (forcedOutageRate != null) {
            generatorStartupAdder
                    .withForcedOutageRate(forcedOutageRate.getValue());
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldForcedOutageRate,
                        forcedOutageRate.getValue(),
                        "Forced outage rate"));
            }
            return true;
        } else {
            generatorStartupAdder
                    .withForcedOutageRate(oldForcedOutageRate);
        }
        return false;
    }

    private static boolean addPlannedOutageRate(AttributeModification<Double> plannedOutageRate, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<ReportNode> reports) {
        Double oldPlannedOutageRate = generatorStartup != null ? generatorStartup.getPlannedOutageRate() : Double.NaN;
        if (plannedOutageRate != null) {
            generatorStartupAdder
                    .withPlannedOutageRate(plannedOutageRate.getValue());
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldPlannedOutageRate,
                        plannedOutageRate.getValue(),
                        "Planning outage rate"));
            }
            return true;
        } else {
            generatorStartupAdder
                    .withPlannedOutageRate(oldPlannedOutageRate);
        }
        return false;
    }

    private static boolean addMarginalCost(AttributeModification<Double> marginalCost, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<ReportNode> reports) {
        Double oldMarginalCost = generatorStartup != null ? generatorStartup.getMarginalCost() : Double.NaN;
        if (marginalCost != null) {
            generatorStartupAdder
                    .withMarginalCost(marginalCost.getValue());
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldMarginalCost,
                        marginalCost.getValue(),
                        "Marginal cost"));
            }
            return true;
        } else {
            generatorStartupAdder
                    .withMarginalCost(oldMarginalCost);
        }
        return false;
    }

    private static boolean addPlannedActivePowerSetPoint(AttributeModification<Double> plannedActivePowerSetPoint, GeneratorStartupAdder generatorStartupAdder,
                                                  GeneratorStartup generatorStartup, List<ReportNode> reports) {
        Double oldPlannedActivePowerSetPoint = generatorStartup != null ? generatorStartup.getPlannedActivePowerSetpoint() : Double.NaN;
        if (plannedActivePowerSetPoint != null) {
            generatorStartupAdder
                    .withPlannedActivePowerSetpoint(plannedActivePowerSetPoint.getValue());
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldPlannedActivePowerSetPoint,
                        plannedActivePowerSetPoint.getValue(),
                        "Planning active power set point"));
            }
            return true;
        } else {
            generatorStartupAdder
                    .withPlannedActivePowerSetpoint(oldPlannedActivePowerSetPoint);
        }
        return false;
    }

    private void modifyGeneratorRegulatingTerminal(GeneratorModificationInfos modificationInfos, Generator generator, List<ReportNode> modificationReports) {
        Terminal regulatingTerminal = generator.getRegulatingTerminal();

        String oldVoltageLevel = null;
        String oldEquipment = null;
        // If there is no regulating terminal in file, regulating terminal voltage level
        // is equal to generator voltage level
        if (regulatingTerminal != null
                && !regulatingTerminal.getVoltageLevel().equals(generator.getTerminal().getVoltageLevel())) {
            oldVoltageLevel = regulatingTerminal.getVoltageLevel().getId();
            oldEquipment = regulatingTerminal.getConnectable().getType().name() + ":"
                    + regulatingTerminal.getConnectable().getId();
        }

        if (modificationInfos.getRegulatingTerminalId() != null
                && modificationInfos.getRegulatingTerminalType() != null
                && modificationInfos.getRegulatingTerminalVlId() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(generator.getNetwork(),
                    modificationInfos.getRegulatingTerminalId().getValue(),
                    modificationInfos.getRegulatingTerminalType().getValue(),
                    modificationInfos.getRegulatingTerminalVlId().getValue());
            generator.setRegulatingTerminal(terminal);

            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                    modificationInfos.getRegulatingTerminalVlId().getValue(),
                    "Voltage level"));
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    modificationInfos.getRegulatingTerminalType().getValue() + ":"
                            + modificationInfos.getRegulatingTerminalId().getValue(),
                    "Equipment"));
        }

        // if the voltageRegulationType is set to LOCAL, we set the regulatingTerminal
        // to null
        if (modificationInfos.getVoltageRegulationType() != null
                && modificationInfos.getVoltageRegulationType().getValue() == VoltageRegulationType.LOCAL
                && oldEquipment != null && oldVoltageLevel != null) {
            generator.setRegulatingTerminal(null);
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                    null,
                    "Voltage level"));
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    null,
                    "Equipment"));
        }
    }

    private ReportNode modifyGeneratorVoltageRegulatorAttributes(GeneratorModificationInfos modificationInfos,
                                                               Generator generator, ReportNode subReportNode, ReportNode subReportNodeSetpoints) {
        List<ReportNode> voltageRegulationReports = new ArrayList<>();

        ReportNode reportVoltageSetpoint = modifyTargetV(generator, modificationInfos.getTargetV());
        if (reportVoltageSetpoint != null) {
            voltageRegulationReports.add(reportVoltageSetpoint);
        }
        // must be done after setting targetV
        ReportNode voltageRegulationOn = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setVoltageRegulatorOn, generator::isVoltageRegulatorOn,
                modificationInfos.getVoltageRegulationOn(), "VoltageRegulationOn");
        if (voltageRegulationOn != null) {
            voltageRegulationReports.add(voltageRegulationOn);
        }

        // We apply modifications to regulatingTerminal and QPercent
        // we apply modifications to the reactivepower setpoint
        modifyGeneratorRegulatingTerminal(modificationInfos, generator, voltageRegulationReports);

        if (modificationInfos.getQPercent() != null) {
            CoordinatedReactiveControl coordinatedReactiveControl = generator
                    .getExtension(CoordinatedReactiveControl.class);
            Double oldQPercent = coordinatedReactiveControl != null ? coordinatedReactiveControl.getQPercent()
                    : Double.NaN;
            generator.newExtension(CoordinatedReactiveControlAdder.class)
                    .withQPercent(modificationInfos.getQPercent().getValue())
                    .add();
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldQPercent,
                    modificationInfos.getQPercent().getValue(), "Reactive percentage"));
        }

        //TargetQ and TargetV are unset after voltage regulation have been dealt with otherwise it can cause unwanted validations exceptions
        if (modificationInfos.getTargetV() != null && modificationInfos.getTargetV().getOp() == OperationType.UNSET) {
            generator.setTargetV(Double.NaN);
        }

        if (modificationInfos.getTargetQ() != null && modificationInfos.getTargetQ().getOp() == OperationType.UNSET) {
            generator.setTargetQ(Double.NaN);
        }

        ReportNode subReportNodeSetpoints2 = subReportNodeSetpoints;
        if (subReportNodeSetpoints == null && !voltageRegulationReports.isEmpty()) {
            subReportNodeSetpoints2 = subReportNode.newReportNode()
                            .withMessageTemplate(SETPOINTS)
                            .add();
        }
        ModificationUtils.getInstance().reportModifications(subReportNodeSetpoints2, voltageRegulationReports, "network.modification.voltageRegulationModified");
        return subReportNodeSetpoints2;
    }

    public static ReportNode modifyTargetV(Generator generator, AttributeModification<Double> modifTargetV) {
        ReportNode reportVoltageSetpoint = null;
        if (modifTargetV != null) {
            if (modifTargetV.getOp() == OperationType.SET) {
                // we always keep the equivalent local target voltage in the network
                reportVoltageSetpoint = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                        v -> generator.setTargetV(v, generator.getEquivalentLocalTargetV()),
                        generator::getTargetV,
                        modifTargetV, TARGET_VOLTAGE);
            } else {
                reportVoltageSetpoint = ModificationUtils.getInstance().buildModificationReport(generator.getTargetV(), Double.NaN, TARGET_VOLTAGE);
            }
        }
        return reportVoltageSetpoint;
    }

    private void modifyGeneratorSetpointsAttributes(GeneratorModificationInfos modificationInfos,
                                                    Generator generator, ReportNode subReportNode) {
        ReportNode reportActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetP, generator::getTargetP, modificationInfos.getTargetP(), "Active power");

        ReportNode reportReactivePower = modifyTargetQ(generator, modificationInfos.getTargetQ());

        ReportNode subReporterSetpoints = null;
        if (reportActivePower != null || reportReactivePower != null) {
            subReporterSetpoints = subReportNode.newReportNode().withMessageTemplate(SETPOINTS).add();
            if (reportActivePower != null) {
                insertReportNode(subReporterSetpoints, reportActivePower);
            }
            if (reportReactivePower != null) {
                insertReportNode(subReporterSetpoints, reportReactivePower);
            }
        }
        subReporterSetpoints = modifyGeneratorVoltageRegulatorAttributes(modificationInfos, generator, subReportNode, subReporterSetpoints);
        modifyGeneratorActivePowerControlAttributes(modificationInfos, generator, subReportNode, subReporterSetpoints);
    }

    public static ReportNode modifyTargetQ(Generator generator, AttributeModification<Double> modifTargetQ) {
        ReportNode reportReactivePower = null;
        if (modifTargetQ != null) {
            if (modifTargetQ.getOp() == OperationType.SET) {
                reportReactivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetQ, generator::getTargetQ, modifTargetQ, "Target reactive power");
            } else {
                reportReactivePower = ModificationUtils.getInstance().buildModificationReport(generator.getTargetQ(), Double.NaN, "Target reactive power");
            }
        }
        return reportReactivePower;
    }

    private void modifyGeneratorLimitsAttributes(GeneratorModificationInfos modificationInfos,
                                                 Generator generator, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = modifyGeneratorActiveLimitsAttributes(modificationInfos.getMaxP(),
                modificationInfos.getMinP(),
                modificationInfos.getRatedS(),
                generator,
                subReportNode);
        modifyGeneratorReactiveLimitsAttributes(modificationInfos, generator, subReportNode, subReportNodeLimits);
    }

    private void modifyGeneratorVoltageLevelBusOrBusBarSectionAttributes(GeneratorModificationInfos modificationInfos,
                                                                       Generator generator, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                generator, generator.getTerminal(),
                modificationInfos.getVoltageLevelId(),
                modificationInfos.getBusOrBusbarSectionId(),
                subReportNode
        );
    }

    private ReportNode modifyGeneratorConnectivityAttributes(GeneratorModificationInfos modificationInfos,
                                                             Generator generator, ReportNode subReportNode) {
        ConnectablePosition<Generator> connectablePosition = generator.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Generator> connectablePositionAdder = generator.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, generator, modificationInfos, subReportNode);
    }
}
