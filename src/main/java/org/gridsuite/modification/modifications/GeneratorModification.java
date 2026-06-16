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
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
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
@NoArgsConstructor
@Getter
public class GeneratorModification extends AbstractInjectionModification {

    private static final String LIMITS = "network.modification.limits";
    private static final String ACTIVE_LIMITS = "network.modification.ActiveLimits";
    private static final String SETPOINTS = "network.modification.Setpoints";
    private static final String TARGET_VOLTAGE = "Target Voltage";
    public static final String ERROR_MESSAGE = "Generator '%s' : ";
    private AttributeModification<EnergySource> energySource;
    private AttributeModification<Double> minP;
    private AttributeModification<Double> maxP;
    private AttributeModification<Double> ratedS;
    private AttributeModification<Double> targetP;
    private AttributeModification<Double> targetQ;
    private AttributeModification<Boolean> voltageRegulationOn;
    private AttributeModification<Double> targetV;
    private AttributeModification<Double> plannedActivePowerSetPoint;
    private AttributeModification<Double> marginalCost;
    private AttributeModification<Double> plannedOutageRate;
    private AttributeModification<Double> forcedOutageRate;
    private AttributeModification<Double> minQ;
    private AttributeModification<Double> maxQ;
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;
    private AttributeModification<Boolean> participate;
    private AttributeModification<Float> droop;
    private AttributeModification<Double> directTransX;
    private AttributeModification<Double> stepUpTransformerX;
    private AttributeModification<VoltageRegulationType> voltageRegulationType;
    private AttributeModification<String> regulatingTerminalId;
    private AttributeModification<String> regulatingTerminalType;
    private AttributeModification<String> regulatingTerminalVlId;
    private AttributeModification<Double> qPercent;
    private AttributeModification<Boolean> reactiveCapabilityCurve;

    @Builder
    public GeneratorModification(String equipmentId,
                                 List<FreePropertyInfos> properties,
                                 AttributeModification<String> equipmentName,
                                 AttributeModification<String> voltageLevelId,
                                 AttributeModification<String> busOrBusbarSectionId,
                                 AttributeModification<String> connectionName,
                                 AttributeModification<ConnectablePosition.Direction> connectionDirection,
                                 AttributeModification<Integer> connectionPosition,
                                 AttributeModification<Boolean> terminalConnected,
                                 AttributeModification<Double> pMeasurementValue,
                                 AttributeModification<Boolean> pMeasurementValidity,
                                 AttributeModification<Double> qMeasurementValue,
                                 AttributeModification<Boolean> qMeasurementValidity,
                                 AttributeModification<EnergySource> energySource,
                                 AttributeModification<Double> minP,
                                 AttributeModification<Double> maxP,
                                 AttributeModification<Double> ratedS,
                                 AttributeModification<Double> targetP,
                                 AttributeModification<Double> targetQ,
                                 AttributeModification<Boolean> voltageRegulationOn,
                                 AttributeModification<Double> targetV,
                                 AttributeModification<Double> plannedActivePowerSetPoint,
                                 AttributeModification<Double> marginalCost,
                                 AttributeModification<Double> plannedOutageRate,
                                 AttributeModification<Double> forcedOutageRate,
                                 AttributeModification<Double> minQ,
                                 AttributeModification<Double> maxQ,
                                 List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints,
                                 AttributeModification<Boolean> participate,
                                 AttributeModification<Float> droop,
                                 AttributeModification<Double> directTransX,
                                 AttributeModification<Double> stepUpTransformerX,
                                 AttributeModification<VoltageRegulationType> voltageRegulationType,
                                 AttributeModification<String> regulatingTerminalId,
                                 AttributeModification<String> regulatingTerminalType,
                                 AttributeModification<String> regulatingTerminalVlId,
                                 AttributeModification<Double> qPercent,
                                 AttributeModification<Boolean> reactiveCapabilityCurve) {
        this.equipmentId = equipmentId;
        this.properties = properties;
        this.equipmentName = equipmentName;
        this.voltageLevelId = voltageLevelId;
        this.busOrBusbarSectionId = busOrBusbarSectionId;
        this.connectionName = connectionName;
        this.connectionDirection = connectionDirection;
        this.connectionPosition = connectionPosition;
        this.terminalConnected = terminalConnected;
        this.pMeasurementValue = pMeasurementValue;
        this.pMeasurementValidity = pMeasurementValidity;
        this.qMeasurementValue = qMeasurementValue;
        this.qMeasurementValidity = qMeasurementValidity;
        this.energySource = energySource;
        this.minP = minP;
        this.maxP = maxP;
        this.ratedS = ratedS;
        this.targetP = targetP;
        this.targetQ = targetQ;
        this.voltageRegulationOn = voltageRegulationOn;
        this.targetV = targetV;
        this.plannedActivePowerSetPoint = plannedActivePowerSetPoint;
        this.marginalCost = marginalCost;
        this.plannedOutageRate = plannedOutageRate;
        this.forcedOutageRate = forcedOutageRate;
        this.minQ = minQ;
        this.maxQ = maxQ;
        this.reactiveCapabilityCurvePoints = reactiveCapabilityCurvePoints;
        this.participate = participate;
        this.droop = droop;
        this.directTransX = directTransX;
        this.stepUpTransformerX = stepUpTransformerX;
        this.voltageRegulationType = voltageRegulationType;
        this.regulatingTerminalId = regulatingTerminalId;
        this.regulatingTerminalType = regulatingTerminalType;
        this.regulatingTerminalVlId = regulatingTerminalVlId;
        this.qPercent = qPercent;
        this.reactiveCapabilityCurve = reactiveCapabilityCurve;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (equipmentId == null) {
            throw new NetworkModificationException(MODIFY_GENERATOR_ERROR, "Missing required attributes to modify the equipment");
        }
        Generator generator = ModificationUtils.getInstance().getGenerator(network, equipmentId);
        // check min max reactive limits
        String errorMessage = "Generator '" + equipmentId + "' : ";
        ModificationUtils.getInstance().checkVoltageLevelModification(network, voltageLevelId,
                busOrBusbarSectionId, generator.getTerminal());
        ModificationUtils.getInstance().checkReactiveLimit(generator, minQ, maxQ,
                reactiveCapabilityCurvePoints, MODIFY_GENERATOR_ERROR, errorMessage);
        // check regulated terminal
        ModificationUtils.getInstance().checkEnableRegulation(
            voltageRegulationType,
            regulatingTerminalId,
            regulatingTerminalType,
            regulatingTerminalVlId,
            generator.getTerminal(),
            generator.getRegulatingTerminal(),
            network,
            MODIFY_GENERATOR_ERROR,
            errorMessage);
        checkActivePowerZeroOrBetweenMinAndMaxActivePowerGenerator(generator, MODIFY_GENERATOR_ERROR, errorMessage);
        if (ratedS != null) {
            checkIsNotNegativeValue(errorMessage, ratedS.getValue(), MODIFY_GENERATOR_ERROR, "Rated apparent power");
        }
        if (droop != null) {
            checkIsPercentage(errorMessage, droop.getValue(), MODIFY_GENERATOR_ERROR, "Droop");
        }
        if (targetV != null) {
            checkIsNotNegativeValue(errorMessage, targetV.getValue(), MODIFY_GENERATOR_ERROR, TARGET_VOLTAGE);
        }
        checkPowerValues(errorMessage, generator);
    }

    private void checkPowerValues(String errorMessage, Generator generator) {
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        Double oldValue = generatorStartup != null && !Double.isNaN(generatorStartup.getPlannedActivePowerSetpoint())
            ? generatorStartup.getPlannedActivePowerSetpoint() : null;
        double newMinP = minP != null ? minP.getValue() : generator.getMinP();
        double newMaxP = maxP != null ? maxP.getValue() : generator.getMaxP();
        double newTargetP = targetP != null ? targetP.getValue() : generator.getTargetP();
        Double plannedActivePowerSetPointValue = plannedActivePowerSetPoint != null ?
            plannedActivePowerSetPoint.applyModification(oldValue) : null;

        if (plannedActivePowerSetPoint != null) {
            checkActivePowerValue(errorMessage, FIELD_PLANNED_ACTIVE_POWER_SET_POINT, plannedActivePowerSetPointValue, newMinP, newMaxP, MODIFY_GENERATOR_ERROR);
        }
        if (maxP != null) {
            checkMaximumActivePower(errorMessage, newMinP, newTargetP, plannedActivePowerSetPointValue, newMaxP, MODIFY_GENERATOR_ERROR);
        }
        if (minP != null) {
            checkMinimumActivePower(errorMessage, newMaxP, newTargetP, plannedActivePowerSetPointValue, newMinP, MODIFY_GENERATOR_ERROR);
        }
    }

    private void checkActivePowerZeroOrBetweenMinAndMaxActivePowerGenerator(Generator generator, NetworkModificationException.Type exceptionType,
            String errorMessage) {
        ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                targetP,
                minP,
                maxP,
                generator.getMinP(),
                generator.getMaxP(),
                generator.getTargetP(),
                exceptionType,
                errorMessage
        );
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Generator generator = ModificationUtils.getInstance().getGenerator(network, equipmentId);
        // modify the generator in the network
        modifyGenerator(generator, subReportNode);
    }

    @Override
    public String getName() {
        return "GeneratorModification";
    }

    private void modifyGenerator(Generator generator, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.generatorModification")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (equipmentName != null && equipmentName.getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(generator::setName, () -> generator.getOptionalName().orElse("No value"), equipmentName, subReportNode,
                    "Name");
        }
        ModificationUtils.getInstance().applyElementaryModifications(generator::setEnergySource, generator::getEnergySource, energySource, subReportNode, "Energy source");
        modifyGeneratorVoltageLevelBusOrBusBarSectionAttributes(generator, subReportNode);
        modifyGeneratorConnectivityAttributes(generator, subReportNode);
        modifyGeneratorLimitsAttributes(generator, subReportNode);
        modifyGeneratorSetpointsAttributes(generator, subReportNode);
        ModificationUtils.getInstance().modifyShortCircuitExtension(directTransX,
                stepUpTransformerX,
                generator.getExtension(GeneratorShortCircuit.class),
                () -> generator.newExtension(GeneratorShortCircuitAdder.class),
                subReportNode);
        modifyGeneratorStartUpAttributes(generator, subReportNode);
        updateMeasurements(generator, subReportNode);
        PropertiesUtils.applyProperties(generator, subReportNode, properties, "network.modification.GeneratorProperties");
    }

    private void modifyGeneratorReactiveCapabilityCurvePoints(Generator generator, ReportNode subReportNode, ReportNode subReportNodeLimits) {
        ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
        Collection<ReactiveCapabilityCurve.Point> points = generator.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? generator.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints(
                ) : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, reactiveCapabilityCurvePoints, adder, subReportNode, subReportNodeLimits);
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

    private void modifyGeneratorReactiveLimitsAttributes(Generator generator, ReportNode subReportNode, ReportNode subReportNodeLimits) {
        // if reactive capability curve is true and there was modifications on the
        // reactive capability curve points,
        // then we have to apply the reactive capability curve modifications
        // else if reactive capability curve is false we have to apply the min and max
        // reactive limits modifications
        if (reactiveCapabilityCurve != null) {
            if (Boolean.TRUE.equals(reactiveCapabilityCurve.getValue()
                    && reactiveCapabilityCurvePoints != null
                    && !reactiveCapabilityCurvePoints.isEmpty())) {
                modifyGeneratorReactiveCapabilityCurvePoints(generator, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(reactiveCapabilityCurve.getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(minQ, maxQ, generator, subReportNode, subReportNodeLimits);
            }
        }
    }

    private ReportNode modifyGeneratorActivePowerControlAttributes(Generator generator, ReportNode subReportNode, ReportNode subReportNodeSetpoints) {
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        ActivePowerControlAdder<Generator> activePowerControlAdder = generator.newExtension(ActivePowerControlAdder.class);

        return ModificationUtils.getInstance().modifyActivePowerControlAttributes(activePowerControl, activePowerControlAdder,
            participate, droop, subReportNode, subReportNodeSetpoints,
            MODIFY_GENERATOR_ERROR, String.format(ERROR_MESSAGE, equipmentId));
    }

    private void modifyGeneratorStartUpAttributes(Generator generator, ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        modifyGeneratorStartUpAttributes(plannedActivePowerSetPoint,
                marginalCost,
                plannedOutageRate,
                forcedOutageRate,
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

    private static boolean addForcedOutageRate(AttributeModification<Double> forcedOutageRate, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup,
            List<ReportNode> reports) {
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

    private static boolean addPlannedOutageRate(AttributeModification<Double> plannedOutageRate, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup,
            List<ReportNode> reports) {
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

    private void modifyGeneratorRegulatingTerminal(Generator generator, List<ReportNode> modificationReports) {
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

        if (regulatingTerminalId != null
                && regulatingTerminalType != null
                && regulatingTerminalVlId != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(generator.getNetwork(),
                    regulatingTerminalId.getValue(),
                    regulatingTerminalType.getValue(),
                    regulatingTerminalVlId.getValue());
            generator.setRegulatingTerminal(terminal);

            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                    regulatingTerminalVlId.getValue(),
                    "Voltage level"));
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    regulatingTerminalType.getValue() + ":"
                            + regulatingTerminalId.getValue(),
                    "Equipment"));
        }

        // if the voltageRegulationType is set to LOCAL, we set the regulatingTerminal
        // to null
        if (voltageRegulationType != null
                && voltageRegulationType.getValue() == VoltageRegulationType.LOCAL
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

    private ReportNode modifyGeneratorVoltageRegulatorAttributes(Generator generator, ReportNode subReportNode, ReportNode subReportNodeSetpoints) {
        List<ReportNode> voltageRegulationReports = new ArrayList<>();

        ReportNode reportVoltageSetpoint = modifyTargetV(generator, targetV);
        if (reportVoltageSetpoint != null) {
            voltageRegulationReports.add(reportVoltageSetpoint);
        }
        // must be done after setting targetV
        ReportNode voltageRegulationOn = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setVoltageRegulatorOn, generator::isVoltageRegulatorOn,
                this.voltageRegulationOn, "VoltageRegulationOn");
        if (voltageRegulationOn != null) {
            voltageRegulationReports.add(voltageRegulationOn);
        }

        // We apply modifications to regulatingTerminal and QPercent
        // we apply modifications to the reactivepower setpoint
        modifyGeneratorRegulatingTerminal(generator, voltageRegulationReports);

        if (qPercent != null) {
            CoordinatedReactiveControl coordinatedReactiveControl = generator
                    .getExtension(CoordinatedReactiveControl.class);
            Double oldQPercent = coordinatedReactiveControl != null ? coordinatedReactiveControl.getQPercent()
                    : Double.NaN;
            generator.newExtension(CoordinatedReactiveControlAdder.class)
                    .withQPercent(qPercent.getValue())
                    .add();
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldQPercent,
                    qPercent.getValue(), "Reactive percentage"));
        }

        //TargetQ and TargetV are unset after voltage regulation have been dealt with otherwise it can cause unwanted validations exceptions
        if (targetV != null && targetV.getOp() == OperationType.UNSET) {
            generator.setTargetV(Double.NaN);
        }

        if (targetQ != null && targetQ.getOp() == OperationType.UNSET) {
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

    private void modifyGeneratorSetpointsAttributes(Generator generator, ReportNode subReportNode) {
        ReportNode reportActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetP, generator::getTargetP, targetP,
                "Active power");

        ReportNode reportReactivePower = modifyTargetQ(generator, targetQ);

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
        subReporterSetpoints = modifyGeneratorVoltageRegulatorAttributes(generator, subReportNode, subReporterSetpoints);
        modifyGeneratorActivePowerControlAttributes(generator, subReportNode, subReporterSetpoints);
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

    private void modifyGeneratorLimitsAttributes(Generator generator, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = modifyGeneratorActiveLimitsAttributes(maxP,
                minP,
                ratedS,
                generator,
                subReportNode);
        modifyGeneratorReactiveLimitsAttributes(generator, subReportNode, subReportNodeLimits);
    }

    private void modifyGeneratorVoltageLevelBusOrBusBarSectionAttributes(Generator generator, ReportNode subReportNode) {
        ModificationUtils.getInstance().moveFeederBay(
                generator, generator.getTerminal(),
                voltageLevelId,
                busOrBusbarSectionId,
                subReportNode
        );
    }

    private ReportNode modifyGeneratorConnectivityAttributes(Generator generator, ReportNode subReportNode) {
        ConnectablePosition<Generator> connectablePosition = generator.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Generator> connectablePositionAdder = generator.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, generator,
                equipmentId, voltageLevelId, busOrBusbarSectionId, connectionName, connectionDirection, connectionPosition, terminalConnected, subReportNode);
    }
}
