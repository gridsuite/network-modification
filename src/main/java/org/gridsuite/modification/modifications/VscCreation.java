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
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRangeAdder;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.ConverterStationCreationModel;
import org.gridsuite.modification.model.VscCreationModel;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

public class VscCreation extends AbstractModification {

    public static final String VSC_SETPOINTS = "network.modification.vscSetPoints";
    public static final String VSC_CHARACTERISTICS = "network.modification.vscCharacteristics";

    private final VscCreationModel modificationModel;

    public VscCreation(VscCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getHvdcLine(modificationModel.getEquipmentId()) != null) {
            throw new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, modificationModel.getEquipmentId());
        }
        String errorMessage = "HVDC vsc '" + modificationModel.getEquipmentId() + "' : ";
        checkConverterStation(network, modificationModel.getConverterStation1());
        checkConverterStation(network, modificationModel.getConverterStation2());
        checkDroop();
        checkIsNotNegativeValue(errorMessage, modificationModel.getR(), CREATE_VSC_ERROR, "Resistance R");
        checkIsNotNegativeValue(errorMessage, modificationModel.getNominalV(), CREATE_VSC_ERROR, "Nominal voltage");
        checkIsNotNegativeValue(errorMessage, modificationModel.getConverterStation1().getVoltageSetpoint(), CREATE_VSC_ERROR, "voltage set point side 1");
        checkIsNotNegativeValue(errorMessage, modificationModel.getConverterStation2().getVoltageSetpoint(), CREATE_VSC_ERROR, "voltage set point side 2");
        checkIsPercentage(errorMessage, modificationModel.getConverterStation1().getLossFactor(), CREATE_VSC_ERROR, "loss factor side 1");
        checkIsPercentage(errorMessage, modificationModel.getConverterStation2().getLossFactor(), CREATE_VSC_ERROR, "loss factor side 2");
    }

    private void checkDroop() {
        boolean isPresentAngleDroopActivePowerControl = modificationModel.getAngleDroopActivePowerControl() != null;
        boolean isPresentDroop = modificationModel.getDroop() != null;
        boolean isPresentP0 = modificationModel.getP0() != null;
        // all fields are provided => OK extension will be created
        if (isPresentAngleDroopActivePowerControl && isPresentDroop && isPresentP0) {
            return;
        }
        // particular case, not enabling extension and others fields are not provided => OK extension will not be created
        if (Boolean.FALSE.equals(modificationModel.getAngleDroopActivePowerControl()) && !isPresentDroop && !isPresentP0) {
            return;
        }
        // at least one field is provided but not for the others => NOT OK
        if (isPresentAngleDroopActivePowerControl || isPresentDroop || isPresentP0) {
            throw new NetworkModificationException(WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL, VscModification.ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG);
        }
        // otherwise, i.e. none of the fields is not provided => OK extension will not be created
    }

    private void checkConverterStation(Network network,
                                       ConverterStationCreationModel converterStation) {
        if (converterStation == null) {
            throw new NetworkModificationException(CREATE_VSC_ERROR, modificationModel.getEquipmentId() + "Missing required converter station");
        }
        // check connectivity
        ModificationUtils.getInstance().controlConnectivity(network,
                converterStation.getVoltageLevelId(),
                converterStation.getBusOrBusbarSectionId()
        );

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(converterStation,
                CREATE_VSC_ERROR,
                modificationModel.getEquipmentId(),
                "Vsc");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VscConverterStation converterStation1 = createConverterStation(network, modificationModel.getConverterStation1(), subReportNode, "1");

        VscConverterStation converterStation2 = createConverterStation(network, modificationModel.getConverterStation2(), subReportNode, "2");

        HvdcLine hvdcLine = network.newHvdcLine()
                .setId(modificationModel.getEquipmentId())
                .setName(modificationModel.getEquipmentName())
                .setNominalV(modificationModel.getNominalV())
                .setR(modificationModel.getR())
                .setMaxP(modificationModel.getMaxP())
                .setActivePowerSetpoint(modificationModel.getActivePowerSetpoint())
                .setConvertersMode(modificationModel.getConvertersMode())
                .setConverterStationId1(converterStation1 != null ? converterStation1.getId() : null)
                .setConverterStationId2(converterStation2 != null ? converterStation2.getId() : null)
                .add();

        if (modificationModel.getOperatorActivePowerLimitFromSide1ToSide2() != null ||
                modificationModel.getOperatorActivePowerLimitFromSide2ToSide1() != null) {
            hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class)
                    .withOprFromCS1toCS2(modificationModel.getOperatorActivePowerLimitFromSide1ToSide2())
                    .withOprFromCS2toCS1(modificationModel.getOperatorActivePowerLimitFromSide2ToSide1())
                    .add();
        }

        if (shouldCreateDroopActivePowerControlExtension()) {
            var activePowerControlExtension = hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class)
                    .withEnabled(modificationModel.getAngleDroopActivePowerControl());
            activePowerControlExtension.withP0(modificationModel.getP0());
            activePowerControlExtension.withDroop(modificationModel.getDroop());

            activePowerControlExtension.add();
        }

        reportHvdcLineModel(subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.vscCreated")
                .withUntypedValue("id", modificationModel.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (modificationModel.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, modificationModel.getEquipmentName(), "Name");
        }

        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationModel.getProperties(), "network.modification.VscProperties");
    }

    @Override
    public String getName() {
        return "VscCreation";
    }

    private boolean shouldCreateDroopActivePowerControlExtension() {
        return VscModification.shouldCreateDroopActivePowerControlExtension(
            modificationModel.getAngleDroopActivePowerControl() != null, modificationModel.getDroop() != null, modificationModel.getP0() != null);
    }

    private void reportHvdcLineModel(ReportNode subReportNode) {
        List<ReportNode> characteristicsReports = new ArrayList<>();
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getNominalV(), "DC nominal voltage"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getR(), "DC resistance"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getMaxP(), "Pmax"));
        ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReports, VSC_CHARACTERISTICS);

        List<ReportNode> limitsReports = new ArrayList<>();
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getOperatorActivePowerLimitFromSide1ToSide2(), "Operator active power limit (Side1 -> Side 2)"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getOperatorActivePowerLimitFromSide2ToSide1(), "Operator active power limit (Side2 -> Side 1)"));
        ModificationUtils.getInstance().reportModifications(subReportNode, limitsReports, "network.modification.vscLimits");

        List<ReportNode> setPointsReports = new ArrayList<>();
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getConvertersMode(), "Converters mode"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getActivePowerSetpoint(), "Active power"));
        ReportNode setPointsReporter = ModificationUtils.getInstance().reportModifications(subReportNode, setPointsReports, VSC_SETPOINTS);

        List<ReportNode> angleDroopActivePowerControlReports = new ArrayList<>();
        angleDroopActivePowerControlReports.add(ModificationUtils.getInstance()
                .createEnabledDisabledReport("network.modification.angleDroopActivePowerControl", modificationModel.getAngleDroopActivePowerControl()));
        if (modificationModel.getP0() != null) {
            angleDroopActivePowerControlReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getP0(), "P0"));
        }
        if (modificationModel.getDroop() != null) {
            angleDroopActivePowerControlReports.add(ModificationUtils.getInstance().buildCreationReport(modificationModel.getDroop(), "Droop"));
        }
        ModificationUtils.getInstance().reportModifications(setPointsReporter, angleDroopActivePowerControlReports, "network.modification.vscAngleDroop");
    }

    private VscConverterStation createConverterStation(Network network,
                                                       ConverterStationCreationModel converterStationCreationModel,
                                                       ReportNode subReportNode,
                                                       String logFieldName) {
        ReportNode converterStationReporter = subReportNode.newReportNode()
            .withMessageTemplate("network.modification.vscConverterStationCreated")
            .withUntypedValue("fieldName", logFieldName)
            .withUntypedValue("id", converterStationCreationModel.getEquipmentId())
            .add();
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, converterStationCreationModel.getVoltageLevelId());
        VscConverterStation vscConverterStation = voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER ?
                createConverterStationInNodeBreaker(network, voltageLevel, converterStationCreationModel, converterStationReporter) :
                createConverterStationInBusBreaker(voltageLevel, converterStationCreationModel, converterStationReporter);

        if (!converterStationCreationModel.isTerminalConnected()) {
            vscConverterStation.getTerminal().disconnect();
        }

        return vscConverterStation;
    }

    private VscConverterStation createConverterStationInNodeBreaker(Network network,
                                                                    VoltageLevel voltageLevel,
                                                                    ConverterStationCreationModel converterStationCreationModel,
                                                                    ReportNode subReportNode) {
        VscConverterStationAdder converterStationAdder = voltageLevel.newVscConverterStation()
                .setId(converterStationCreationModel.getEquipmentId())
                .setName(converterStationCreationModel.getEquipmentName())
                .setVoltageRegulatorOn(converterStationCreationModel.getVoltageRegulationOn());

        if (converterStationCreationModel.getReactivePowerSetpoint() != null) {
            converterStationAdder.setReactivePowerSetpoint(converterStationCreationModel.getReactivePowerSetpoint());
        }

        if (converterStationCreationModel.getLossFactor() != null) {
            converterStationAdder.setLossFactor(converterStationCreationModel.getLossFactor());
        }

        if (converterStationCreationModel.getVoltageSetpoint() != null) {
            converterStationAdder.setVoltageSetpoint(converterStationCreationModel.getVoltageSetpoint());
        }
        createInjectionInNodeBreaker(voltageLevel, converterStationCreationModel, network, converterStationAdder, subReportNode);
        VscConverterStation vscConverterStation = ModificationUtils.getInstance()
                .getVscConverterStation(network, converterStationCreationModel.getEquipmentId());

        addExtensionsAndReports(vscConverterStation,
                converterStationCreationModel,
                subReportNode);

        return vscConverterStation;

    }

    private VscConverterStation createConverterStationInBusBreaker(VoltageLevel voltageLevel,
                                                                   ConverterStationCreationModel converterStationCreationModel,
                                                                   ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, converterStationCreationModel.getBusOrBusbarSectionId());
        VscConverterStation vscConverterStation = voltageLevel.newVscConverterStation()
                .setId(converterStationCreationModel.getEquipmentId())
                .setName(converterStationCreationModel.getEquipmentName())
                .setVoltageRegulatorOn(converterStationCreationModel.getVoltageRegulationOn())
                .setReactivePowerSetpoint(converterStationCreationModel.getReactivePowerSetpoint())
                .setBus(bus.getId())
                .setLossFactor(converterStationCreationModel.getLossFactor())
                .setVoltageSetpoint(converterStationCreationModel.getVoltageSetpoint())
                .add();

        addExtensionsAndReports(vscConverterStation, converterStationCreationModel, subReportNode);

        return vscConverterStation;
    }

    private void addExtensionsAndReports(VscConverterStation vscConverterStation,
                                         ConverterStationCreationModel converterStationCreationModel,
                                         ReportNode subReporter) {
        reportInjectionCreationConnectivity(converterStationCreationModel, subReporter);

        ModificationUtils.getInstance().reportModifications(subReporter,
                List.of(ModificationUtils.getInstance().buildCreationReport(converterStationCreationModel.getLossFactor(), "Loss Factor")),
                "network.modification.converterStationCharacteristics");

        ModificationUtils.getInstance().createReactiveLimits(converterStationCreationModel, vscConverterStation, subReporter);

        reportConverterStationSetPoints(converterStationCreationModel, subReporter);
    }

    private void reportConverterStationSetPoints(ConverterStationCreationModel converterStationCreationModel, ReportNode subReportNode) {
        ReportNode setPointReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.converterStationSetPoint").add();

        if (converterStationCreationModel.getReactivePowerSetpoint() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(setPointReporter,
                    converterStationCreationModel.getReactivePowerSetpoint(),
                    "Reactive power");
        }

        List<ReportNode> setPointsVoltageReports = new ArrayList<>();
        setPointsVoltageReports.add(ModificationUtils.getInstance().createEnabledDisabledReport("network.modification.voltageRegulationOn",
                converterStationCreationModel.getVoltageRegulationOn()));
        if (converterStationCreationModel.getVoltageSetpoint() != null) {
            setPointsVoltageReports.add(ModificationUtils.getInstance().buildCreationReport(converterStationCreationModel.getReactivePowerSetpoint(), "Voltage"));
        }

        ModificationUtils.getInstance().reportModifications(setPointReporter,
                setPointsVoltageReports,
                "network.modification.converterStationSetPointsVoltageRegulation");
    }
}
