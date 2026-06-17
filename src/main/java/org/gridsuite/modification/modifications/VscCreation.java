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
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.modifications.data.VscConverterStationCreation;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.utils.ModificationUtils.*;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@Getter
@Setter
public class VscCreation extends AbstractEquipmentCreation {

    public static final String VSC_SETPOINTS = "network.modification.vscSetPoints";
    public static final String VSC_CHARACTERISTICS = "network.modification.vscCharacteristics";

    private Double nominalV;
    private Double r;
    private Double maxP;
    private Float operatorActivePowerLimitFromSide1ToSide2;
    private Float operatorActivePowerLimitFromSide2ToSide1;
    private HvdcLine.ConvertersMode convertersMode;
    private Double activePowerSetpoint;
    private Boolean angleDroopActivePowerControl;
    private Float p0;
    private Float droop;
    private VscConverterStationCreation converterStation1;
    private VscConverterStationCreation converterStation2;

    @Builder
    public VscCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName, Double nominalV,
                       Double r, Double maxP, Float operatorActivePowerLimitFromSide1ToSide2,
                       Float operatorActivePowerLimitFromSide2ToSide1, HvdcLine.ConvertersMode convertersMode,
                       Double activePowerSetpoint, Boolean angleDroopActivePowerControl, Float p0, Float droop,
                       VscConverterStationCreation converterStation1,
                       VscConverterStationCreation converterStation2) {
        super(equipmentId, properties, equipmentName);
        this.nominalV = nominalV;
        this.r = r;
        this.maxP = maxP;
        this.operatorActivePowerLimitFromSide1ToSide2 = operatorActivePowerLimitFromSide1ToSide2;
        this.operatorActivePowerLimitFromSide2ToSide1 = operatorActivePowerLimitFromSide2ToSide1;
        this.convertersMode = convertersMode;
        this.activePowerSetpoint = activePowerSetpoint;
        this.angleDroopActivePowerControl = angleDroopActivePowerControl;
        this.p0 = p0;
        this.droop = droop;
        this.converterStation1 = converterStation1;
        this.converterStation2 = converterStation2;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getHvdcLine(equipmentId) != null) {
            throw new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, equipmentId);
        }
        String errorMessage = "HVDC vsc '" + equipmentId + "' : ";
        checkConverterStation(network, converterStation1);
        checkConverterStation(network, converterStation2);
        checkDroop();
        checkIsNotNegativeValue(errorMessage, r, CREATE_VSC_ERROR, "Resistance R");
        checkIsNotNegativeValue(errorMessage, nominalV, CREATE_VSC_ERROR, "Nominal voltage");
        checkIsNotNegativeValue(errorMessage, converterStation1.getVoltageSetpoint(), CREATE_VSC_ERROR, "voltage set point side 1");
        checkIsNotNegativeValue(errorMessage, converterStation2.getVoltageSetpoint(), CREATE_VSC_ERROR, "voltage set point side 2");
        checkIsPercentage(errorMessage, converterStation1.getLossFactor(), CREATE_VSC_ERROR, "loss factor side 1");
        checkIsPercentage(errorMessage, converterStation2.getLossFactor(), CREATE_VSC_ERROR, "loss factor side 2");
    }

    private void checkDroop() {
        boolean isPresentAngleDroopActivePowerControl = angleDroopActivePowerControl != null;
        boolean isPresentDroop = droop != null;
        boolean isPresentP0 = p0 != null;
        // all fields are provided => OK extension will be created
        if (isPresentAngleDroopActivePowerControl && isPresentDroop && isPresentP0) {
            return;
        }
        // particular case, not enabling extension and others fields are not provided => OK extension will not be created
        if (Boolean.FALSE.equals(angleDroopActivePowerControl) && !isPresentDroop && !isPresentP0) {
            return;
        }
        // at least one field is provided but not for the others => NOT OK
        if (isPresentAngleDroopActivePowerControl || isPresentDroop || isPresentP0) {
            throw new NetworkModificationException(WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL, VscModification.ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG);
        }
        // otherwise, i.e. none of the fields is not provided => OK extension will not be created
    }

    private void checkConverterStation(Network network,
                                       VscConverterStationCreation converterStation) {
        if (converterStation == null) {
            throw new NetworkModificationException(CREATE_VSC_ERROR, equipmentId + "Missing required converter station");
        }
        // check connectivity
        ModificationUtils.getInstance().controlConnectivity(network,
                converterStation.getVoltageLevelId(),
                converterStation.getBusOrBusbarSectionId()
        );

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(converterStation,
                CREATE_VSC_ERROR,
                equipmentId,
                "Vsc");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VscConverterStation createdConverterStation1 = createConverterStation(network, converterStation1, subReportNode, "1");

        VscConverterStation createdConverterStation2 = createConverterStation(network, converterStation2, subReportNode, "2");

        HvdcLine hvdcLine = network.newHvdcLine()
                .setId(equipmentId)
                .setName(equipmentName)
                .setNominalV(nominalV)
                .setR(r)
                .setMaxP(maxP)
                .setActivePowerSetpoint(activePowerSetpoint)
                .setConvertersMode(convertersMode)
                .setConverterStationId1(createdConverterStation1 != null ? createdConverterStation1.getId() : null)
                .setConverterStationId2(createdConverterStation2 != null ? createdConverterStation2.getId() : null)
                .add();

        if (operatorActivePowerLimitFromSide1ToSide2 != null ||
                operatorActivePowerLimitFromSide2ToSide1 != null) {
            hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class)
                    .withOprFromCS1toCS2(operatorActivePowerLimitFromSide1ToSide2)
                    .withOprFromCS2toCS1(operatorActivePowerLimitFromSide2ToSide1)
                    .add();
        }

        if (shouldCreateDroopActivePowerControlExtension()) {
            var activePowerControlExtension = hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class)
                    .withEnabled(angleDroopActivePowerControl);
            activePowerControlExtension.withP0(p0);
            activePowerControlExtension.withDroop(droop);

            activePowerControlExtension.add();
        }

        reportHvdcLineInfos(subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.vscCreated")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (equipmentName != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, equipmentName, "Name");
        }

        PropertiesUtils.applyProperties(hvdcLine, subReportNode, properties, "network.modification.VscProperties");
    }

    @Override
    public String getName() {
        return "VscCreation";
    }

    private boolean shouldCreateDroopActivePowerControlExtension() {
        return VscModification.shouldCreateDroopActivePowerControlExtension(
            angleDroopActivePowerControl != null, droop != null, p0 != null);
    }

    private void reportHvdcLineInfos(ReportNode subReportNode) {
        List<ReportNode> characteristicsReports = new ArrayList<>();
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(nominalV, "DC nominal voltage"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(r, "DC resistance"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(maxP, "Pmax"));
        ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReports, VSC_CHARACTERISTICS);

        List<ReportNode> limitsReports = new ArrayList<>();
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(operatorActivePowerLimitFromSide1ToSide2, "Operator active power limit (Side1 -> Side 2)"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(operatorActivePowerLimitFromSide2ToSide1, "Operator active power limit (Side2 -> Side 1)"));
        ModificationUtils.getInstance().reportModifications(subReportNode, limitsReports, "network.modification.vscLimits");

        List<ReportNode> setPointsReports = new ArrayList<>();
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(convertersMode, "Converters mode"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(activePowerSetpoint, "Active power"));
        ReportNode setPointsReporter = ModificationUtils.getInstance().reportModifications(subReportNode, setPointsReports, VSC_SETPOINTS);

        List<ReportNode> angleDroopActivePowerControlReports = new ArrayList<>();
        angleDroopActivePowerControlReports.add(ModificationUtils.getInstance()
                .createEnabledDisabledReport("network.modification.angleDroopActivePowerControl", angleDroopActivePowerControl));
        if (p0 != null) {
            angleDroopActivePowerControlReports.add(ModificationUtils.getInstance().buildCreationReport(p0, "P0"));
        }
        if (droop != null) {
            angleDroopActivePowerControlReports.add(ModificationUtils.getInstance().buildCreationReport(droop, "Droop"));
        }
        ModificationUtils.getInstance().reportModifications(setPointsReporter, angleDroopActivePowerControlReports, "network.modification.vscAngleDroop");
    }

    private VscConverterStation createConverterStation(Network network,
                                                       VscConverterStationCreation vscConverterStationCreation,
                                                       ReportNode subReportNode,
                                                       String logFieldName) {
        ReportNode converterStationReporter = subReportNode.newReportNode()
            .withMessageTemplate("network.modification.vscConverterStationCreated")
            .withUntypedValue("fieldName", logFieldName)
            .withUntypedValue("id", vscConverterStationCreation.getEquipmentId())
            .add();
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, vscConverterStationCreation.getVoltageLevelId());
        VscConverterStation vscConverterStation = voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER ?
                createConverterStationInNodeBreaker(network, voltageLevel, vscConverterStationCreation, converterStationReporter) :
                createConverterStationInBusBreaker(voltageLevel, vscConverterStationCreation, converterStationReporter);

        if (!vscConverterStationCreation.isTerminalConnected()) {
            vscConverterStation.getTerminal().disconnect();
        }

        return vscConverterStation;
    }

    private VscConverterStation createConverterStationInNodeBreaker(Network network,
                                                                    VoltageLevel voltageLevel,
                                                                    VscConverterStationCreation vscConverterStationCreation,
                                                                    ReportNode subReportNode) {
        VscConverterStationAdder converterStationAdder = voltageLevel.newVscConverterStation()
                .setId(vscConverterStationCreation.getEquipmentId())
                .setName(vscConverterStationCreation.getEquipmentName())
                .setVoltageRegulatorOn(vscConverterStationCreation.getVoltageRegulationOn());

        if (vscConverterStationCreation.getReactivePowerSetpoint() != null) {
            converterStationAdder.setReactivePowerSetpoint(vscConverterStationCreation.getReactivePowerSetpoint());
        }

        if (vscConverterStationCreation.getLossFactor() != null) {
            converterStationAdder.setLossFactor(vscConverterStationCreation.getLossFactor());
        }

        if (vscConverterStationCreation.getVoltageSetpoint() != null) {
            converterStationAdder.setVoltageSetpoint(vscConverterStationCreation.getVoltageSetpoint());
        }
        createInjectionInNodeBreaker(voltageLevel, vscConverterStationCreation,
                network, converterStationAdder, subReportNode);
        VscConverterStation vscConverterStation = ModificationUtils.getInstance()
                .getVscConverterStation(network, vscConverterStationCreation.getEquipmentId());

        addExtensionsAndReports(vscConverterStation,
                vscConverterStationCreation,
                subReportNode);

        return vscConverterStation;

    }

    private VscConverterStation createConverterStationInBusBreaker(VoltageLevel voltageLevel,
                                                                   VscConverterStationCreation vscConverterStationCreation,
                                                                   ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, vscConverterStationCreation.getBusOrBusbarSectionId());
        VscConverterStation vscConverterStation = voltageLevel.newVscConverterStation()
                .setId(vscConverterStationCreation.getEquipmentId())
                .setName(vscConverterStationCreation.getEquipmentName())
                .setVoltageRegulatorOn(vscConverterStationCreation.getVoltageRegulationOn())
                .setReactivePowerSetpoint(vscConverterStationCreation.getReactivePowerSetpoint())
                .setBus(bus.getId())
                .setLossFactor(vscConverterStationCreation.getLossFactor())
                .setVoltageSetpoint(vscConverterStationCreation.getVoltageSetpoint())
                .add();

        addExtensionsAndReports(vscConverterStation, vscConverterStationCreation, subReportNode);

        return vscConverterStation;
    }

    private void addExtensionsAndReports(VscConverterStation vscConverterStation,
                                         VscConverterStationCreation vscConverterStationCreation,
                                         ReportNode subReporter) {
        reportInjectionCreationConnectivity(vscConverterStationCreation, subReporter);

        ModificationUtils.getInstance().reportModifications(subReporter,
                List.of(ModificationUtils.getInstance().buildCreationReport(vscConverterStationCreation.getLossFactor(), "Loss Factor")),
                "network.modification.converterStationCharacteristics");

        ModificationUtils.getInstance().createReactiveLimits(vscConverterStationCreation, vscConverterStation, subReporter);

        reportConverterStationSetPoints(vscConverterStationCreation, subReporter);
    }

    private void reportConverterStationSetPoints(VscConverterStationCreation vscConverterStationCreation, ReportNode subReportNode) {
        ReportNode setPointReporter = subReportNode.newReportNode().withMessageTemplate("network.modification.converterStationSetPoint").add();

        if (vscConverterStationCreation.getReactivePowerSetpoint() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(setPointReporter,
                    vscConverterStationCreation.getReactivePowerSetpoint(),
                    "Reactive power");
        }

        List<ReportNode> setPointsVoltageReports = new ArrayList<>();
        setPointsVoltageReports.add(ModificationUtils.getInstance().createEnabledDisabledReport("network.modification.voltageRegulationOn",
                vscConverterStationCreation.getVoltageRegulationOn()));
        if (vscConverterStationCreation.getVoltageSetpoint() != null) {
            setPointsVoltageReports.add(ModificationUtils.getInstance().buildCreationReport(vscConverterStationCreation.getReactivePowerSetpoint(), "Voltage"));
        }

        ModificationUtils.getInstance().reportModifications(setPointReporter,
                setPointsVoltageReports,
                "network.modification.converterStationSetPointsVoltageRegulation");
    }
}
