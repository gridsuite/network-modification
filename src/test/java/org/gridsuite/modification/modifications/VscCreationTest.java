/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControl;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRange;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.modifications.VscModification.ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class VscCreationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationModel buildModification() {
        return VscCreationModel.builder()
            .equipmentId("vsc1")
            .equipmentName("vsc1Name")
            .nominalV(39.)
            .r(4.)
            .maxP(56.)
            .p0(5F)
            .operatorActivePowerLimitFromSide2ToSide1(5.6F)
            .convertersMode(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER)
            .activePowerSetpoint(5.)
            .operatorActivePowerLimitFromSide1ToSide2(6.0F)
            .operatorActivePowerLimitFromSide2ToSide1(8F)
            .droop(1F)
            .angleDroopActivePowerControl(true)
            .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
            .converterStation2(buildConverterStationWithMinMaxReactiveLimits())
            .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    private static ConverterStationCreationModel buildConverterStationWithMinMaxReactiveLimits() {
        return ConverterStationCreationModel.builder()
            .equipmentId("stationId2")
            .equipmentName("station2")
            .voltageRegulationOn(false)
            .reactivePowerSetpoint(23.)
            .reactiveCapabilityCurve(false)
            .maxQ(66.)
            .lossFactor(4F)
            .minQ(55.)
            .voltageSetpoint(34.)
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .reactiveCapabilityCurvePoints(List.of())
            .reactiveCapabilityCurve(false)
            .build();
    }

    private static ConverterStationCreationModel buildConverterStationWithReactiveCapabilityCurve() {
        var point1 = ReactiveCapabilityCurvePointsModel.builder()
            .p(0.4)
            .maxQ(3.)
            .minQ(0.)
            .build();
        var point2 = ReactiveCapabilityCurvePointsModel.builder()
            .p(0.6)
            .maxQ(2.)
            .minQ(1.1)
            .build();

        return ConverterStationCreationModel.builder()
            .equipmentId("stationId1")
            .equipmentName("station1")
            .voltageRegulationOn(true)
            .voltageSetpoint(66.)
            .reactivePowerSetpoint(44.)
            .lossFactor(40F)
            .reactiveCapabilityCurve(true)
            .reactiveCapabilityCurvePoints(List.of(point1, point2))
            .voltageLevelId("v1")
            .busOrBusbarSectionId("1.1")
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getHvdcLine("vsc1"));

        assertEquals(1, getNetwork().getVoltageLevel("v1").getVscConverterStationStream()
            .filter(converterStation -> converterStation.getId().equals("stationId1")).count());

        assertEquals(1, getNetwork().getVoltageLevel("v2").getVscConverterStationStream()
            .filter(converterStation -> converterStation.getId().equals("stationId2")).count());

        HvdcLine hvdcLine = getNetwork().getHvdcLine("vsc1");
        assertNotNull(hvdcLine);
        assertEquals(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, hvdcLine.getConvertersMode());
        assertEquals(39, hvdcLine.getNominalV(), 0);
        assertEquals(4, hvdcLine.getR(), 0);
        assertEquals(5, hvdcLine.getActivePowerSetpoint(), 0);
        assertEquals(56, hvdcLine.getMaxP(), 0);
        assertEquals(PROPERTY_VALUE, hvdcLine.getProperty(PROPERTY_NAME));

        HvdcOperatorActivePowerRange hvdcOperatorActivePowerRange = hvdcLine.getExtension(HvdcOperatorActivePowerRange.class);
        assertEquals(6, hvdcOperatorActivePowerRange.getOprFromCS1toCS2(), 0);
        assertEquals(8, hvdcOperatorActivePowerRange.getOprFromCS2toCS1(), 0);

        HvdcAngleDroopActivePowerControl activePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertEquals(1, activePowerControl.getDroop(), 0);
        assertEquals(5, activePowerControl.getP0(), 0);

        VscConverterStation vscConverterStation1 = (VscConverterStation) hvdcLine.getConverterStation1();
        assertNotNull(vscConverterStation1);
        assertEquals(44, vscConverterStation1.getReactivePowerSetpoint(), 0);
        assertEquals(40, vscConverterStation1.getLossFactor(), 0);
        assertEquals(ReactiveLimitsKind.CURVE, vscConverterStation1.getReactiveLimits().getKind());
        ReactiveCapabilityCurve reactiveLimits1 = vscConverterStation1.getReactiveLimits(ReactiveCapabilityCurve.class);
        assertEquals(2, reactiveLimits1.getPointCount());
        assertEquals(0.6, reactiveLimits1.getMaxP(), 0);
        assertEquals(0.4, reactiveLimits1.getMinP(), 0);
        assertEquals(66, vscConverterStation1.getVoltageSetpoint(), 0);
        assertEquals("v1", vscConverterStation1.getTerminal().getVoltageLevel().getId());

        VscConverterStation vscConverterStation2 = (VscConverterStation) hvdcLine.getConverterStation2();
        assertNotNull(vscConverterStation2);
        assertEquals(23, vscConverterStation2.getReactivePowerSetpoint(), 0);
        assertEquals(4, vscConverterStation2.getLossFactor(), 0);
        assertEquals(ReactiveLimitsKind.MIN_MAX, vscConverterStation2.getReactiveLimits().getKind());
        MinMaxReactiveLimits reactiveLimits2 = vscConverterStation2.getReactiveLimits(MinMaxReactiveLimits.class);
        assertEquals(66, reactiveLimits2.getMaxQ(), 0);
        assertEquals(55, reactiveLimits2.getMinQ(), 0);
        assertEquals(34, vscConverterStation2.getVoltageSetpoint(), 0);
        assertEquals("v2", vscConverterStation2.getTerminal().getVoltageLevel().getId());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        // assertEquals("VSC_CREATION", modificationModel.getMessageType());
        // Map<String, String> createdValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals("vsc1", createdValues.get("equipmentId"));
    }

    @Override
    protected void checkModification() {
        Network network = getNetwork();
        VscCreationModel vscCreationModel = (VscCreationModel) buildModification();
        // not found voltage level
        vscCreationModel.setEquipmentId("vscId");
        ConverterStationCreationModel converterStationCreationModel = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationModel.setVoltageLevelId("notFoundVoltageLevelId");
        vscCreationModel.setConverterStation2(converterStationCreationModel);
        VscCreation vscCreation = (VscCreation) vscCreationModel.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> vscCreation.check(network));
        assertEquals(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(), exception.getMessage());

        // invalid min max reactive limit
        vscCreationModel = (VscCreationModel) buildModification();
        converterStationCreationModel = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationModel.setConnectionPosition(35);
        converterStationCreationModel.setReactiveCapabilityCurve(false);
        converterStationCreationModel.setMinQ(Double.NaN);
        vscCreationModel.setConverterStation1(converterStationCreationModel);

        VscCreation vscCreation1 = (VscCreation) vscCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation1.check(network));
        assertEquals(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : minimum reactive power is not set").getMessage(), exception.getMessage());

        vscCreationModel = (VscCreationModel) buildModification();
        converterStationCreationModel = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationModel.setConnectionPosition(66);
        converterStationCreationModel.setReactiveCapabilityCurve(false);
        converterStationCreationModel.setMaxQ(Double.NaN);
        vscCreationModel.setConverterStation1(converterStationCreationModel);

        VscCreation vscCreation2 = (VscCreation) vscCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation2.check(network));
        assertEquals(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : maximum reactive power is not set").getMessage(), exception.getMessage());

        vscCreationModel = (VscCreationModel) buildModification();
        converterStationCreationModel = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationModel.setConnectionPosition(15);
        converterStationCreationModel.setReactiveCapabilityCurve(false);
        converterStationCreationModel.setMinQ(200.);
        converterStationCreationModel.setMaxQ(100.);
        vscCreationModel.setConverterStation1(converterStationCreationModel);

        VscCreation vscCreation3 = (VscCreation) vscCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation3.check(network));
        assertEquals(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : maximum reactive power is expected to be greater than or equal to minimum reactive power").getMessage(), exception.getMessage());

        // invalid reactive capability curve limit
        vscCreationModel = (VscCreationModel) buildModification();
        converterStationCreationModel = buildConverterStationWithReactiveCapabilityCurve();
        converterStationCreationModel.setConnectionPosition(55);
        converterStationCreationModel.getReactiveCapabilityCurvePoints().getFirst().setP(Double.NaN);
        vscCreationModel.setConverterStation1(converterStationCreationModel);

        VscCreation vscCreation4 = (VscCreation) vscCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation4.check(network));
        assertEquals(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : P is not set in a reactive capability curve limits point").getMessage(), exception.getMessage());

        // try to create an existing vsc
        vscCreationModel = (VscCreationModel) buildModification();
        vscCreationModel.setEquipmentId("hvdcLine");
        VscCreation vscCreation5 = (VscCreation) vscCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation5.check(network));
        assertEquals(new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, "hvdcLine").getMessage(), exception.getMessage());

        VscCreationModel vscCreationModel6 = VscCreationModel.builder()
            .equipmentId("hvdcLine2")
            .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
            .converterStation2(buildConverterStationWithMinMaxReactiveLimits())
            .r(-1d)
            .build();
        VscCreation vscCreation6 = (VscCreation) vscCreationModel6.toModification();
        String message = assertThrows(NetworkModificationException.class,
            () -> vscCreation6.check(network)).getMessage();
        assertEquals("CREATE_VSC_ERROR : HVDC vsc 'hvdcLine2' : can not have a negative value for Resistance R", message);

        VscCreationModel vscCreationModel7 = VscCreationModel.builder()
            .equipmentId("hvdcLine2")
            .converterStation1(ConverterStationCreationModel.builder()
                .equipmentId("station1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .voltageSetpoint(-100d)
                .build())
            .converterStation2(buildConverterStationWithReactiveCapabilityCurve())
            .build();
        VscCreation vscCreation7 = (VscCreation) vscCreationModel7.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> vscCreation7.check(network)).getMessage();
        assertEquals("CREATE_VSC_ERROR : HVDC vsc 'hvdcLine2' : can not have a negative value for voltage set point side 1", message);

        VscCreationModel vscCreationModel8 = VscCreationModel.builder()
            .equipmentId("hvdcLine2")
            .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
            .converterStation2(ConverterStationCreationModel.builder()
                .equipmentId("station2")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .voltageSetpoint(-100d)
                .build())
            .build();
        VscCreation vscCreation8 = (VscCreation) vscCreationModel8.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> vscCreation8.check(network)).getMessage();
        assertEquals("CREATE_VSC_ERROR : HVDC vsc 'hvdcLine2' : can not have a negative value for voltage set point side 2", message);

        VscCreationModel vscCreationModel9 = VscCreationModel.builder()
            .equipmentId("hvdcLine2")
            .nominalV(-10d)
            .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
            .converterStation2(buildConverterStationWithReactiveCapabilityCurve())
            .build();
        VscCreation vscCreation9 = (VscCreation) vscCreationModel9.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> vscCreation9.check(network)).getMessage();
        assertEquals("CREATE_VSC_ERROR : HVDC vsc 'hvdcLine2' : can not have a negative value for Nominal voltage", message);

        VscCreationModel vscCreationModel10 = VscCreationModel.builder()
            .equipmentId("hvdcLine2")
            .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
            .converterStation2(ConverterStationCreationModel.builder()
                .equipmentId("station2")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .lossFactor(101f)
                .build())
            .build();
        VscCreation vscCreation10 = (VscCreation) vscCreationModel10.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> vscCreation10.check(network)).getMessage();
        assertEquals("CREATE_VSC_ERROR : HVDC vsc 'hvdcLine2' : must have loss factor side 2 between 0 and 100", message);

        VscCreationModel vscCreationModel11 = VscCreationModel.builder()
            .equipmentId("hvdcLine2")
            .converterStation2(buildConverterStationWithReactiveCapabilityCurve())
            .converterStation1(ConverterStationCreationModel.builder()
                .equipmentId("station2")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .lossFactor(-1f)
                .build())
            .build();
        VscCreation vscCreation11 = (VscCreation) vscCreationModel11.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> vscCreation11.check(network)).getMessage();
        assertEquals("CREATE_VSC_ERROR : HVDC vsc 'hvdcLine2' : must have loss factor side 1 between 0 and 100", message);
    }

    @Test
    void testCreateAngleDroopPowerControlWithoutEnabling() {
        VscCreationModel vscCreationModel = (VscCreationModel) buildModification();
        vscCreationModel.setAngleDroopActivePowerControl(false);
        vscCreationModel.toModification().apply(getNetwork());
        HvdcLine hvdcLine = getNetwork().getHvdcLine("vsc1");
        assertThat(hvdcLine).isNotNull();
        HvdcAngleDroopActivePowerControl activePowerControlExt = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertThat(activePowerControlExt).isNotNull();
        assertThat(activePowerControlExt.isEnabled()).isFalse();
        assertThat(activePowerControlExt.getDroop()).isEqualTo(1F);
        assertThat(activePowerControlExt.getP0()).isEqualTo(5F);
    }

    @Test
    void testNotCreateAngleDroopPowerControlWithoutEnabling() {
        VscCreationModel vscCreationModel = (VscCreationModel) buildModification();
        vscCreationModel.setAngleDroopActivePowerControl(false);
        vscCreationModel.setDroop(null);
        vscCreationModel.setP0(null);
        vscCreationModel.toModification().apply(getNetwork());
        HvdcLine hvdcLine = getNetwork().getHvdcLine("vsc1");
        assertThat(hvdcLine).isNotNull();
        HvdcAngleDroopActivePowerControl activePowerControlExt = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertThat(activePowerControlExt).isNull();
    }

    @Test
    void testAngleDroopPowerControlWithAbsentModel() {
        boolean[][] droopModelIsPresentData = {
            {true, false, false},
            {true, true, false},
            {true, false, true},
            {false, true, false},
            {false, true, true},
            {false, false, true},
        };

        for (boolean[] droopInfoIsPresent : droopModelIsPresentData) {
            VscCreationModel vscCreationModel = buildModificationWithDroopAbsentModel(droopInfoIsPresent[0], droopInfoIsPresent[1], droopInfoIsPresent[2]);
            checkDroopWithAbsentModel(vscCreationModel);
        }
    }

    private VscCreationModel buildModificationWithDroopAbsentModel(boolean isPresentAngleDroopActivePowerControl, boolean isPresentDroop, boolean isPresentP0) {
        VscCreationModel vscCreationModel = (VscCreationModel) buildModification();
        // reset null depending to test arguments
        if (!isPresentAngleDroopActivePowerControl) {
            vscCreationModel.setAngleDroopActivePowerControl(null);
        }
        if (!isPresentDroop) {
            vscCreationModel.setDroop(null);
        }
        if (!isPresentP0) {
            vscCreationModel.setP0(null);
        }
        return vscCreationModel;
    }

    private void checkDroopWithAbsentModel(VscCreationModel vscCreationModel) {
        Network network = getNetwork();
        VscCreation vscCreation = (VscCreation) vscCreationModel.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> vscCreation.check(network));
        assertEquals(new NetworkModificationException(WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL,
                ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG).getMessage(),
            exception.getMessage());
    }
}
