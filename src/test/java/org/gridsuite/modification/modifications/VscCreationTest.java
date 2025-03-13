/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControl;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRange;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
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
    protected ModificationInfos buildModification() {
        return VscCreationInfos.builder()
                .stashed(false)
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
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    private static ConverterStationCreationInfos buildConverterStationWithMinMaxReactiveLimits() {
        return ConverterStationCreationInfos.builder()
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

    private static ConverterStationCreationInfos buildConverterStationWithReactiveCapabilityCurve() {
        var point1 = ReactiveCapabilityCurvePointsInfos.builder()
                .p(0.4)
                .maxQ(3.)
                .minQ(0.)
                .build();
        var point2 = ReactiveCapabilityCurvePointsInfos.builder()
                .p(0.6)
                .maxQ(2.)
                .minQ(1.1)
                .build();

        return ConverterStationCreationInfos.builder()
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
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VSC_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("vsc1", createdValues.get("equipmentId"));
    }

    @Override
    protected void checkModification() {
        Network network = getNetwork();
        VscCreationInfos vscCreationInfos = (VscCreationInfos) buildModification();
        // not found voltage level
        vscCreationInfos.setEquipmentId("vscId");
        ConverterStationCreationInfos converterStationCreationInfos = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        vscCreationInfos.setConverterStation2(converterStationCreationInfos);
        VscCreation vscCreation = (VscCreation) vscCreationInfos.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> vscCreation.check(network));
        assertEquals(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(), exception.getMessage());

        // invalid min max reactive limit
        vscCreationInfos = (VscCreationInfos) buildModification();
        converterStationCreationInfos = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationInfos.setConnectionPosition(35);
        converterStationCreationInfos.setReactiveCapabilityCurve(false);
        converterStationCreationInfos.setMinQ(Double.NaN);
        vscCreationInfos.setConverterStation1(converterStationCreationInfos);

        VscCreation vscCreation1 = (VscCreation) vscCreationInfos.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation1.check(network));
        assertEquals(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : minimum reactive power is not set").getMessage(), exception.getMessage());

        vscCreationInfos = (VscCreationInfos) buildModification();
        converterStationCreationInfos = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationInfos.setConnectionPosition(66);
        converterStationCreationInfos.setReactiveCapabilityCurve(false);
        converterStationCreationInfos.setMaxQ(Double.NaN);
        vscCreationInfos.setConverterStation1(converterStationCreationInfos);

        VscCreation vscCreation2 = (VscCreation) vscCreationInfos.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation2.check(network));
        assertEquals(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : maximum reactive power is not set").getMessage(), exception.getMessage());

        vscCreationInfos = (VscCreationInfos) buildModification();
        converterStationCreationInfos = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationInfos.setConnectionPosition(15);
        converterStationCreationInfos.setReactiveCapabilityCurve(false);
        converterStationCreationInfos.setMinQ(200.);
        converterStationCreationInfos.setMaxQ(100.);
        vscCreationInfos.setConverterStation1(converterStationCreationInfos);

        VscCreation vscCreation3 = (VscCreation) vscCreationInfos.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation3.check(network));
        assertEquals(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : maximum reactive power is expected to be greater than or equal to minimum reactive power").getMessage(), exception.getMessage());

        // invalid reactive capability curve limit
        vscCreationInfos = (VscCreationInfos) buildModification();
        converterStationCreationInfos = buildConverterStationWithReactiveCapabilityCurve();
        converterStationCreationInfos.setConnectionPosition(55);
        converterStationCreationInfos.getReactiveCapabilityCurvePoints().get(0).setP(Double.NaN);
        vscCreationInfos.setConverterStation1(converterStationCreationInfos);

        VscCreation vscCreation4 = (VscCreation) vscCreationInfos.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation4.check(network));
        assertEquals(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : P is not set in a reactive capability curve limits point").getMessage(), exception.getMessage());

        // try to create an existing vsc
        vscCreationInfos = (VscCreationInfos) buildModification();
        vscCreationInfos.setEquipmentId("hvdcLine");
        VscCreation vscCreation5 = (VscCreation) vscCreationInfos.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> vscCreation5.check(network));
        assertEquals(new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, "hvdcLine").getMessage(), exception.getMessage());

        VscCreationInfos vscCreationInfos6 = VscCreationInfos.builder()
            .equipmentId("hvdcLine2")
            .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
            .converterStation2(buildConverterStationWithMinMaxReactiveLimits())
            .r(-1d)
            .build();
        VscCreation vscCreation6 = (VscCreation) vscCreationInfos6.toModification();
        String message = assertThrows(NetworkModificationException.class,
            () -> vscCreation6.check(network)).getMessage();
        assertEquals("CREATE_VSC_ERROR : HVDC vsc 'hvdcLine2' : can not have a negative value for R", message);

        VscCreationInfos vscCreationInfos7 = VscCreationInfos.builder()
            .equipmentId("hvdcLine2")
            .converterStation1(ConverterStationCreationInfos.builder()
                .equipmentId("station1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .voltageSetpoint(-100d)
                .build())
            .converterStation2(buildConverterStationWithReactiveCapabilityCurve())
            .build();
        VscCreation vscCreation7 = (VscCreation) vscCreationInfos7.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> vscCreation7.check(network)).getMessage();
        assertEquals("CREATE_VSC_ERROR : HVDC vsc 'hvdcLine2' : can not have a negative value for voltage set point side 1", message);

        VscCreationInfos vscCreationInfos8 = VscCreationInfos.builder()
            .equipmentId("hvdcLine2")
            .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
            .converterStation2(ConverterStationCreationInfos.builder()
                .equipmentId("station2")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .voltageSetpoint(-100d)
                .build())
            .build();
        VscCreation vscCreation8 = (VscCreation) vscCreationInfos8.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> vscCreation8.check(network)).getMessage();
        assertEquals("CREATE_VSC_ERROR : HVDC vsc 'hvdcLine2' : can not have a negative value for voltage set point side 2", message);
    }

    @Test
    void testCreateAngleDroopPowerControlWithoutEnabling() throws Exception {
        VscCreationInfos vscCreationInfos = (VscCreationInfos) buildModification();
        vscCreationInfos.setAngleDroopActivePowerControl(false);
        vscCreationInfos.toModification().apply(getNetwork());
        HvdcLine hvdcLine = getNetwork().getHvdcLine("vsc1");
        assertThat(hvdcLine).isNotNull();
        HvdcAngleDroopActivePowerControl activePowerControlExt = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertThat(activePowerControlExt).isNotNull();
        assertThat(activePowerControlExt.isEnabled()).isFalse();
        assertThat(activePowerControlExt.getDroop()).isEqualTo(1F);
        assertThat(activePowerControlExt.getP0()).isEqualTo(5F);
    }

    @Test
    void testNotCreateAngleDroopPowerControlWithoutEnabling() throws Exception {
        VscCreationInfos vscCreationInfos = (VscCreationInfos) buildModification();
        vscCreationInfos.setAngleDroopActivePowerControl(false);
        vscCreationInfos.setDroop(null);
        vscCreationInfos.setP0(null);
        vscCreationInfos.toModification().apply(getNetwork());
        HvdcLine hvdcLine = getNetwork().getHvdcLine("vsc1");
        assertThat(hvdcLine).isNotNull();
        HvdcAngleDroopActivePowerControl activePowerControlExt = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertThat(activePowerControlExt).isNull();
    }

    @Test
    void testAngleDroopPowerControlWithAbsentInfos() throws Exception {
        boolean[][] droopInfosIsPresentData = {
            {true, false, false},
            {true, true, false},
            {true, false, true},
            {false, true, false},
            {false, true, true},
            {false, false, true},
        };

        for (boolean[] droopInfoIsPresent : droopInfosIsPresentData) {
            VscCreationInfos vscCreationInfos = buildModificationWithDroopAbsentInfos(droopInfoIsPresent[0], droopInfoIsPresent[1], droopInfoIsPresent[2]);
            checkDroopWithAbsentInfos(vscCreationInfos);
        }
    }

    private VscCreationInfos buildModificationWithDroopAbsentInfos(boolean isPresentAngleDroopActivePowerControl, boolean isPresentDroop, boolean isPresentP0) {
        VscCreationInfos vscCreationInfos = (VscCreationInfos) buildModification();
        // reset null depending to test arguments
        if (!isPresentAngleDroopActivePowerControl) {
            vscCreationInfos.setAngleDroopActivePowerControl(null);
        }
        if (!isPresentDroop) {
            vscCreationInfos.setDroop(null);
        }
        if (!isPresentP0) {
            vscCreationInfos.setP0(null);
        }
        return vscCreationInfos;
    }

    private void checkDroopWithAbsentInfos(VscCreationInfos vscCreationInfos) throws Exception {
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> vscCreationInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL,
                        ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG).getMessage(),
                exception.getMessage());
    }
}
