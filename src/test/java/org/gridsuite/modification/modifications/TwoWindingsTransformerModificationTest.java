/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import com.powsybl.iidm.network.extensions.TwoWindingsTransformerToBeEstimated;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.modifications.TwoWindingsTransformerModification.processPhaseTapRegulation;
import static org.gridsuite.modification.utils.NetworkUtil.createTwoWindingsTransformer;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TwoWindingsTransformerModificationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final Double MEASUREMENT_P_VALUE = 150.0;
    private static final Double MEASUREMENT_Q_VALUE = -20.0;
    private static final Boolean MEASUREMENT_P_VALID = false;
    private static final Boolean MEASUREMENT_Q_VALID = true;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .stashed(false)
                .equipmentName(new AttributeModification<>("2wt modified name", OperationType.SET))
                .r(new AttributeModification<>(1., OperationType.SET))
                .x(new AttributeModification<>(2., OperationType.SET))
                .g(new AttributeModification<>(3., OperationType.SET))
                .b(new AttributeModification<>(4., OperationType.SET))
                .ratedU1(new AttributeModification<>(5., OperationType.SET))
                .ratedU2(new AttributeModification<>(6., OperationType.SET))
                .ratedS(new AttributeModification<>(7., OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(12.0)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(null)
                                .name("name31")
                                .value(null)
                                .modificationType(TemporaryLimitModificationType.ADDED)
                                .build()))
                        .build())
                .currentLimits2(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(22.0)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(32)
                                .name("name32")
                                .value(42.0)
                                .modificationType(TemporaryLimitModificationType.ADDED)
                                .build()))
                        .build())
                .voltageLevelId1(new AttributeModification<>("v1", OperationType.SET))
                .voltageLevelId2(new AttributeModification<>("v2", OperationType.SET))
                .busOrBusbarSectionId1(new AttributeModification<>("1B", OperationType.SET))
                .busOrBusbarSectionId2(new AttributeModification<>("2B", OperationType.SET))
                .connectionName1(new AttributeModification<>("trf1", OperationType.SET))
                .connectionName2(new AttributeModification<>("trf1", OperationType.SET))
                .connectionDirection1(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionDirection2(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionPosition1(new AttributeModification<>(1, OperationType.SET))
                .connectionPosition2(new AttributeModification<>(2, OperationType.SET))
                .p1MeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
                .p1MeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
                .q1MeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
                .q1MeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
                .ratioTapChangerToBeEstimated(new AttributeModification<>(true, OperationType.SET))
                .phaseTapChangerToBeEstimated(new AttributeModification<>(false, OperationType.SET))
                .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                        .enabled(new AttributeModification<>(true, OperationType.SET))
                        .loadTapChangingCapabilities(new AttributeModification<>(true, OperationType.SET))
                        .regulating(new AttributeModification<>(false, OperationType.SET))
                        .targetV(new AttributeModification<>(100., OperationType.SET))
                        .targetDeadband(new AttributeModification<>(100., OperationType.SET))
                        .lowTapPosition(new AttributeModification<>(1, OperationType.SET))
                        .tapPosition(new AttributeModification<>(1, OperationType.SET))
                        .regulatingTerminalId(new AttributeModification<>("trf1", OperationType.SET))
                        .regulatingTerminalType(new AttributeModification<>("TWO_WINDINGS_TRANSFORMER", OperationType.SET))
                        .regulatingTerminalVlId(new AttributeModification<>("v1", OperationType.SET))
                        .steps(List.of(TapChangerStepCreationInfos.builder()
                                .index(0)
                                .r(0)
                                .g(0)
                                .b(0)
                                .x(0)
                                .rho(1)
                                .build(),
                                TapChangerStepCreationInfos.builder()
                                .index(1)
                                .r(0)
                                .g(0)
                                .b(0)
                                .x(0)
                                .rho(1)
                                .build()
                                ))
                        .build())
                .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                    .enabled(new AttributeModification<>(true, OperationType.SET))
                    .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                    .regulationValue(new AttributeModification<>(100., OperationType.SET))
                    .targetDeadband(new AttributeModification<>(100., OperationType.SET))
                    .lowTapPosition(new AttributeModification<>(1, OperationType.SET))
                    .tapPosition(new AttributeModification<>(1, OperationType.SET))
                    .regulatingTerminalId(new AttributeModification<>("trf1", OperationType.SET))
                    .regulatingTerminalType(new AttributeModification<>("TWO_WINDINGS_TRANSFORMER", OperationType.SET))
                    .regulatingTerminalVlId(new AttributeModification<>("v1", OperationType.SET))
                    .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(0)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .alpha(1.)
                        .build(),
                        TapChangerStepCreationInfos.builder()
                            .index(1)
                            .r(0)
                            .g(0)
                            .b(0)
                            .x(0)
                            .rho(1)
                            .alpha(1.1)
                            .build()
                        ))
                    .build())
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        TwoWindingsTransformer modifiedTwoWindingsTransformer = getNetwork().getTwoWindingsTransformer("trf1");
        assertNotNull(modifiedTwoWindingsTransformer);
        assertEquals("2wt modified name", modifiedTwoWindingsTransformer.getNameOrId());
        assertEquals(1, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("trf1")).count());
        assertEquals(1, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("trf1")).count());
        assertEquals("v1", modifiedTwoWindingsTransformer.getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", modifiedTwoWindingsTransformer.getTerminal2().getVoltageLevel().getId());
        assertEquals(1.0, modifiedTwoWindingsTransformer.getR(), 0.1);
        assertEquals(2.0, modifiedTwoWindingsTransformer.getX(), 0.1);
        assertEquals(3.0, modifiedTwoWindingsTransformer.getG(), 0.1);
        assertEquals(4.0, modifiedTwoWindingsTransformer.getB(), 0.1);
        assertEquals(5.0, modifiedTwoWindingsTransformer.getRatedU1(), 0.1);
        assertEquals(6.0, modifiedTwoWindingsTransformer.getRatedU2(), 0.1);
        assertEquals(7.0, modifiedTwoWindingsTransformer.getRatedS(), 0.1);
        // limits
        assertNotNull(modifiedTwoWindingsTransformer.getNullableCurrentLimits1());
        assertEquals(12.0, modifiedTwoWindingsTransformer.getNullableCurrentLimits1().getPermanentLimit());
        LoadingLimits.TemporaryLimit temporaryLimit = modifiedTwoWindingsTransformer.getNullableCurrentLimits1().getTemporaryLimit(Integer.MAX_VALUE);
        assertEquals(Integer.MAX_VALUE, temporaryLimit.getAcceptableDuration());
        assertEquals("name31", temporaryLimit.getName());
        assertEquals(Double.MAX_VALUE, temporaryLimit.getValue());
        assertNotNull(modifiedTwoWindingsTransformer.getNullableCurrentLimits2());
        assertEquals(22.0, modifiedTwoWindingsTransformer.getNullableCurrentLimits2().getPermanentLimit());
        temporaryLimit = modifiedTwoWindingsTransformer.getNullableCurrentLimits2().getTemporaryLimit(32);
        assertEquals(32, temporaryLimit.getAcceptableDuration());
        assertEquals("name32", temporaryLimit.getName());
        assertEquals(42.0, temporaryLimit.getValue());
        assertEquals(PROPERTY_VALUE, getNetwork().getTwoWindingsTransformer("trf1").getProperty(PROPERTY_NAME));
        assertMeasurements(modifiedTwoWindingsTransformer);
        assertToBeEstimated(modifiedTwoWindingsTransformer);
    }

    private void assertMeasurements(TwoWindingsTransformer twt) {
        Measurements<?> measurements = (Measurements<?>) twt.getExtension(Measurements.class);
        assertNotNull(measurements);
        Collection<Measurement> activePowerMeasurements = measurements.getMeasurements(Measurement.Type.ACTIVE_POWER).stream().toList();
        assertFalse(CollectionUtils.isEmpty(activePowerMeasurements));
        assertThat(activePowerMeasurements).allMatch(m -> m.getValue() == MEASUREMENT_P_VALUE && m.isValid() == MEASUREMENT_P_VALID);
        Collection<Measurement> reactivePowerMeasurements = measurements.getMeasurements(Measurement.Type.REACTIVE_POWER).stream().toList();
        assertFalse(CollectionUtils.isEmpty(reactivePowerMeasurements));
        assertThat(reactivePowerMeasurements).allMatch(m -> m.getValue() == MEASUREMENT_Q_VALUE && m.isValid() == MEASUREMENT_Q_VALID);
    }

    private void assertToBeEstimated(TwoWindingsTransformer twt) {
        TwoWindingsTransformerToBeEstimated toBeEstimated = twt.getExtension(TwoWindingsTransformerToBeEstimated.class);
        assertNotNull(toBeEstimated);
        assertTrue(toBeEstimated.shouldEstimateRatioTapChanger());
        assertFalse(toBeEstimated.shouldEstimatePhaseTapChanger());
    }

    @Override
    protected void checkModification() {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) buildModification();
        twoWindingsTransformerModificationInfos.setEquipmentId("2wt_not_existing");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> twoWindingsTransformerModificationInfos.toModification().check(getNetwork()));
        assertEquals("TWO_WINDINGS_TRANSFORMER_NOT_FOUND : Two windings transformer with ID '2wt_not_existing' : it does not exist in the network", exception.getMessage());

        // no phase tap changer on this transformer
        // ratio tap changer check regulating terminal
        TwoWindingsTransformerModificationInfos twtModificationInfos2 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos2.getRatioTapChanger().setRegulatingTerminalId(new AttributeModification<>(null, OperationType.UNSET));
        twtModificationInfos2.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        NetworkModificationException exception2 = assertThrows(NetworkModificationException.class,
            () -> twtModificationInfos2.toModification().check(getNetwork()));
        assertEquals("MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR : Two windings transformer with ID 'trf1' : Regulation is set to Distant but regulating terminal information are incomplete",
            exception2.getMessage());

        // ratio tap changer check regulating terminal
        TwoWindingsTransformerModificationInfos twtModificationInfos4 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos4.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        twtModificationInfos4.getRatioTapChanger().setRegulatingTerminalId(new AttributeModification<>("test", OperationType.UNSET));
        NetworkModificationException exception4 = assertThrows(NetworkModificationException.class,
            () -> twtModificationInfos4.toModification().check(getNetwork()));
        assertEquals("EQUIPMENT_NOT_FOUND : Equipment with id=test not found with type TWO_WINDINGS_TRANSFORMER", exception4.getMessage());

        // ratio tap changer check regulating terminal
        TwoWindingsTransformerModificationInfos twtModificationInfos5 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos5.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        twtModificationInfos5.getRatioTapChanger().setRegulatingTerminalVlId(new AttributeModification<>("test", OperationType.UNSET));
        NetworkModificationException exception5 = assertThrows(NetworkModificationException.class,
            () -> twtModificationInfos5.toModification().check(getNetwork()));
        assertEquals("VOLTAGE_LEVEL_NOT_FOUND : Voltage level with id=test not found", exception5.getMessage());

        // do not throw
        TwoWindingsTransformerModificationInfos twtModificationInfos6 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos6.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        twtModificationInfos6.getRatioTapChanger().setRegulatingTerminalId(new AttributeModification<>(null, OperationType.UNSET));
        twtModificationInfos6.getRatioTapChanger().setRegulatingTerminalVlId(new AttributeModification<>(null, OperationType.UNSET));
        twtModificationInfos6.getRatioTapChanger().setRegulatingTerminalType(new AttributeModification<>(null, OperationType.UNSET));
        assertDoesNotThrow(() -> twtModificationInfos6.toModification().check(getNetwork()));

        // do not throw
        TwoWindingsTransformerModificationInfos twtModificationInfos7 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos7.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        assertDoesNotThrow(() -> twtModificationInfos7.toModification().check(getNetwork()));
    }

    private TwoWindingsTransformer createPhaseTapChanger() {
        return createPhaseTapChanger(PhaseTapChanger.RegulationMode.FIXED_TAP);
    }

    private TwoWindingsTransformer createPhaseTapChanger(PhaseTapChanger.RegulationMode regulationMode) {
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);
        Terminal phaseTapChangerTerminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(getNetwork(),
            "v3load",
            "LOAD",
            "v3");
        twt3.newPhaseTapChanger()
            .setLowTapPosition(0)
            .setTapPosition(1)
            .setRegulationTerminal(phaseTapChangerTerminal)
            .setRegulationMode(regulationMode)
            .beginStep()
            .setR(39.78473)
            .setX(39.784725)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.)
            .endStep()
            .beginStep()
            .setR(39.78475)
            .setX(39.784727)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.1)
            .endStep()
            .add();
        return twt3;
    }

    @Test
    void testPhaseTapChangerRegulationModification() throws Exception {
        TwoWindingsTransformer twt3 = createPhaseTapChanger();
        String twtId = "trf3";
        // modification 1 : FIXED_TAP -> CURRENT_LIMITER
        TwoWindingsTransformerModificationInfos phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                .regulationValue(new AttributeModification<>(10.0, OperationType.SET))
                .build())
            .build();

        phaseTapChangerCreation.toModification().apply(getNetwork());

        PhaseTapChanger phaseTapChanger = twt3.getPhaseTapChanger();

        // modification 1 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(phaseTapChanger.isRegulating());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());

        // modification 2  : CURRENT_LIMITER -> FIXED_TAP
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.FIXED_TAP, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationValue(null);

        phaseTapChangerCreation.toModification().apply(getNetwork());
        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 2 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertFalse(phaseTapChanger.isRegulating());

        // modification 3   : FIXED_TAP -> ACTIVE_POWER_CONTROL
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setTargetDeadband(new AttributeModification<>(1.0, OperationType.SET));

        phaseTapChangerCreation.toModification().apply(getNetwork());
        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 3 assert
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(1.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertTrue(phaseTapChanger.isRegulating());

        // modification 4 : ACTIVE_POWER_CONTROL -> CURRENT_LIMITER
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationValue(new AttributeModification<>(8.0, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setTargetDeadband(new AttributeModification<>(2.0, OperationType.SET));

        phaseTapChangerCreation.toModification().apply(getNetwork());

        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 4 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(2.0, phaseTapChanger.getTargetDeadband());
        assertEquals(8.0, phaseTapChanger.getRegulationValue());
        assertTrue(phaseTapChanger.isRegulating());

        // modification 5 : CURRENT_LIMITER -> FIX_TAP
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.FIXED_TAP, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationValue(null);
        phaseTapChangerCreation.getPhaseTapChanger().setTargetDeadband(null);

        phaseTapChangerCreation.toModification().apply(getNetwork());

        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 5 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(2.0, phaseTapChanger.getTargetDeadband());
        assertEquals(8.0, phaseTapChanger.getRegulationValue());
        assertFalse(phaseTapChanger.isRegulating());
    }

    @Test
    void testPhaseTapChangerRegulationModification2() throws Exception {
        TwoWindingsTransformer twt3 = createPhaseTapChanger();
        String twtId = "trf3";

        // modification 1 : FIXED_TAP -> ACTIVE_POWER_CONTROL error
        TwoWindingsTransformerModificationInfos phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET))
                .build())
            .build();

        // modification 1 assert
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> phaseTapChangerCreation.toModification().apply(getNetwork()));
        assertEquals("MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR : Regulation value is missing, phase tap changer can not regulate", exception.getMessage());

        // modification 2 : FIXED_TAP -> FIXED_TAP
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.FIXED_TAP, OperationType.SET));

        phaseTapChangerCreation.toModification().apply(getNetwork());

        PhaseTapChanger phaseTapChanger = twt3.getPhaseTapChanger();

        // modification 2 assert
        assertEquals(PhaseTapChanger.RegulationMode.FIXED_TAP, phaseTapChanger.getRegulationMode());
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
        assertFalse(phaseTapChanger.isRegulating());

        // modification 3 : FIXED_TAP -> ACTIVE_POWER_CONTROL
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationValue(new AttributeModification<>(8.0, OperationType.SET));

        phaseTapChangerCreation.toModification().apply(getNetwork());

        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 3 assert
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(8.0, phaseTapChanger.getRegulationValue());
        assertTrue(phaseTapChanger.isRegulating());

        // modification 4 : ACTIVE_POWER_CONTROL -> CURRENT_LIMITER
        twt3.remove();
        createPhaseTapChanger(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL);
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationValue(new AttributeModification<>(6.0, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setTargetDeadband(null);
        phaseTapChangerCreation.toModification().apply(getNetwork());

        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 4 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(6.0, phaseTapChanger.getRegulationValue());
        assertTrue(phaseTapChanger.isRegulating());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("TWO_WINDINGS_TRANSFORMER_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("trf1", createdValues.get("equipmentId"));
    }

    @Test
    void testChangeConnectionStatus() throws Exception {
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.ONE, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.ONE, true, false, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.TWO, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.TWO, true, false, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.ONE, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.ONE, true, false, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.TWO, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.TWO, true, false, "BRANCH_MODIFICATION_ERROR : Could not disconnect equipment 'trf2'");
    }

    private void changeConnectionState(TwoWindingsTransformer existingEquipment, TwoSides side, boolean actualState, boolean expectedState, String errorMessage) throws Exception {
        Terminal terminal = existingEquipment.getTerminal(side);
        assertThat(terminal.isConnected()).isEqualTo(actualState);

        TwoWindingsTransformerModificationInfos modificationInfos =
                TwoWindingsTransformerModificationInfos.builder()
                        .stashed(false)
                        .equipmentId(existingEquipment.getId())
                        .terminal1Connected(side == TwoSides.ONE ? new AttributeModification<>(expectedState, OperationType.SET) : null)
                        .terminal2Connected(side == TwoSides.TWO ? new AttributeModification<>(expectedState, OperationType.SET) : null)
                        .build();

        if (!Objects.isNull(errorMessage)) {
            NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> modificationInfos.toModification().apply(getNetwork()));
            // change not applied
            assertThat(terminal.isConnected()).isNotEqualTo(expectedState);
            assertThat(exception.getMessage()).isEqualTo(errorMessage);
        } else {
            modificationInfos.toModification().apply(getNetwork());
            // connection state has changed as expected
            assertThat(terminal.isConnected()).isEqualTo(expectedState);
            // try to modify again => no change on connection state
            modificationInfos.toModification().apply(getNetwork());
            assertThat(terminal.isConnected()).isEqualTo(expectedState);
        }
    }

    @Test
    void testProcessPhaseTapChangerModification() {
        TwoWindingsTransformer twt = createPhaseTapChanger();
        PhaseTapChanger phaseTapChanger = twt.getPhaseTapChanger();
        List<ReportNode> regulationReports = new ArrayList<>();
        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.FIXED_TAP, OperationType.SET),
            null, null, regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.FIXED_TAP, phaseTapChanger.getRegulationMode());
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertFalse(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET),
            new AttributeModification<>(10.0, OperationType.SET), null, regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertTrue(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.FIXED_TAP, OperationType.SET),
            null, null, regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertFalse(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            new AttributeModification<>(12.0, OperationType.SET),
            new AttributeModification<>(8.0, OperationType.SET),
            regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(12.0, phaseTapChanger.getRegulationValue());
        assertEquals(8.0, phaseTapChanger.getTargetDeadband());
        assertTrue(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.FIXED_TAP, OperationType.SET),
            null, null, regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(12.0, phaseTapChanger.getRegulationValue());
        assertEquals(8.0, phaseTapChanger.getTargetDeadband());
        assertFalse(phaseTapChanger.isRegulating());
    }

    @Test
    void testProcessPhaseTapChangerCreation() {
        TwoWindingsTransformer twt = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);
        List<ReportNode> regulationReports = new ArrayList<>();
        PhaseTapChangerAdder adder = twt.newPhaseTapChanger();
        preparePhaseTapChangerAdder(adder);
        String message = assertThrows(NetworkModificationException.class, () -> processPhaseTapRegulation(null, adder, false,
            null, null, null, regulationReports)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Regulation mode is missing when creating tap phase changer", message);

        AttributeModification<PhaseTapChanger.RegulationMode> regulationModeModification = new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET);
        String message2 = assertThrows(NetworkModificationException.class, () -> processPhaseTapRegulation(null, adder, false,
            regulationModeModification, null, null, regulationReports)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Regulation value is missing when creating tap phase changer with regulation enabled (different from FIXED_TAP)", message2);
        processPhaseTapRegulation(null, adder, false,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.FIXED_TAP, OperationType.SET),
            null, null, regulationReports);
        adder.add();
        PhaseTapChanger phaseTapChanger = twt.getPhaseTapChanger();
        assertEquals(PhaseTapChanger.RegulationMode.FIXED_TAP, phaseTapChanger.getRegulationMode());
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertFalse(phaseTapChanger.isRegulating());

        PhaseTapChangerAdder adder1 = twt.newPhaseTapChanger();
        preparePhaseTapChangerAdder(adder1);
        processPhaseTapRegulation(null, adder1, false,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            new AttributeModification<>(10.0, OperationType.SET), null, regulationReports);
        adder1.add();
        phaseTapChanger = twt.getPhaseTapChanger();
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertTrue(phaseTapChanger.isRegulating());
    }

    private void preparePhaseTapChangerAdder(PhaseTapChangerAdder phaseTapChangerAdder) {
        Terminal phaseTapChangerTerminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(getNetwork(),
            "v3load",
            "LOAD",
            "v3");
        phaseTapChangerAdder.setLowTapPosition(0)
            .setTapPosition(1)
            .setRegulationTerminal(phaseTapChangerTerminal)
            .beginStep()
            .setR(39.78473)
            .setX(39.784725)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.)
            .endStep()
            .beginStep()
            .setR(39.78475)
            .setX(39.784727)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.1)
            .endStep();
    }

    @Test
    void testPhaseTapChangerRegulationCreation() throws Exception {
        // test with non pre-existent phase tap changer
        String twtId = "trf3";
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);
        // creation 1 : CURRENT_LIMITER
        TwoWindingsTransformerModificationInfos phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                .regulationValue(new AttributeModification<>(10.0, OperationType.SET))
                .lowTapPosition(new AttributeModification<>(0, OperationType.SET))
                .tapPosition(new AttributeModification<>(1, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v3load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v3", OperationType.SET))
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(0)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(1)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build()
                ))
                .build())
            .build();

        phaseTapChangerCreation.toModification().apply(getNetwork());
        PhaseTapChanger phaseTapChanger = twt3.getPhaseTapChanger();

        // creation 1 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(phaseTapChanger.isRegulating());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());

        // creation 2 : FIXED_TAP
        twt3.getPhaseTapChanger().remove();
        phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.FIXED_TAP, OperationType.SET))
                .lowTapPosition(new AttributeModification<>(0, OperationType.SET))
                .tapPosition(new AttributeModification<>(1, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v3load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v3", OperationType.SET))
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(0)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(1)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build()
                ))
                .build())
            .build();

        phaseTapChangerCreation.toModification().apply(getNetwork());

        phaseTapChanger = twt3.getPhaseTapChanger();

        // creation 2 assert
        assertEquals(PhaseTapChanger.RegulationMode.FIXED_TAP, phaseTapChanger.getRegulationMode());
        assertFalse(phaseTapChanger.isRegulating());
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
    }

    @Test
    void testRatioTapChangerRegulationCreation() throws Exception {
        String twtId = "trf3";
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);

        TwoWindingsTransformerModificationInfos ratioTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .lowTapPosition(new AttributeModification<>(0, OperationType.SET))
                .tapPosition(new AttributeModification<>(1, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v3load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v3", OperationType.SET))
                .regulating(new AttributeModification<>(false, OperationType.SET))
                .targetDeadband(new AttributeModification<>(10.0, OperationType.SET))
                .regulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET))
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(0)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(1)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build()
                ))
                .build())
            .build();

        ratioTapChangerCreation.toModification().apply(getNetwork());
        RatioTapChanger ratioTapChanger = twt3.getRatioTapChanger();
        assertFalse(ratioTapChanger.isRegulating());
        assertEquals(RatioTapChanger.RegulationMode.REACTIVE_POWER, ratioTapChanger.getRegulationMode());

        TwoWindingsTransformerModificationInfos ratioTapChangerModification = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .regulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .build())
            .build();
        ratioTapChangerModification.toModification().apply(getNetwork());

        assertTrue(ratioTapChanger.isRegulating());
        assertEquals(RatioTapChanger.RegulationMode.VOLTAGE, ratioTapChanger.getRegulationMode());
    }

    @Test
    void testRatioTapChangerTargetDeadBandModification() throws Exception {
        String twtId = "trf3";
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);

        TwoWindingsTransformerModificationInfos ratioTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .lowTapPosition(new AttributeModification<>(0, OperationType.SET))
                .tapPosition(new AttributeModification<>(1, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v3load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v3", OperationType.SET))
                .regulating(new AttributeModification<>(false, OperationType.SET))
                .regulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET))
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(0)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(1)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build()
                ))
                .build())
            .build();
        ratioTapChangerCreation.toModification().apply(getNetwork());
        RatioTapChanger ratioTapChanger = twt3.getRatioTapChanger();

        // when creating the ratioTapChanger in a modification the targetDeadband is set to 0
        assertEquals(0.0, ratioTapChanger.getTargetDeadband());

        // hard set to NaN
        ratioTapChanger.setTargetDeadband(Double.NaN);
        assertTrue(Double.isNaN(ratioTapChanger.getTargetDeadband()));

        // set regulation without target deadband it will set target deadband to 0
        TwoWindingsTransformerModificationInfos ratioTapChangerModification = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .regulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .build())
            .build();
        ratioTapChangerModification.toModification().apply(getNetwork());

        assertEquals(0.0, ratioTapChanger.getTargetDeadband());
    }
}

