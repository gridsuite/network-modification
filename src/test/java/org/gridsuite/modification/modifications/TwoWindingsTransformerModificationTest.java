/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import com.powsybl.iidm.network.extensions.TwoWindingsTransformerToBeEstimated;
import com.powsybl.iidm.network.util.BusbarSectionFinderTraverser;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.dto.OperationalLimitsGroupModificationType.ADD;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.BRANCH_MODIFICATION_ERROR;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.CREATE_TWO_WINDINGS_TRANSFORMER_ERROR;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.EQUIPMENT_NOT_FOUND;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.TWO_WINDINGS_TRANSFORMER_NOT_FOUND;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.VOLTAGE_LEVEL_NOT_FOUND;
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
    private static final int RATIO_TAP_POSITION_MEASUREMENT = 3;
    private static final int PHASE_TAP_POSITION_MEASUREMENT = -2;

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
                .voltageLevelId1(new AttributeModification<>("v1", OperationType.SET))
                .voltageLevelId2(new AttributeModification<>("v2", OperationType.SET))
                .busOrBusbarSectionId1(new AttributeModification<>("1.1", OperationType.SET))
                .busOrBusbarSectionId2(new AttributeModification<>("1B", OperationType.SET))
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
                .ratioTapChangerPositionMeasurement(new AttributeModification<>(RATIO_TAP_POSITION_MEASUREMENT, OperationType.SET))
                .phaseTapChangerPositionMeasurement(new AttributeModification<>(PHASE_TAP_POSITION_MEASUREMENT, OperationType.SET))
                .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                        .enabled(new AttributeModification<>(true, OperationType.SET))
                        .loadTapChangingCapabilities(new AttributeModification<>(true, OperationType.SET))
                        .regulating(new AttributeModification<>(false, OperationType.SET))
                        .targetV(new AttributeModification<>(100., OperationType.SET))
                        .targetDeadband(new AttributeModification<>(100., OperationType.SET))
                        .lowTapPosition(new AttributeModification<>(1, OperationType.SET))
                        .tapPosition(new AttributeModification<>(1, OperationType.SET))
                        .terminalRefConnectableId(new AttributeModification<>("trf1", OperationType.SET))
                        .terminalRefConnectableType(new AttributeModification<>("TWO_WINDINGS_TRANSFORMER", OperationType.SET))
                        .terminalRefConnectableVlId(new AttributeModification<>("v1", OperationType.SET))
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
                    .terminalRefConnectableId(new AttributeModification<>("trf1", OperationType.SET))
                    .terminalRefConnectableType(new AttributeModification<>("TWO_WINDINGS_TRANSFORMER", OperationType.SET))
                    .terminalRefConnectableVlId(new AttributeModification<>("v1", OperationType.SET))
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
                .enableOLGModification(true)
                .operationalLimitsGroups(List.of(
                        OperationalLimitsGroupModificationInfos.builder()
                                .id("ETE")
                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                .modificationType(ADD)
                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                        .permanentLimit(800.0)
                                        .build()
                                        )
                                .build(),
                        OperationalLimitsGroupModificationInfos.builder()
                                .id("ETE")
                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE2)
                                .modificationType(ADD)
                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                        .permanentLimit(800.0)
                                        .build()
                                        )
                                .build()
                ))
                .selectedOperationalLimitsGroupId1(new AttributeModification<>("DEFAULT", OperationType.SET))
                .selectedOperationalLimitsGroupId2(new AttributeModification<>("ETE", OperationType.SET))
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
        assertEquals(PROPERTY_VALUE, modifiedTwoWindingsTransformer.getProperty(PROPERTY_NAME));
        assertOLG(modifiedTwoWindingsTransformer);
        assertMeasurements(modifiedTwoWindingsTransformer);
        assertToBeEstimated(modifiedTwoWindingsTransformer);
        assertTapPositionMeasurement(modifiedTwoWindingsTransformer, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, RATIO_TAP_POSITION_MEASUREMENT);
        assertTapPositionMeasurement(modifiedTwoWindingsTransformer, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, PHASE_TAP_POSITION_MEASUREMENT);
    }

    private static void assertOLG(TwoWindingsTransformer modifiedTwt) {
        assertTrue(modifiedTwt.getSelectedOperationalLimitsGroup1().isPresent());
        assertEquals("DEFAULT", modifiedTwt.getSelectedOperationalLimitsGroup1().get().getId());
        assertTrue(modifiedTwt.getSelectedOperationalLimitsGroup2().isPresent());
        assertEquals("ETE", modifiedTwt.getSelectedOperationalLimitsGroup2().get().getId());
        assertTrue(modifiedTwt.getOperationalLimitsGroup2("ETE").isPresent());
        assertTrue(modifiedTwt.getOperationalLimitsGroup2("ETE").get().getCurrentLimits().isPresent());
        assertEquals(800.0, modifiedTwt.getOperationalLimitsGroup2("ETE").get().getCurrentLimits().get().getPermanentLimit());
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
        Network network = getNetwork();
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) buildModification();
        twoWindingsTransformerModificationInfos.setEquipmentId("2wt_not_existing");
        TwoWindingsTransformerModification twoWindingsTransformerModification = (TwoWindingsTransformerModification) twoWindingsTransformerModificationInfos.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> twoWindingsTransformerModification.check(network));
        assertEquals(TWO_WINDINGS_TRANSFORMER_NOT_FOUND.getMessage() + " : Two windings transformer '2wt_not_existing' : it does not exist in the network", exception.getMessage());

        // no phase tap changer on this transformer
        // ratio tap changer check regulating terminal
        TwoWindingsTransformerModificationInfos twtModificationInfos2 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos2.getRatioTapChanger().setTerminalRefConnectableId(new AttributeModification<>(null, OperationType.UNSET));
        twtModificationInfos2.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        TwoWindingsTransformerModification twoWindingsTransformerModification2 = (TwoWindingsTransformerModification) twtModificationInfos2.toModification();
        NetworkModificationException exception2 = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerModification2.check(network));
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Two windings transformer 'trf1' : Regulation is set to Distant but regulating terminal information are incomplete",
            exception2.getMessage());

        // ratio tap changer check regulating terminal
        TwoWindingsTransformerModificationInfos twtModificationInfos4 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos4.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        twtModificationInfos4.getRatioTapChanger().setTerminalRefConnectableId(new AttributeModification<>("test", OperationType.UNSET));
        TwoWindingsTransformerModification twoWindingsTransformerModification4 = (TwoWindingsTransformerModification) twtModificationInfos4.toModification();
        NetworkModificationException exception4 = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerModification4.check(network));
        assertEquals(EQUIPMENT_NOT_FOUND.getMessage() + " : Equipment with id=test not found with type TWO_WINDINGS_TRANSFORMER", exception4.getMessage());

        // ratio tap changer check regulating terminal
        TwoWindingsTransformerModificationInfos twtModificationInfos5 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos5.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        twtModificationInfos5.getRatioTapChanger().setTerminalRefConnectableVlId(new AttributeModification<>("test", OperationType.UNSET));
        TwoWindingsTransformerModification twoWindingsTransformerModification5 = (TwoWindingsTransformerModification) twtModificationInfos5.toModification();
        NetworkModificationException exception5 = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerModification5.check(network));
        assertEquals(VOLTAGE_LEVEL_NOT_FOUND.getMessage() + " : Voltage level with id=test not found", exception5.getMessage());

        // do not throw
        TwoWindingsTransformerModificationInfos twtModificationInfos6 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos6.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        twtModificationInfos6.getRatioTapChanger().setTerminalRefConnectableId(new AttributeModification<>(null, OperationType.UNSET));
        twtModificationInfos6.getRatioTapChanger().setTerminalRefConnectableVlId(new AttributeModification<>(null, OperationType.UNSET));
        twtModificationInfos6.getRatioTapChanger().setTerminalRefConnectableType(new AttributeModification<>(null, OperationType.UNSET));
        assertDoesNotThrow(() -> twtModificationInfos6.toModification().check(getNetwork()));
        assertTrue(((TwoWindingsTransformerModification) twtModificationInfos6.toModification()).characteristicsModified());

        // do not throw
        TwoWindingsTransformerModificationInfos twtModificationInfos7 = (TwoWindingsTransformerModificationInfos) buildModification();
        twtModificationInfos7.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.UNSET));
        assertDoesNotThrow(() -> twtModificationInfos7.toModification().check(getNetwork()));

        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos1 = TwoWindingsTransformerModificationInfos.builder()
            .equipmentId("trf1")
            .r(new AttributeModification<>(-1d, OperationType.SET))
            .build();
        TwoWindingsTransformerModification twoWindingsTransformerModification6 = (TwoWindingsTransformerModification) twoWindingsTransformerModificationInfos1.toModification();
        String message = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerModification6.check(network)).getMessage();
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Two windings transformer 'trf1' : can not have a negative value for Resistance R", message);

        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos2 = TwoWindingsTransformerModificationInfos.builder()
            .equipmentId("trf1")
            .g(new AttributeModification<>(-2d, OperationType.SET))
            .build();
        TwoWindingsTransformerModification twoWindingsTransformerModification7 = (TwoWindingsTransformerModification) twoWindingsTransformerModificationInfos2.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerModification7.check(network)).getMessage();
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Two windings transformer 'trf1' : can not have a negative value for Conductance G", message);

        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos3 = TwoWindingsTransformerModificationInfos.builder()
            .equipmentId("trf1")
            .ratedU1(new AttributeModification<>(-100d, OperationType.SET))
            .build();
        TwoWindingsTransformerModification twoWindingsTransformerModification8 = (TwoWindingsTransformerModification) twoWindingsTransformerModificationInfos3.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerModification8.check(network)).getMessage();
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Two windings transformer 'trf1' : can not have a negative value for Rated Voltage on side 1", message);

        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos4 = TwoWindingsTransformerModificationInfos.builder()
            .equipmentId("trf1")
            .ratedU2(new AttributeModification<>(-100d, OperationType.SET))
            .build();
        TwoWindingsTransformerModification twoWindingsTransformerModification9 = (TwoWindingsTransformerModification) twoWindingsTransformerModificationInfos4.toModification();
        message = assertThrows(NetworkModificationException.class, () -> twoWindingsTransformerModification9.check(network)).getMessage();
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Two windings transformer 'trf1' : can not have a negative value for Rated Voltage on side 2", message);

        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos5 = TwoWindingsTransformerModificationInfos.builder()
            .equipmentId("trf1")
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .targetV(new AttributeModification<>(-100d, OperationType.SET)).build())
            .build();
        TwoWindingsTransformerModification twoWindingsTransformerModification10 = (TwoWindingsTransformerModification) twoWindingsTransformerModificationInfos5.toModification();
        message = assertThrows(NetworkModificationException.class, () -> twoWindingsTransformerModification10.check(network)).getMessage();
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Two windings transformer 'trf1' : can not have a negative value for Target voltage for ratio tap changer", message);

        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos6 = TwoWindingsTransformerModificationInfos.builder()
            .equipmentId("trf1")
            .ratedS(new AttributeModification<>(-200d, OperationType.SET))
            .build();
        TwoWindingsTransformerModification twoWindingsTransformerModification11 = (TwoWindingsTransformerModification) twoWindingsTransformerModificationInfos6.toModification();
        message = assertThrows(NetworkModificationException.class, () -> twoWindingsTransformerModification11.check(network)).getMessage();
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Two windings transformer 'trf1' : can not have a negative value for Rated nominal power", message);
    }

    private TwoWindingsTransformer createPhaseTapChanger() {
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
            .setRegulating(false)
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
    void testTerminalRefConnectableModification() {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) buildModification();
        for (int i = 1; i <= 3; i++) {
            TapChangerModificationInfos phaseTapChangerModificationInfos = twoWindingsTransformerModificationInfos.getPhaseTapChanger();
            switch (i) {
                case 3:
                    phaseTapChangerModificationInfos.setTerminalRefConnectableId(new AttributeModification<>("test", OperationType.SET));
                    phaseTapChangerModificationInfos.setTerminalRefConnectableType(new AttributeModification<>(null, OperationType.UNSET));
                    phaseTapChangerModificationInfos.setTerminalRefConnectableVlId(new AttributeModification<>(null, OperationType.UNSET));
                    break;
                case 2:
                    phaseTapChangerModificationInfos.setTerminalRefConnectableId(new AttributeModification<>(null, OperationType.UNSET));
                    phaseTapChangerModificationInfos.setTerminalRefConnectableType(new AttributeModification<>(null, OperationType.UNSET));
                    phaseTapChangerModificationInfos.setTerminalRefConnectableVlId(new AttributeModification<>("test", OperationType.SET));
                    break;
                case 1:
                    phaseTapChangerModificationInfos.setTerminalRefConnectableId(new AttributeModification<>(null, OperationType.UNSET));
                    phaseTapChangerModificationInfos.setTerminalRefConnectableType(new AttributeModification<>("test", OperationType.SET));
                    phaseTapChangerModificationInfos.setTerminalRefConnectableVlId(new AttributeModification<>(null, OperationType.UNSET));
                    break;
                default:
            }
            TwoWindingsTransformerModification twoWindingsTransformerModification = (TwoWindingsTransformerModification) twoWindingsTransformerModificationInfos.toModification();
            Boolean value = ReflectionTestUtils.invokeMethod(twoWindingsTransformerModification, "commonTapChangerAttributesModified", phaseTapChangerModificationInfos);
            assertNotNull(value);
            assertTrue(value);
        }
    }

    @Test
    void testPhaseTapChangerRegulationModification() {
        TwoWindingsTransformer twt3 = createPhaseTapChanger();
        String twtId = "trf3";
        // modification 1
        TwoWindingsTransformerModificationInfos phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                .regulationValue(new AttributeModification<>(10.0, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .build())
            .build();

        phaseTapChangerCreation.toModification().apply(getNetwork());

        PhaseTapChanger phaseTapChanger = twt3.getPhaseTapChanger();

        // modification 1 assert
        assertEquals("TWO_WINDINGS_TRANSFORMER_MODIFICATION", phaseTapChangerCreation.toModification().getName());
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(phaseTapChanger.isRegulating());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());

        // modification 2
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulating(new AttributeModification<>(false, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationValue(null);

        phaseTapChangerCreation.toModification().apply(getNetwork());
        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 2 assert
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertFalse(phaseTapChanger.isRegulating());

        // modification 3
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setTargetDeadband(new AttributeModification<>(1.0, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulating(new AttributeModification<>(true, OperationType.SET));

        phaseTapChangerCreation.toModification().apply(getNetwork());
        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 3 assert
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(1.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertTrue(phaseTapChanger.isRegulating());

        // modification 4
        phaseTapChangerCreation.getPhaseTapChanger().setEnabled(new AttributeModification<>(false, OperationType.SET));

        phaseTapChangerCreation.toModification().apply(getNetwork());
        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 4 assert
        assertNull(phaseTapChanger);
    }

    @Test
    void testPhaseTapChangerRegulationModification2() {
        TwoWindingsTransformer twt3 = createPhaseTapChanger();
        String twtId = "trf3";

        // modification 1 : settings
        TwoWindingsTransformerModificationInfos phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .build())
            .build();

        // modification 1 assert
        Network network = getNetwork();
        TwoWindingsTransformerModification twoWindingsTransformerModification = (TwoWindingsTransformerModification) phaseTapChangerCreation.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> twoWindingsTransformerModification.apply(network));
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Regulation value is missing when modifying, phase tap changer can not regulate", exception.getMessage());

        // modification 2 : settings
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationValue(new AttributeModification<>(10.0, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET));

        phaseTapChangerCreation.toModification().apply(getNetwork());

        PhaseTapChanger phaseTapChanger = twt3.getPhaseTapChanger();

        // modification 2 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertTrue(phaseTapChanger.isRegulating());

        // Regulation mode not set
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(null, OperationType.SET));
        TwoWindingsTransformerModification tmpTransformerModification = (TwoWindingsTransformerModification) phaseTapChangerCreation.toModification();
        Throwable exc = assertThrows(ValidationException.class, () -> tmpTransformerModification.apply(network));
        assertEquals("2 windings transformer 'trf3': phase regulation mode is not set", exc.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("TWO_WINDINGS_TRANSFORMER_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("trf1", createdValues.get("equipmentId"));
    }

    @Test
    void testChangeConnectionStatus() {
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.ONE, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.ONE, true, false, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.TWO, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.TWO, true, false, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.ONE, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.ONE, true, false, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.TWO, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.TWO, true, false,
            BRANCH_MODIFICATION_ERROR.getMessage() + " : Could not disconnect equipment 'trf2' on side 2");
    }

    private void changeConnectionState(TwoWindingsTransformer existingEquipment, TwoSides side, boolean actualState, boolean expectedState, String errorMessage) {
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
            Network network = getNetwork();
            TwoWindingsTransformerModification twoWindingsTransformerModification = (TwoWindingsTransformerModification) modificationInfos.toModification();
            NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> twoWindingsTransformerModification.apply(network));
            // change not applied
            assertThat(terminal.isConnected()).isNotEqualTo(expectedState);
            assertThat(exception.getMessage()).isEqualTo(errorMessage);
            assertFalse(twoWindingsTransformerModification.characteristicsModified());
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
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            null, null, null, regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertFalse(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET),
            new AttributeModification<>(10.0, OperationType.SET), null,
            new AttributeModification<>(true, OperationType.SET), regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertTrue(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET),
            null, null,
            new AttributeModification<>(false, OperationType.SET), regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertFalse(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            new AttributeModification<>(12.0, OperationType.SET),
            new AttributeModification<>(8.0, OperationType.SET), null,
            regulationReports);
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
        AttributeModification<Double> regulationValueModification = new AttributeModification<>(10.0, OperationType.SET);
        AttributeModification<Boolean> regulatingModification = new AttributeModification<>(true, OperationType.SET);
        String message = assertThrows(NetworkModificationException.class, () -> processPhaseTapRegulation(null, adder, false,
            null, regulationValueModification, null, regulatingModification, regulationReports)).getMessage();
        assertEquals(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Regulation mode is missing when creating tap phase changer with regulation enabled", message);

        AttributeModification<PhaseTapChanger.RegulationMode> regulationModeModification = new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET);
        String message2 = assertThrows(NetworkModificationException.class, () -> processPhaseTapRegulation(null, adder, false,
            regulationModeModification, null, null, regulatingModification, regulationReports)).getMessage();
        assertEquals(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Regulation value is missing when creating tap phase changer with regulation enabled", message2);
        processPhaseTapRegulation(null, adder, false,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            null, null, null, regulationReports);
        adder.add();
        PhaseTapChanger phaseTapChanger = twt.getPhaseTapChanger();
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertFalse(phaseTapChanger.isRegulating());

        PhaseTapChangerAdder adder1 = twt.newPhaseTapChanger();
        preparePhaseTapChangerAdder(adder1);
        processPhaseTapRegulation(null, adder1, false,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            new AttributeModification<>(10.0, OperationType.SET), null,
            new AttributeModification<>(true, OperationType.SET), regulationReports);
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
    void testPhaseTapChangerRegulationCreation() {
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
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .regulationValue(new AttributeModification<>(10.0, OperationType.SET))
                .lowTapPosition(new AttributeModification<>(0, OperationType.SET))
                .tapPosition(new AttributeModification<>(1, OperationType.SET))
                .terminalRefConnectableId(new AttributeModification<>("v3load", OperationType.SET))
                .terminalRefConnectableType(new AttributeModification<>("LOAD", OperationType.SET))
                .terminalRefConnectableVlId(new AttributeModification<>("v3", OperationType.SET))
                .regulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
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
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                .lowTapPosition(new AttributeModification<>(0, OperationType.SET))
                .tapPosition(new AttributeModification<>(1, OperationType.SET))
                .terminalRefConnectableId(new AttributeModification<>("v3load", OperationType.SET))
                .terminalRefConnectableType(new AttributeModification<>("LOAD", OperationType.SET))
                .terminalRefConnectableVlId(new AttributeModification<>("v3", OperationType.SET))
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
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertFalse(phaseTapChanger.isRegulating());
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
    }

    @Test
    void testRatioTapChangerRegulationCreation() {
        String twtId = "trf3";
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);

        TwoWindingsTransformerModificationInfos ratioTapChangerCreation = createRatioTapChangerInfos(twtId);

        ratioTapChangerCreation.toModification().apply(getNetwork());
        RatioTapChanger ratioTapChanger = twt3.getRatioTapChanger();
        assertFalse(ratioTapChanger.isRegulating());
        assertNull(ratioTapChanger.getRegulationMode());

        TwoWindingsTransformerModificationInfos ratioTapChangerModification = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .regulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .targetV(new AttributeModification<>(10.0, OperationType.SET))
                .loadTapChangingCapabilities(new AttributeModification<>(true, OperationType.SET))
                .build())
            .build();
        ratioTapChangerModification.toModification().apply(getNetwork());

        assertTrue(ratioTapChanger.isRegulating());
        assertTrue(ratioTapChanger.hasLoadTapChangingCapabilities());
        assertEquals(10.0, ratioTapChanger.getTargetV());
        assertEquals(RatioTapChanger.RegulationMode.VOLTAGE, ratioTapChanger.getRegulationMode());
    }

    @Test
    void testRatioTapChangerTargetDeadBandModification() {
        String twtId = "trf3";
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);

        TwoWindingsTransformerModificationInfos ratioTapChangerCreation = createRatioTapChangerInfos(twtId);
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
                .targetV(new AttributeModification<>(10.0, OperationType.SET))
                .loadTapChangingCapabilities(new AttributeModification<>(true, OperationType.SET))
                .build())
            .build();
        ratioTapChangerModification.toModification().apply(getNetwork());

        assertEquals(0.0, ratioTapChanger.getTargetDeadband());

        // Add disable test
        ratioTapChangerModification.getRatioTapChanger().setEnabled(new AttributeModification<>(false, OperationType.SET));
        ratioTapChangerModification.toModification().apply(getNetwork());
        assertNull(getNetwork().getTwoWindingsTransformer(twtId).getRatioTapChanger());
    }

    @Test
    void testProcessRatioTapChangerRegulationModification() {
        String twtId = "trf3";
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);

        TwoWindingsTransformerModificationInfos ratioTapChangerCreation = createRatioTapChangerInfos(twtId);
        ratioTapChangerCreation.toModification().apply(getNetwork());
        RatioTapChanger ratioTapChanger = twt3.getRatioTapChanger();

        assertFalse(ratioTapChanger.isRegulating());
        assertNull(ratioTapChanger.getRegulationMode());

        TwoWindingsTransformerModificationInfos ratioTapChangerModification = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .targetV(new AttributeModification<>(10.0, OperationType.SET))
                .loadTapChangingCapabilities(new AttributeModification<>(true, OperationType.SET))
                .build())
            .build();
        ratioTapChangerModification.toModification().apply(getNetwork());

        assertTrue(ratioTapChanger.isRegulating());
        assertEquals(RatioTapChanger.RegulationMode.VOLTAGE, ratioTapChanger.getRegulationMode());
        assertEquals(10.0, ratioTapChanger.getTargetV());

        // applying another modification on others attributes does not change regulation
        ratioTapChangerModification = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .targetV(new AttributeModification<>(120.0, OperationType.SET))
                .build())
            .build();
        ratioTapChangerModification.toModification().apply(getNetwork());
        assertTrue(ratioTapChanger.isRegulating());
        assertEquals(RatioTapChanger.RegulationMode.VOLTAGE, ratioTapChanger.getRegulationMode());
        assertEquals(120.0, ratioTapChanger.getTargetV());

        ratioTapChangerModification.getRatioTapChanger().setRegulationSide(new AttributeModification<>(RegulationSide.SIDE1, OperationType.SET));
        ratioTapChangerModification.getRatioTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET));
        ratioTapChangerModification.toModification().apply(getNetwork());
        assertFalse(ratioTapChanger.getRegulationTerminal().isConnected());
    }

    @Test
    void testRatioTapChangerLoadTapChangingCapabilitiesModification() {
        String twtId = "trf3";
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);

        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = createRatioTapChangerInfos(twtId);
        twoWindingsTransformerModificationInfos.toModification().apply(getNetwork());

        RatioTapChanger ratioTapChanger = twt3.getRatioTapChanger();
        ratioTapChanger.setLoadTapChangingCapabilities(true);
        ratioTapChanger.setRegulationMode(RatioTapChanger.RegulationMode.VOLTAGE);
        ratioTapChanger.setRegulationValue(200);
        ratioTapChanger.setTargetDeadband(2);
        ratioTapChanger.setRegulating(true);

        assertEquals("v3", ratioTapChanger.getRegulationTerminal().getVoltageLevel().getId());
        assertEquals("v3load", ratioTapChanger.getRegulationTerminal().getConnectable().getId());

        // apply modification with loadTapChangingCapabilities set to false
        twoWindingsTransformerModificationInfos = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId("trf3")
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .loadTapChangingCapabilities(new AttributeModification<>(false, OperationType.SET))
                .build())
            .build();

        twoWindingsTransformerModificationInfos.toModification().apply(getNetwork());
        assertFalse(ratioTapChanger.isRegulating());
        assertFalse(ratioTapChanger.hasLoadTapChangingCapabilities());
        assertEquals(Double.NaN, ratioTapChanger.getRegulationValue());
        assertEquals(Double.NaN, ratioTapChanger.getTargetDeadband());
        assertNull(ratioTapChanger.getRegulationTerminal());
    }

    @Test
    void testCreateRegulatingRatioTapChanger() {
        String twtId = "trf3";
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);

        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = createRatioTapChangerInfos(twtId);
        // modify 'twoWindingsTransformerModificationInfos' in order to test the creation of a ratio tap changer immediately regulating
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setRegulating(new AttributeModification<>(true, OperationType.SET));
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setLoadTapChangingCapabilities(new AttributeModification<>(true, OperationType.SET));
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTargetV(new AttributeModification<>(200.0, OperationType.SET));
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTargetDeadband(new AttributeModification<>(2.0, OperationType.SET));
        twoWindingsTransformerModificationInfos.toModification().apply(getNetwork());

        RatioTapChanger ratioTapChanger = twt3.getRatioTapChanger();

        assertTrue(ratioTapChanger.isRegulating());
        assertSame(RatioTapChanger.RegulationMode.VOLTAGE, ratioTapChanger.getRegulationMode());
        assertEquals("v3", ratioTapChanger.getRegulationTerminal().getVoltageLevel().getId());
        assertEquals("v3load", ratioTapChanger.getRegulationTerminal().getConnectable().getId());
    }

    private TwoWindingsTransformerModificationInfos createRatioTapChangerInfos(String twtId) {
        return TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .lowTapPosition(new AttributeModification<>(0, OperationType.SET))
                .tapPosition(new AttributeModification<>(1, OperationType.SET))
                .terminalRefConnectableId(new AttributeModification<>("v3load", OperationType.SET))
                .terminalRefConnectableType(new AttributeModification<>("LOAD", OperationType.SET))
                .terminalRefConnectableVlId(new AttributeModification<>("v3", OperationType.SET))
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
    }

    @Test
    void testMoveFeederBay() {
        TwoWindingsTransformerModificationInfos transformerModificationInfos = (TwoWindingsTransformerModificationInfos) buildModification();
        // Change busbar section only on terminal 2
        transformerModificationInfos.setBusOrBusbarSectionId2(new AttributeModification<>("1A", OperationType.SET));

        transformerModificationInfos.toModification().apply(getNetwork());
        TwoWindingsTransformer twoWindingsTransformer = getNetwork().getTwoWindingsTransformer("trf1");
        Terminal terminal1 = twoWindingsTransformer.getTerminal1();
        Terminal terminal2 = twoWindingsTransformer.getTerminal2();
        assertEquals("v1", twoWindingsTransformer.getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", terminal2.getVoltageLevel().getId());
        assertEquals("1.1", BusbarSectionFinderTraverser.findBusbarSectionId(terminal1));
        assertEquals("1A", BusbarSectionFinderTraverser.findBusbarSectionId(terminal2));
    }

    @Test
    void testProcessPhaseTapChangerRegulationModification() {
        TwoWindingsTransformer twt = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
                41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
                "trf3", 1, ConnectablePosition.Direction.TOP,
                "trf3", 2, ConnectablePosition.Direction.TOP);
        PhaseTapChangerAdder adder = twt.newPhaseTapChanger();
        preparePhaseTapChangerAdder(adder);
        adder.add();
        List<ReportNode> regulationReports = new ArrayList<>();
        AttributeModification<Double> regulationValueModification = new AttributeModification<>(-10.0, OperationType.SET);
        AttributeModification<PhaseTapChanger.RegulationMode> regulationCurrentLimiterModeModification = new AttributeModification<>(
                PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET);
        AttributeModification<PhaseTapChanger.RegulationMode> regulationActivePowerControlModeModification = new AttributeModification<>(
                PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET);
        AttributeModification<Boolean> regulatingModification = new AttributeModification<>(true, OperationType.SET);
        // for CURRENT_LIMITER mode, regulation value can not be negative
        // creation
        PhaseTapChanger phaseTapChanger = twt.getPhaseTapChanger();
        String message = assertThrows(NetworkModificationException.class, () -> processPhaseTapRegulation(null, adder, false,
                regulationCurrentLimiterModeModification, regulationValueModification, null, regulatingModification, regulationReports)).getMessage();
        assertEquals(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Regulation value must be positive if regulation mode is CURRENT_LIMITER "
                        + "when creating tap phase changer with regulation enabled",
                message);
        // modification
        String message2 = assertThrows(NetworkModificationException.class, () -> processPhaseTapRegulation(phaseTapChanger, null, true,
                regulationCurrentLimiterModeModification, regulationValueModification, null, regulatingModification, regulationReports)).getMessage();
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Regulation value must be positive if regulation mode is CURRENT_LIMITER "
                + "when modifying, phase tap changer can not regulate", message2);
        // tap changer mode is initially CURRENT_LIMITER, so it must throw if a negative target value is set and the mode is not modified
        String message3 = assertThrows(NetworkModificationException.class, () -> processPhaseTapRegulation(phaseTapChanger, null, true,
                null, regulationValueModification, null, regulatingModification, regulationReports)).getMessage();
        assertEquals(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR.getMessage() + " : Regulation value must be positive if regulation mode is CURRENT_LIMITER "
                + "when modifying, phase tap changer can not regulate", message3);
        // for ACTIVE_POWER_CONTROL mode, regulation value can be negative
        // creation
        assertDoesNotThrow(() -> processPhaseTapRegulation(null, adder, false,
                regulationActivePowerControlModeModification, regulationValueModification, null, regulatingModification, regulationReports));
        // modification
        assertDoesNotThrow(() -> processPhaseTapRegulation(phaseTapChanger, null, true,
                regulationActivePowerControlModeModification, regulationValueModification, null, regulatingModification, regulationReports));
    }

    @Test
    void testProcessPhaseTapChangerRegulationModificationWithRegulating() {
        TwoWindingsTransformer twt = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
                41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
                "trf3", 1, ConnectablePosition.Direction.TOP,
                "trf3", 2, ConnectablePosition.Direction.TOP);
        PhaseTapChangerAdder adder = twt.newPhaseTapChanger();
        preparePhaseTapChangerAdder(adder);
        adder.add();
        PhaseTapChanger phaseTapChanger = twt.getPhaseTapChanger();

        phaseTapChanger.setRegulationValue(100);
        phaseTapChanger.setTargetDeadband(0);
        phaseTapChanger.setRegulationMode(PhaseTapChanger.RegulationMode.CURRENT_LIMITER);
        phaseTapChanger.setRegulating(true);

        List<ReportNode> regulationReports = new ArrayList<>();
        AttributeModification<PhaseTapChanger.RegulationMode> regulationActivePowerControlModeModification = new AttributeModification<>(
                PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET);
        AttributeModification<Double> regulationValueModification = new AttributeModification<>(-100.0, OperationType.SET);

        processPhaseTapRegulation(phaseTapChanger, null, true,
                regulationActivePowerControlModeModification, regulationValueModification, null, null, regulationReports);
        assertEquals(-100.0, phaseTapChanger.getRegulationValue());
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertTrue(phaseTapChanger.isRegulating());

        AttributeModification<PhaseTapChanger.RegulationMode> regulationCurrentLimiterModeModification = new AttributeModification<>(
                PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET);
        AttributeModification<Double> regulationValueModification2 = new AttributeModification<>(100.0, OperationType.SET);
        processPhaseTapRegulation(phaseTapChanger, null, true,
                regulationCurrentLimiterModeModification, regulationValueModification2, null, null, regulationReports);
        assertEquals(100.0, phaseTapChanger.getRegulationValue());
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(phaseTapChanger.isRegulating());
    }

    @Test
    void testMeasurementValidityPropertyForTransformer() {
        TwoWindingsTransformer transformer = getNetwork().getTwoWindingsTransformer("trf1");
        Measurements<?> measurements = (Measurements<?>) transformer.getExtension(Measurements.class);
        if (measurements == null) {
            measurements = (Measurements<?>) transformer.newExtension(MeasurementsAdder.class).add();
        }
        Measurement activePowerMeasurement = measurements.newMeasurement()
                .setId(UUID.randomUUID().toString())
                .setType(Measurement.Type.ACTIVE_POWER)
                .setSide(ThreeSides.ONE)
                .setValue(11.0)
                .setValid(true)
                .add();

        activePowerMeasurement.putProperty("validity", "1");
        TwoWindingsTransformerModificationInfos modificationInfosTrueFromOne = TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .p1MeasurementValidity(new AttributeModification<>(true, OperationType.SET))
                .build();
        modificationInfosTrueFromOne.toModification().apply(getNetwork());
        assertEquals("0", activePowerMeasurement.getProperty("validity"));

        activePowerMeasurement.putProperty("validity", "3");
        TwoWindingsTransformerModificationInfos modificationInfosTrueFromThree = TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .p1MeasurementValidity(new AttributeModification<>(true, OperationType.SET))
                .build();
        modificationInfosTrueFromThree.toModification().apply(getNetwork());
        assertEquals("2", activePowerMeasurement.getProperty("validity"));

        Measurement reactivePowerMeasurement = measurements.newMeasurement()
                .setId(UUID.randomUUID().toString())
                .setType(Measurement.Type.REACTIVE_POWER)
                .setSide(ThreeSides.ONE)
                .setValue(-11.0)
                .setValid(false)
                .add();
        reactivePowerMeasurement.putProperty("validity", "2");
        TwoWindingsTransformerModificationInfos modificationInfosFalseFromTwo = TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .q1MeasurementValidity(new AttributeModification<>(false, OperationType.SET))
                .build();
        modificationInfosFalseFromTwo.toModification().apply(getNetwork());
        assertEquals("3", reactivePowerMeasurement.getProperty("validity"));

        reactivePowerMeasurement.putProperty("validity", "0");
        TwoWindingsTransformerModificationInfos modificationInfosFalseFromZero = TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .q1MeasurementValidity(new AttributeModification<>(false, OperationType.SET))
                .build();
        modificationInfosFalseFromZero.toModification().apply(getNetwork());
        assertEquals("1", reactivePowerMeasurement.getProperty("validity"));
    }

    private static DiscreteMeasurements<?> getDiscreteMeasurements(TwoWindingsTransformer twt) {
        return (DiscreteMeasurements<?>) twt.getExtension(DiscreteMeasurements.class);
    }

    private static DiscreteMeasurements<?> getOrCreateDiscreteMeasurements(TwoWindingsTransformer twt) {
        DiscreteMeasurements<?> discreteMeasurements = getDiscreteMeasurements(twt);
        if (discreteMeasurements == null) {
            discreteMeasurements = (DiscreteMeasurements<?>) twt.newExtension(DiscreteMeasurementsAdder.class).add();
        }
        return discreteMeasurements;
    }

    private static List<DiscreteMeasurement> getTapPositionMeasurements(TwoWindingsTransformer twt, DiscreteMeasurement.TapChanger tapChanger) {
        DiscreteMeasurements<?> discreteMeasurements = getDiscreteMeasurements(twt);
        if (discreteMeasurements == null) {
            return List.of();
        }
        return discreteMeasurements.getDiscreteMeasurements(DiscreteMeasurement.Type.TAP_POSITION).stream()
                .filter(m -> m.getTapChanger() == tapChanger)
                .toList();
    }

    private static DiscreteMeasurement addTapPositionMeasurement(TwoWindingsTransformer twt, DiscreteMeasurement.TapChanger tapChanger,
                                                                 String id, Object value, boolean valid) {
        DiscreteMeasurementAdder adder = getOrCreateDiscreteMeasurements(twt).newDiscreteMeasurement()
                .setId(id)
                .setType(DiscreteMeasurement.Type.TAP_POSITION)
                .setTapChanger(tapChanger)
                .setValid(valid);
        if (value instanceof Integer intValue) {
            adder.setValue(intValue);
        } else if (value instanceof String stringValue) {
            adder.setValue(stringValue);
        }
        return adder.add();
    }

    private static void assertTapPositionMeasurement(TwoWindingsTransformer twt, DiscreteMeasurement.TapChanger tapChanger, int expectedValue) {
        List<DiscreteMeasurement> measurements = getTapPositionMeasurements(twt, tapChanger);
        assertEquals(1, measurements.size());
        DiscreteMeasurement measurement = measurements.get(0);
        assertEquals(DiscreteMeasurement.ValueType.INT, measurement.getValueType());
        assertEquals(expectedValue, measurement.getValueAsInt());
        assertTrue(measurement.isValid());
    }

    private static TwoWindingsTransformerModificationInfos tapPositionModification(Integer ratio, Integer phase) {
        return TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .ratioTapChangerPositionMeasurement(ratio == null ? null : new AttributeModification<>(ratio, OperationType.SET))
                .phaseTapChangerPositionMeasurement(phase == null ? null : new AttributeModification<>(phase, OperationType.SET))
                .build();
    }

    private ReportNode applyWithReport(ModificationInfos modificationInfos) {
        ReportNode report = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        modificationInfos.toModification().apply(getNetwork(), report);
        return report;
    }

    private static List<ReportNode> childrenWithKey(ReportNode node, String key) {
        return node.getChildren().stream().filter(c -> key.equals(c.getMessageKey())).toList();
    }

    private static String fieldNameOf(ReportNode node) {
        return node.getValue("fieldName").map(TypedValue::toString).orElseThrow();
    }

    private static String valueOf(ReportNode node, String key) {
        return node.getValue(key).map(TypedValue::toString).orElseThrow();
    }

    @Test
    void testCreateRatioTapPositionMeasurementWithoutExtension() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");
        assertNull(getDiscreteMeasurements(twt));

        tapPositionModification(5, null).toModification().apply(getNetwork());

        assertTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, 5);
        assertTrue(getTapPositionMeasurements(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER).isEmpty());
        assertEquals(1, getDiscreteMeasurements(twt).getDiscreteMeasurements().size());
        DiscreteMeasurement measurement = getTapPositionMeasurements(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER).get(0);
        assertDoesNotThrow(() -> UUID.fromString(measurement.getId()));
    }

    @Test
    void testCreatePhaseTapPositionMeasurementWithoutPhaseTapChanger() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");
        assertFalse(twt.hasPhaseTapChanger());

        tapPositionModification(null, 7).toModification().apply(getNetwork());

        assertTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, 7);
        assertTrue(getTapPositionMeasurements(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER).isEmpty());
        assertFalse(twt.hasPhaseTapChanger());
    }

    @Test
    void testCreateBothTapPositionMeasurementsWithExistingExtension() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");
        DiscreteMeasurement other = getOrCreateDiscreteMeasurements(twt).newDiscreteMeasurement()
                .setId("other")
                .setType(DiscreteMeasurement.Type.OTHER)
                .setValue("some value")
                .setValid(true)
                .add();
        other.putProperty("p", "v");

        tapPositionModification(2, 4).toModification().apply(getNetwork());

        assertTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, 2);
        assertTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, 4);
        assertEquals(3, getDiscreteMeasurements(twt).getDiscreteMeasurements().size());
        assertSame(other, getDiscreteMeasurements(twt).getDiscreteMeasurement("other"));
        assertEquals("some value", other.getValueAsString());
        assertEquals("v", other.getProperty("p"));
        assertNotEquals(getTapPositionMeasurements(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER).get(0).getId(),
                getTapPositionMeasurements(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER).get(0).getId());
    }

    @Test
    void testUpdateKeepsIdPropertiesAndOppositeMeasurement() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");
        DiscreteMeasurement ratio = addTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, "ratio-id", 1, true);
        ratio.putProperty("validity", "0").putProperty("custom", "x");
        DiscreteMeasurement phase = addTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, "phase-id", 9, true);
        phase.putProperty("custom", "y");

        tapPositionModification(6, null).toModification().apply(getNetwork());

        assertEquals(2, getDiscreteMeasurements(twt).getDiscreteMeasurements().size());
        assertSame(ratio, getDiscreteMeasurements(twt).getDiscreteMeasurement("ratio-id"));
        assertEquals(6, ratio.getValueAsInt());
        assertTrue(ratio.isValid());
        assertEquals("0", ratio.getProperty("validity"));
        assertEquals("x", ratio.getProperty("custom"));
        assertEquals(9, phase.getValueAsInt());
        assertEquals("y", phase.getProperty("custom"));

        tapPositionModification(null, 8).toModification().apply(getNetwork());

        assertEquals(2, getDiscreteMeasurements(twt).getDiscreteMeasurements().size());
        assertEquals(6, ratio.getValueAsInt());
        assertEquals(8, phase.getValueAsInt());
        assertEquals("phase-id", phase.getId());
    }

    @Test
    void testUpdateOnlyFirstMatchingMeasurement() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");
        DiscreteMeasurement first = addTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, "first", 1, true);
        DiscreteMeasurement second = addTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, "second", 2, true);

        tapPositionModification(10, null).toModification().apply(getNetwork());

        assertEquals(10, first.getValueAsInt());
        assertEquals(2, second.getValueAsInt());
        assertEquals(2, getDiscreteMeasurements(twt).getDiscreteMeasurements().size());
    }

    @Test
    void testInvalidMeasurementsBecomeValid() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");
        DiscreteMeasurement ratio = addTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, "ratio-id", 1, false);
        ratio.putProperty("validity", "1");
        DiscreteMeasurement phase = addTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, "phase-id", null, false);
        assertNull(phase.getValueAsString());

        tapPositionModification(3, 4).toModification().apply(getNetwork());

        assertTrue(ratio.isValid());
        assertEquals(3, ratio.getValueAsInt());
        // validity is a plain property: it is neither masked nor transformed
        assertEquals("1", ratio.getProperty("validity"));
        assertTrue(phase.isValid());
        assertEquals(DiscreteMeasurement.ValueType.INT, phase.getValueType());
        assertEquals(4, phase.getValueAsInt());
        assertEquals("phase-id", phase.getId());
    }

    @Test
    void testInvalidMeasurementWithNonIntegerValueIsOverwritten() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");
        DiscreteMeasurement ratio = addTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, "ratio-id", "not an int", false);

        ReportNode report = applyWithReport(tapPositionModification(11, null));

        assertTrue(ratio.isValid());
        assertEquals(DiscreteMeasurement.ValueType.INT, ratio.getValueType());
        assertEquals(11, ratio.getValueAsInt());
        ReportNode tapReport = childrenWithKey(childrenWithKey(report, "network.modification.StateEstimationData").get(0),
                "network.modification.tapPositionMeasurements").get(0);
        assertEquals(2, tapReport.getChildren().size());
        assertEquals("Ratio tap changer position measurement value", fieldNameOf(tapReport.getChildren().get(0)));
        assertEquals("not an int", valueOf(tapReport.getChildren().get(0), "oldValue"));
        assertEquals("11", valueOf(tapReport.getChildren().get(0), "newValue"));
        assertEquals("Ratio tap changer position measurement validity", fieldNameOf(tapReport.getChildren().get(1)));
    }

    @Test
    void testZeroAndNegativeValues() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");

        tapPositionModification(0, -4).toModification().apply(getNetwork());
        assertTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, 0);
        assertTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, -4);

        tapPositionModification(-1, 0).toModification().apply(getNetwork());
        assertTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, -1);
        assertTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, 0);
    }

    @Test
    void testNullValuesAreNoOpAndDoNotCreateExtension() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");

        tapPositionModification(null, null).toModification().apply(getNetwork());
        assertNull(getDiscreteMeasurements(twt));

        TwoWindingsTransformerModificationInfos nullValues = TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .ratioTapChangerPositionMeasurement(new AttributeModification<>(null, OperationType.SET))
                .phaseTapChangerPositionMeasurement(new AttributeModification<>(null, OperationType.UNSET))
                .build();
        ReportNode report = applyWithReport(nullValues);
        assertNull(getDiscreteMeasurements(twt));
        assertTrue(childrenWithKey(report, "network.modification.StateEstimationData").isEmpty());
        assertTrue(childrenWithKey(report, "network.modification.stateEstimationData").isEmpty());
    }

    @Test
    void testNullValuesDoNotDeleteOrChangeExistingMeasurements() {
        TwoWindingsTransformer twt = getNetwork().getTwoWindingsTransformer("trf1");
        DiscreteMeasurement ratio = addTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER, "ratio-id", 1, false);
        DiscreteMeasurement phase = addTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, "phase-id", 2, true);

        TwoWindingsTransformerModificationInfos nullValues = TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .ratioTapChangerPositionMeasurement(new AttributeModification<>(null, OperationType.UNSET))
                .phaseTapChangerPositionMeasurement(new AttributeModification<>(null, OperationType.SET))
                .build();
        nullValues.toModification().apply(getNetwork());

        assertEquals(2, getDiscreteMeasurements(twt).getDiscreteMeasurements().size());
        assertEquals(1, ratio.getValueAsInt());
        assertFalse(ratio.isValid());
        assertEquals(2, phase.getValueAsInt());
        assertTrue(phase.isValid());
    }

    @Test
    void testPhysicalTapPositionsAreNotChanged() {
        TwoWindingsTransformer twt = createPhaseTapChanger();
        int ratioTapPosition = twt.getRatioTapChanger() == null ? Integer.MIN_VALUE : twt.getRatioTapChanger().getTapPosition();
        int phaseTapPosition = twt.getPhaseTapChanger().getTapPosition();
        TwoWindingsTransformer trf1 = getNetwork().getTwoWindingsTransformer("trf1");
        int trf1RatioTapPosition = trf1.getRatioTapChanger().getTapPosition();

        TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf3")
                .ratioTapChangerPositionMeasurement(new AttributeModification<>(1, OperationType.SET))
                .phaseTapChangerPositionMeasurement(new AttributeModification<>(0, OperationType.SET))
                .build().toModification().apply(getNetwork());
        tapPositionModification(1, 1).toModification().apply(getNetwork());

        assertEquals(phaseTapPosition, twt.getPhaseTapChanger().getTapPosition());
        assertEquals(ratioTapPosition, twt.getRatioTapChanger() == null ? Integer.MIN_VALUE : twt.getRatioTapChanger().getTapPosition());
        assertEquals(trf1RatioTapPosition, trf1.getRatioTapChanger().getTapPosition());
        assertEquals(1, getTapPositionMeasurements(twt, DiscreteMeasurement.TapChanger.RATIO_TAP_CHANGER).size());
        assertTapPositionMeasurement(twt, DiscreteMeasurement.TapChanger.PHASE_TAP_CHANGER, 0);
    }

    @Test
    void testStandaloneTapPositionReport() {
        ReportNode report = applyWithReport(tapPositionModification(2, -3));

        List<ReportNode> stateEstimation = childrenWithKey(report, "network.modification.StateEstimationData");
        assertEquals(1, stateEstimation.size());
        assertEquals(1, stateEstimation.get(0).getChildren().size());
        ReportNode tapReport = stateEstimation.get(0).getChildren().get(0);
        assertEquals("network.modification.tapPositionMeasurements", tapReport.getMessageKey());
        assertEquals(2, tapReport.getChildren().size());
        assertEquals("Ratio tap changer position measurement value", fieldNameOf(tapReport.getChildren().get(0)));
        assertEquals("No value", valueOf(tapReport.getChildren().get(0), "oldValue"));
        assertEquals("2", valueOf(tapReport.getChildren().get(0), "newValue"));
        assertEquals("Phase tap changer position measurement value", fieldNameOf(tapReport.getChildren().get(1)));
        assertEquals("-3", valueOf(tapReport.getChildren().get(1), "newValue"));
        assertTrue(childrenWithKey(stateEstimation.get(0), "network.modification.twtToBeEstimated").isEmpty());
        assertTrue(childrenWithKey(stateEstimation.get(0), "network.modification.measurementsSide1").isEmpty());
    }

    @Test
    void testTapPositionReportIsEmittedWhenOnlyPhaseRequested() {
        ReportNode report = applyWithReport(tapPositionModification(null, 1));

        ReportNode tapReport = childrenWithKey(childrenWithKey(report, "network.modification.StateEstimationData").get(0),
                "network.modification.tapPositionMeasurements").get(0);
        assertEquals(1, tapReport.getChildren().size());
        assertEquals("Phase tap changer position measurement value", fieldNameOf(tapReport.getChildren().get(0)));
    }

    @Test
    void testCombinedStateEstimationReportHasSingleParent() {
        TwoWindingsTransformerModificationInfos infos = TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .p1MeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
                .ratioTapChangerToBeEstimated(new AttributeModification<>(true, OperationType.SET))
                .ratioTapChangerPositionMeasurement(new AttributeModification<>(4, OperationType.SET))
                .phaseTapChangerPositionMeasurement(new AttributeModification<>(5, OperationType.SET))
                .build();

        ReportNode report = applyWithReport(infos);

        assertEquals(1, childrenWithKey(report, "network.modification.stateEstimationData").size());
        assertTrue(childrenWithKey(report, "network.modification.StateEstimationData").isEmpty());
        ReportNode parent = childrenWithKey(report, "network.modification.stateEstimationData").get(0);
        assertEquals(1, childrenWithKey(parent, "network.modification.measurementsSide1").size());
        assertEquals(1, childrenWithKey(parent, "network.modification.tapPositionMeasurements").size());
        assertEquals(1, childrenWithKey(parent, "network.modification.twtToBeEstimated").size());
        assertEquals(3, parent.getChildren().size());
    }

    @Test
    void testTapPositionAndToBeEstimatedShareParentWithoutPq() {
        TwoWindingsTransformerModificationInfos infos = TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .phaseTapChangerToBeEstimated(new AttributeModification<>(true, OperationType.SET))
                .ratioTapChangerPositionMeasurement(new AttributeModification<>(4, OperationType.SET))
                .build();

        ReportNode report = applyWithReport(infos);

        assertEquals(1, childrenWithKey(report, "network.modification.StateEstimationData").size());
        ReportNode parent = childrenWithKey(report, "network.modification.StateEstimationData").get(0);
        assertEquals(1, childrenWithKey(parent, "network.modification.tapPositionMeasurements").size());
        assertEquals(1, childrenWithKey(parent, "network.modification.twtToBeEstimated").size());
        assertTrue(getNetwork().getTwoWindingsTransformer("trf1").getExtension(TwoWindingsTransformerToBeEstimated.class) != null);
    }

    @Test
    void testToBeEstimatedOnlyStillWorksWithoutTapPositionMeasurements() {
        TwoWindingsTransformerModificationInfos infos = TwoWindingsTransformerModificationInfos.builder()
                .equipmentId("trf1")
                .ratioTapChangerToBeEstimated(new AttributeModification<>(true, OperationType.SET))
                .build();

        ReportNode report = applyWithReport(infos);

        ReportNode parent = childrenWithKey(report, "network.modification.StateEstimationData").get(0);
        assertTrue(childrenWithKey(parent, "network.modification.tapPositionMeasurements").isEmpty());
        assertEquals(1, childrenWithKey(parent, "network.modification.twtToBeEstimated").size());
        assertNull(getDiscreteMeasurements(getNetwork().getTwoWindingsTransformer("trf1")));
    }

    @Test
    void testTapPositionMeasurementFieldsRoundTrip() throws Exception {
        TwoWindingsTransformerModificationInfos infos = tapPositionModification(-1, 12);

        TwoWindingsTransformerModification modification = (TwoWindingsTransformerModification) infos.toModification();
        assertEquals(-1, modification.getRatioTapChangerPositionMeasurement().getValue());
        assertEquals(12, modification.getPhaseTapChangerPositionMeasurement().getValue());

        TwoWindingsTransformerModificationInfos deserializedInfos = mapper.readValue(mapper.writeValueAsString(infos), TwoWindingsTransformerModificationInfos.class);
        assertEquals(-1, deserializedInfos.getRatioTapChangerPositionMeasurement().getValue());
        assertEquals(12, deserializedInfos.getPhaseTapChangerPositionMeasurement().getValue());

        AbstractModification deserialized = mapper.readValue(mapper.writeValueAsString(modification), AbstractModification.class);
        assertEquals(modification, deserialized);

        TwoWindingsTransformerModification empty = (TwoWindingsTransformerModification) tapPositionModification(null, null).toModification();
        assertNull(empty.getRatioTapChangerPositionMeasurement());
        assertNull(empty.getPhaseTapChangerPositionMeasurement());
    }
}
