/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import com.powsybl.iidm.network.extensions.MeasurementsAdder;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.*;
import static org.gridsuite.modification.dto.OperationalLimitsGroupModificationType.DELETE;
import static org.gridsuite.modification.dto.OperationalLimitsGroupModificationType.MODIFY_OR_ADD;
import static org.gridsuite.modification.dto.OperationalLimitsGroupModificationType.REPLACE;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LineModificationTest extends AbstractNetworkModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final Double MEASUREMENT_P_VALUE = 10.0;
    private static final Double MEASUREMENT_Q_VALUE = -10.0;
    private static final Boolean MEASUREMENT_P_VALID = true;
    private static final Boolean MEASUREMENT_Q_VALID = false;

    private static final String OLG_PROP1_NAME = "olgProperyName1";
    private static final String OLG_PROP1_VALUE = "olgProperyValue1";
    private static final String OLG_PROP2_NAME = "olgProperyName2";
    private static final String OLG_PROP2_VALUE = "olgProperyValue2";
    private static final String OLG_PROP3_NAME = "olgProperyName3";
    private static final String OLG_PROP3_VALUE = "olgProperyValue3";
    private static final String OTHER_VALUE = "OtherValue";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network n = NetworkCreation.create(networkUuid, true);
        // add Measurements extension on ONE side
        Line line1 = n.getLine("line1");
        MeasurementsAdder<?> measurementsAdder = line1.newExtension(MeasurementsAdder.class);
        Measurements<?> measurements = measurementsAdder.add();
        measurements.newMeasurement().setId(UUID.randomUUID().toString())
                .setType(Measurement.Type.ACTIVE_POWER).setSide(ThreeSides.ONE).setValue(MEASUREMENT_P_VALUE).setValid(MEASUREMENT_P_VALID)
                .add();
        measurements.newMeasurement().setId(UUID.randomUUID().toString())
                .setType(Measurement.Type.REACTIVE_POWER).setSide(ThreeSides.ONE).setValue(MEASUREMENT_Q_VALUE).setValid(MEASUREMENT_Q_VALID)
                .add();
        return n;
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineModificationInfos.builder()
                .stashed(false)
                .equipmentId("line1")
                .equipmentName(new AttributeModification<>("LineModified", OperationType.SET))
                .connectionName1(new AttributeModification<>("cn1Line1", OperationType.SET))
                .connectionName2(new AttributeModification<>("cn2Line1", OperationType.SET))
                .connectionDirection1(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionDirection2(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionPosition1(new AttributeModification<>(1, OperationType.SET))
                .connectionPosition2(new AttributeModification<>(1, OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .p1MeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
                .p1MeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
                .p2MeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
                .p2MeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
                .q1MeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
                .q1MeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
                .q2MeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
                .q2MeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
                .enableOLGModification(false)
                .selectedOperationalLimitsGroup1(new AttributeModification<>("invalid_opLG", OperationType.SET))
                .selectedOperationalLimitsGroup2(new AttributeModification<>("invalid_opLG", OperationType.SET))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Line modifiedLine = getNetwork().getLine("line1");
        assertEquals("LineModified", modifiedLine.getNameOrId());
        assertEquals(1.0, modifiedLine.getR());
        assertEquals(1.0, modifiedLine.getX());
        assertEquals(1.0, modifiedLine.getG1());
        assertEquals(1.0, modifiedLine.getB1());
        assertEquals(2.0, modifiedLine.getG2());
        assertEquals(2.0, modifiedLine.getB2());
        assertEquals(PROPERTY_VALUE, modifiedLine.getProperty(PROPERTY_NAME));
        assertMeasurements(modifiedLine);
    }

    private void assertMeasurements(Line line) {
        Measurements<?> measurements = (Measurements<?>) line.getExtension(Measurements.class);
        assertNotNull(measurements);
        Collection<Measurement> activePowerMeasurements = measurements.getMeasurements(Measurement.Type.ACTIVE_POWER).stream().toList();
        assertFalse(CollectionUtils.isEmpty(activePowerMeasurements));
        assertThat(activePowerMeasurements).allMatch(m -> m.getValue() == MEASUREMENT_P_VALUE && m.isValid() == MEASUREMENT_P_VALID);
        Collection<Measurement> reactivePowerMeasurements = measurements.getMeasurements(Measurement.Type.REACTIVE_POWER).stream().toList();
        assertFalse(CollectionUtils.isEmpty(reactivePowerMeasurements));
        assertThat(reactivePowerMeasurements).allMatch(m -> m.getValue() == MEASUREMENT_Q_VALUE && m.isValid() == MEASUREMENT_Q_VALID);
    }

    @Override
    protected void checkModification() {
        Network network = getNetwork();
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();
        lineModificationInfos.setEquipmentId("lineNotFound");
        LineModification lineModification = (LineModification) lineModificationInfos.toModification();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> lineModification.check(network));
        assertEquals(new NetworkModificationException(LINE_NOT_FOUND, "Line 'lineNotFound' : does not exist in network").getMessage(),
                exception.getMessage());
        LineModificationInfos lineModificationInfos1 = LineModificationInfos.builder()
            .equipmentId("line1")
            .r(new AttributeModification<>(-1d, OperationType.SET))
            .build();
        LineModification lineModification1 = (LineModification) lineModificationInfos1.toModification();
        String message = assertThrows(NetworkModificationException.class,
            () -> lineModification1.check(network)).getMessage();
        assertEquals("MODIFY_LINE_ERROR : Line 'line1' : can not have a negative value for Resistance R", message);

        LineModificationInfos lineModificationInfos2 = LineModificationInfos.builder()
            .equipmentId("line1")
            .g1(new AttributeModification<>(-2d, OperationType.SET))
            .build();
        LineModification lineModification2 = (LineModification) lineModificationInfos2.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> lineModification2.check(network)).getMessage();
        assertEquals("MODIFY_LINE_ERROR : Line 'line1' : can not have a negative value for Conductance on side 1 G1", message);

        LineModificationInfos lineModificationInfos3 = LineModificationInfos.builder()
            .equipmentId("line1")
            .g2(new AttributeModification<>(-100d, OperationType.SET))
            .build();
        LineModification lineModification3 = (LineModification) lineModificationInfos3.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> lineModification3.check(network)).getMessage();
        assertEquals("MODIFY_LINE_ERROR : Line 'line1' : can not have a negative value for Conductance on side 2 G2", message);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line1", createdValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() {
        changeLineConnectionState(getNetwork().getLine("line1"), false);
    }

    @Test
    void testConnection() {
        changeLineConnectionState(getNetwork().getLine("line1"), true);
    }

    @Test
    void testApplicabilityChangeFromSide1ToSide2() {
        Line line = getNetwork().getLine("line1");
        OperationalLimitsGroup limitsGroup = line.newOperationalLimitsGroup1("NewLimitsGroup1");
        limitsGroup.newCurrentLimits()
            .setPermanentLimit(10.0)
            .beginTemporaryLimit().setName("IT10").setValue(15.0).setAcceptableDuration(600).endTemporaryLimit()
            .add();

        assertNotNull(line.getOperationalLimitsGroup1("NewLimitsGroup1"));
        assertTrue(line.getOperationalLimitsGroup2("NewLimitsGroup1").isEmpty());

        // Change from Side 1 -> Side 2
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos = OperationalLimitsGroupModificationInfos.builder()
            .id("NewLimitsGroup1").applicability(SIDE2).modificationType(MODIFY_OR_ADD).build();
        opLimitsGroupInfos.setCurrentLimits(new CurrentLimitsModificationInfos());
        LineModificationInfos lineModificationInfos = LineModificationInfos.builder()
            .equipmentId("line1")
            .enableOLGModification(true)
            .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos)).build();

        lineModificationInfos.toModification().apply(getNetwork());
        assertTrue(line.getOperationalLimitsGroup1("NewLimitsGroup1").isEmpty());
        assertNotNull(line.getOperationalLimitsGroup2("NewLimitsGroup1"));

        // Test on the copy of the limit set
        OperationalLimitsGroup opLimitsGroupOnLine = line.getOperationalLimitsGroup2("NewLimitsGroup1").get();
        assertTrue(opLimitsGroupOnLine.getCurrentLimits().isPresent());
        CurrentLimits currentLimits = opLimitsGroupOnLine.getCurrentLimits().get();
        assertEquals(10.0, currentLimits.getPermanentLimit());
        assertNotNull(currentLimits.getTemporaryLimit(600));
        LoadingLimits.TemporaryLimit temporaryLimit = currentLimits.getTemporaryLimit(600);
        assertEquals(15.0, temporaryLimit.getValue());
        assertEquals(600, temporaryLimit.getAcceptableDuration());
        assertEquals("IT10", temporaryLimit.getName());
    }

    @Test
    void testApplicabilityChangeFromSide2ToEquipment() {
        Line line = getNetwork().getLine("line1");
        // Change from Side 2 -> Equipment
        line.newOperationalLimitsGroup2("NewLimitsGroup2").newCurrentLimits()
                .setPermanentLimit(10.0)
                .beginTemporaryLimit().setName("IT10").setValue(15.0).setAcceptableDuration(600).endTemporaryLimit()
                .add();

        OperationalLimitsGroupModificationInfos opLimitsGroupInfos2 = OperationalLimitsGroupModificationInfos.builder()
                .id("NewLimitsGroup2").applicability(EQUIPMENT).modificationType(MODIFY_OR_ADD).build();
        opLimitsGroupInfos2.setCurrentLimits(new CurrentLimitsModificationInfos());

        LineModificationInfos lineModificationInfos2 = LineModificationInfos.builder()
                .equipmentId("line1")
                .enableOLGModification(true)
                .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos2)).build();
        lineModificationInfos2.toModification().apply(getNetwork());

        lineModificationInfos2.toModification().apply(getNetwork());
        assertNotNull(line.getOperationalLimitsGroup1("NewLimitsGroup2"));
        assertNotNull(line.getOperationalLimitsGroup2("NewLimitsGroup2"));
    }

    @Test
    void testApplicabilityChangeFromEquipmentToSide2() {
        Line line = getNetwork().getLine("line1");
        line.newOperationalLimitsGroup1("NewLimitsGroup3").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();
        line.newOperationalLimitsGroup2("NewLimitsGroup3").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos3 = OperationalLimitsGroupModificationInfos.builder()
                .id("NewLimitsGroup3").applicability(SIDE2).modificationType(MODIFY_OR_ADD).build();
        opLimitsGroupInfos3.setCurrentLimits(new CurrentLimitsModificationInfos());
        LineModificationInfos lineModificationInfos3 = LineModificationInfos.builder()
                .enableOLGModification(true)
                .equipmentId("line1")
                .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos3)).build();

        lineModificationInfos3.toModification().apply(getNetwork());
        assertNotNull(line.getOperationalLimitsGroup2("NewLimitsGroup3"));
        assertTrue(line.getOperationalLimitsGroup1("NewLimitsGroup3").isEmpty());
    }

    @Test
    void testApplicabilityChangeFromSide2ToSide1() {
        Line line = getNetwork().getLine("line1");
        line.newOperationalLimitsGroup2("NewLimitsGroup4").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos4 = OperationalLimitsGroupModificationInfos.builder()
                .id("NewLimitsGroup4").applicability(SIDE2).modificationType(MODIFY_OR_ADD).build();
        opLimitsGroupInfos4.setCurrentLimits(new CurrentLimitsModificationInfos());
        LineModificationInfos lineModificationInfos4 = LineModificationInfos.builder()
                .enableOLGModification(true)
                .equipmentId("line1")
                .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos4)).build();

        lineModificationInfos4.toModification().apply(getNetwork());
        assertNotNull(line.getOperationalLimitsGroup1("NewLimitsGroup4"));
        assertTrue(line.getOperationalLimitsGroup1("NewLimitsGroup4").isEmpty());
    }

    @Test
    void testApplicabilityChangeFromSide1ToEquipment() {
        Line line = getNetwork().getLine("line1");
        line.newOperationalLimitsGroup1("NewLimitsGroup5").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos5 = OperationalLimitsGroupModificationInfos.builder()
                .id("NewLimitsGroup5").applicability(EQUIPMENT).modificationType(MODIFY_OR_ADD).build();
        opLimitsGroupInfos5.setCurrentLimits(new CurrentLimitsModificationInfos());

        LineModificationInfos lineModificationInfos5 = LineModificationInfos.builder()
                .equipmentId("line1")
                .enableOLGModification(true)
                .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos5)).build();
        lineModificationInfos5.toModification().apply(getNetwork());

        lineModificationInfos5.toModification().apply(getNetwork());
        assertNotNull(line.getOperationalLimitsGroup1("NewLimitsGroup5"));
        assertNotNull(line.getOperationalLimitsGroup2("NewLimitsGroup5"));
    }

    @Test
    void testApplicabilityChangeFromEquipmentToSide1() {
        Line line = getNetwork().getLine("line1");
        line.newOperationalLimitsGroup1("NewLimitsGroup5").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();
        line.newOperationalLimitsGroup2("NewLimitsGroup5").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos5 = OperationalLimitsGroupModificationInfos.builder()
                .id("NewLimitsGroup5").applicability(SIDE1).modificationType(MODIFY_OR_ADD).build();
        opLimitsGroupInfos5.setCurrentLimits(new CurrentLimitsModificationInfos());

        LineModificationInfos lineModificationInfos5 = LineModificationInfos.builder()
                .equipmentId("line1")
                .operationalLimitsGroupsModificationType(OperationalLimitsGroupsModificationType.REPLACE)
                .enableOLGModification(true)
                .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos5)).build();
        lineModificationInfos5.toModification().apply(getNetwork());

        lineModificationInfos5.toModification().apply(getNetwork());
        assertNotNull(line.getOperationalLimitsGroup1("NewLimitsGroup5"));
        assertTrue(line.getOperationalLimitsGroup2("NewLimitsGroup5").isEmpty());
    }

    @Test
    void testConnectWhenNoSwitchesOpened() {
        getNetwork().getSwitch("v3dl1").setOpen(true);
        getNetwork().getSwitch("v3bl1").setOpen(true);
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> changeLineConnectionState(getNetwork().getLine("line1"), true));
        assertEquals("BRANCH_MODIFICATION_ERROR : Could not connect equipment 'line1' on side 1", exception.getMessage());
        getNetwork().getSwitch("v3dl1").setOpen(false);
        getNetwork().getSwitch("v3bl1").setOpen(false);
        getNetwork().getSwitch("v4dl1").setOpen(true);
        getNetwork().getSwitch("v4bl1").setOpen(true);
        exception = assertThrows(NetworkModificationException.class, () -> changeLineConnectionState(getNetwork().getLine("line1"), true));
        assertEquals("BRANCH_MODIFICATION_ERROR : Could not connect equipment 'line1' on side 2", exception.getMessage());
        getNetwork().getSwitch("v3dl1").setOpen(true);
        getNetwork().getSwitch("v3bl1").setOpen(true);
        exception = assertThrows(NetworkModificationException.class, () -> changeLineConnectionState(getNetwork().getLine("line1"), true));
        assertEquals("BRANCH_MODIFICATION_ERROR : Could not connect equipment 'line1' on side 1 & 2", exception.getMessage());
    }

    @Test
    void testDelete() {
        Line line = getNetwork().getLine("line1");
        // side 1
        line.newOperationalLimitsGroup1("NewLimitsGroup1").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();
        line.newOperationalLimitsGroup1("NewLimitsGroup2").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();
        // side 2
        line.newOperationalLimitsGroup2("NewLimitsGroup1").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();
        line.newOperationalLimitsGroup2("NewLimitsGroup3").newCurrentLimits()
                .setPermanentLimit(10.0)
                .add();

        // modification 1 remove olg on both side
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos1 = OperationalLimitsGroupModificationInfos.builder()
                .id("NewLimitsGroup1").applicability(EQUIPMENT).modificationType(DELETE).build();
        LineModificationInfos lineModificationInfos1 = LineModificationInfos.builder()
                .enableOLGModification(true)
                .equipmentId("line1")
                .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos1)).build();
        lineModificationInfos1.toModification().apply(getNetwork());

        assertTrue(line.getOperationalLimitsGroup1("NewLimitsGroup1").isEmpty());
        assertTrue(line.getOperationalLimitsGroup2("NewLimitsGroup1").isEmpty());
        assertTrue(line.getOperationalLimitsGroup1("NewLimitsGroup2").isPresent());
        assertTrue(line.getOperationalLimitsGroup2("NewLimitsGroup3").isPresent());

        // modification 2 remove olg on side one
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos2 = OperationalLimitsGroupModificationInfos.builder()
                .id("NewLimitsGroup2").applicability(SIDE1).modificationType(DELETE).build();
        LineModificationInfos lineModificationInfos2 = LineModificationInfos.builder()
                .enableOLGModification(true)
                .equipmentId("line1")
                .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos2)).build();
        lineModificationInfos2.toModification().apply(getNetwork());

        assertTrue(line.getOperationalLimitsGroup1("NewLimitsGroup1").isEmpty());
        assertTrue(line.getOperationalLimitsGroup2("NewLimitsGroup1").isEmpty());
        assertTrue(line.getOperationalLimitsGroup1("NewLimitsGroup2").isEmpty());
        assertTrue(line.getOperationalLimitsGroup2("NewLimitsGroup3").isPresent());

        // modification 3 remove olg on side two
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos3 = OperationalLimitsGroupModificationInfos.builder()
                .id("NewLimitsGroup3").applicability(SIDE2).modificationType(DELETE).build();
        LineModificationInfos lineModificationInfos3 = LineModificationInfos.builder()
                .enableOLGModification(true)
                .equipmentId("line1")
                .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos3)).build();
        lineModificationInfos3.toModification().apply(getNetwork());

        assertTrue(line.getOperationalLimitsGroup1("NewLimitsGroup1").isEmpty());
        assertTrue(line.getOperationalLimitsGroup2("NewLimitsGroup1").isEmpty());
        assertTrue(line.getOperationalLimitsGroup1("NewLimitsGroup2").isEmpty());
        assertTrue(line.getOperationalLimitsGroup2("NewLimitsGroup3").isEmpty());

        // try to remove not existing group
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos4 = OperationalLimitsGroupModificationInfos.builder()
                .id("doesNotExist").applicability(SIDE2).modificationType(DELETE).build();
        LineModificationInfos lineModificationInfos4 = LineModificationInfos.builder()
                .enableOLGModification(true)
                .equipmentId("line1")
                .operationalLimitsGroups(Collections.singletonList(opLimitsGroupInfos4)).build();
        Network network = getNetwork();
        AbstractModification modification = lineModificationInfos4.toModification();
        String errorMessage = assertThrows(PowsyblException.class, () -> modification.apply(network)).getMessage();
        assertEquals("Cannot delete operational limit group doesNotExist which has not been found in equipment on side SIDE2", errorMessage);
    }

    private void changeLineConnectionState(Line existingEquipment, boolean expectedState) {
        LineModificationInfos modificationInfos = (LineModificationInfos) buildModification();
        modificationInfos.setTerminal1Connected(new AttributeModification<>(expectedState, OperationType.SET));
        modificationInfos.setTerminal2Connected(new AttributeModification<>(expectedState, OperationType.SET));

        if (expectedState) {
            if (existingEquipment.getTerminal1().isConnected()) {
                existingEquipment.getTerminal1().disconnect();
            }
            if (existingEquipment.getTerminal2().isConnected()) {
                existingEquipment.getTerminal2().disconnect();
            }
        } else {
            if (!existingEquipment.getTerminal1().isConnected()) {
                existingEquipment.getTerminal1().connect();
            }
            if (!existingEquipment.getTerminal2().isConnected()) {
                existingEquipment.getTerminal2().connect();
            }
        }
        assertThat(existingEquipment.getTerminal1().isConnected()).isNotEqualTo(expectedState);
        assertThat(existingEquipment.getTerminal2().isConnected()).isNotEqualTo(expectedState);

        modificationInfos.toModification().apply(getNetwork());
        // connection state has changed as expected
        assertThat(existingEquipment.getTerminal1().isConnected()).isEqualTo(expectedState);
        assertThat(existingEquipment.getTerminal2().isConnected()).isEqualTo(expectedState);

        // try to modify again => no change on connection state
        modificationInfos.toModification().apply(getNetwork());
        assertThat(existingEquipment.getTerminal1().isConnected()).isEqualTo(expectedState);
        assertThat(existingEquipment.getTerminal2().isConnected()).isEqualTo(expectedState);
    }

    @Test
    void testLimitsPropertiesModificationDifferentOperationalLimits() {
        Line line = getNetwork().getLine("line1");
        OperationalLimitsGroup limitsGroup1 = line.newOperationalLimitsGroup1("NewLimitsGroup");
        limitsGroup1.newCurrentLimits().setPermanentLimit(10.0).add();
        limitsGroup1.setProperty(OLG_PROP1_NAME, OLG_PROP1_VALUE);

        OperationalLimitsGroup limitsGroup2 = line.newOperationalLimitsGroup2("NewLimitsGroup");
        limitsGroup2.newCurrentLimits().setPermanentLimit(10.0).add();
        limitsGroup2.setProperty(OLG_PROP1_NAME, OLG_PROP1_VALUE);
        limitsGroup2.setProperty(OLG_PROP2_NAME, OLG_PROP2_VALUE);

        // We modify the list of properties on side 1 to add new properties
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos1 = OperationalLimitsGroupModificationInfos.builder()
            .id("NewLimitsGroup").applicability(SIDE1).modificationType(MODIFY_OR_ADD)
            .currentLimits(CurrentLimitsModificationInfos.builder().permanentLimit(10.0).build())
            .limitsProperties(List.of(new LimitsPropertyInfos(OLG_PROP2_NAME, OLG_PROP2_VALUE))).build();

        OperationalLimitsGroupModificationInfos opLimitsGroupInfos2 = OperationalLimitsGroupModificationInfos.builder()
            .id("NewLimitsGroup").applicability(SIDE2).modificationType(MODIFY_OR_ADD)
            .currentLimits(CurrentLimitsModificationInfos.builder().permanentLimit(10.0).build())
            .limitsProperties(List.of(new LimitsPropertyInfos(OLG_PROP1_NAME, OTHER_VALUE),
                new LimitsPropertyInfos(OLG_PROP3_NAME, OLG_PROP3_VALUE))).build();

        LineModificationInfos lineModificationInfos1 = LineModificationInfos.builder()
            .equipmentId("line1")
            .enableOLGModification(true)
            .operationalLimitsGroupsModificationType(OperationalLimitsGroupsModificationType.REPLACE)
            .operationalLimitsGroups(List.of(opLimitsGroupInfos1, opLimitsGroupInfos2)).build();
        lineModificationInfos1.toModification().apply(getNetwork());

        assertEquals(1, limitsGroup1.getPropertyNames().size());
        assertTrue(limitsGroup1.hasProperty(OLG_PROP2_NAME));
        assertFalse(limitsGroup1.hasProperty(OLG_PROP1_NAME));
        assertEquals(OLG_PROP2_VALUE, limitsGroup1.getProperty(OLG_PROP2_NAME));

        assertEquals(2, limitsGroup2.getPropertyNames().size());
        assertTrue(limitsGroup2.hasProperty(OLG_PROP1_NAME));
        assertTrue(limitsGroup2.hasProperty(OLG_PROP3_NAME));
        assertFalse(limitsGroup2.hasProperty(OLG_PROP2_NAME));
        assertEquals(OTHER_VALUE, limitsGroup2.getProperty(OLG_PROP1_NAME));
        assertEquals(OLG_PROP3_VALUE, limitsGroup2.getProperty(OLG_PROP3_NAME));
    }

    @Test
    void testLimitsPropertiesModificationSameOperationalLimits() {
        Line line = getNetwork().getLine("line1");
        OperationalLimitsGroup limitsGroup1 = line.newOperationalLimitsGroup1("NewLimitsGroup");
        limitsGroup1.newCurrentLimits().setPermanentLimit(10.0).add();
        limitsGroup1.setProperty(OLG_PROP1_NAME, OLG_PROP1_VALUE);

        OperationalLimitsGroup limitsGroup2 = line.newOperationalLimitsGroup2("NewLimitsGroup");
        limitsGroup2.newCurrentLimits().setPermanentLimit(10.0).add();
        limitsGroup2.setProperty(OLG_PROP1_NAME, OLG_PROP1_VALUE);

        // We modify the list of properties on side 1 to add new properties
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos = OperationalLimitsGroupModificationInfos.builder()
            .id("NewLimitsGroup").applicability(EQUIPMENT).modificationType(MODIFY_OR_ADD)
            .currentLimits(CurrentLimitsModificationInfos.builder().permanentLimit(10.0).build())
            .limitsProperties(List.of(new LimitsPropertyInfos(OLG_PROP2_NAME, OLG_PROP2_VALUE),
                new LimitsPropertyInfos(OLG_PROP3_NAME, OLG_PROP3_VALUE))).build();

        LineModificationInfos lineModificationInfos = LineModificationInfos.builder()
            .equipmentId("line1")
            .enableOLGModification(true)
            .operationalLimitsGroups(List.of(opLimitsGroupInfos)).build();
        lineModificationInfos.toModification().apply(getNetwork());

        assertEquals(2, limitsGroup1.getPropertyNames().size());
        assertTrue(limitsGroup1.hasProperty(OLG_PROP2_NAME));
        assertTrue(limitsGroup1.hasProperty(OLG_PROP3_NAME));
        assertFalse(limitsGroup1.hasProperty(OLG_PROP1_NAME));
        assertEquals(OLG_PROP2_VALUE, limitsGroup1.getProperty(OLG_PROP2_NAME));
        assertEquals(OLG_PROP3_VALUE, limitsGroup1.getProperty(OLG_PROP3_NAME));

        assertEquals(2, limitsGroup2.getPropertyNames().size());
        assertTrue(limitsGroup2.hasProperty(OLG_PROP2_NAME));
        assertTrue(limitsGroup2.hasProperty(OLG_PROP3_NAME));
        assertFalse(limitsGroup2.hasProperty(OLG_PROP1_NAME));
        assertEquals(OLG_PROP2_VALUE, limitsGroup2.getProperty(OLG_PROP2_NAME));
        assertEquals(OLG_PROP3_VALUE, limitsGroup2.getProperty(OLG_PROP3_NAME));
    }

    @Test
    void testLimitsPropertiesModificationSameOperationalLimitsReplace() {
        Line line = getNetwork().getLine("line1");
        OperationalLimitsGroup limitsGroup1 = line.newOperationalLimitsGroup1("NewLimitsGroup");
        limitsGroup1.newCurrentLimits().setPermanentLimit(10.0).add();
        limitsGroup1.setProperty(OLG_PROP1_NAME, OLG_PROP1_VALUE);

        OperationalLimitsGroup limitsGroup2 = line.newOperationalLimitsGroup2("NewLimitsGroup");
        limitsGroup2.newCurrentLimits().setPermanentLimit(10.0).add();
        limitsGroup2.setProperty(OLG_PROP1_NAME, OLG_PROP1_VALUE);

        // We modify the list of properties on side 1 to add new properties
        OperationalLimitsGroupModificationInfos opLimitsGroupInfos = OperationalLimitsGroupModificationInfos.builder()
            .id("NewLimitsGroup").applicability(EQUIPMENT).modificationType(REPLACE)
            .currentLimits(CurrentLimitsModificationInfos.builder().permanentLimit(10.0).build())
            .limitsProperties(List.of(new LimitsPropertyInfos(OLG_PROP2_NAME, OLG_PROP2_VALUE),
                new LimitsPropertyInfos(OLG_PROP3_NAME, OLG_PROP3_VALUE))).build();

        LineModificationInfos lineModificationInfos = LineModificationInfos.builder()
            .equipmentId("line1")
            .enableOLGModification(true)
            .operationalLimitsGroups(List.of(opLimitsGroupInfos)).build();
        lineModificationInfos.toModification().apply(getNetwork());

        OperationalLimitsGroup repLimitsGroup1 = line.getOperationalLimitsGroup1("NewLimitsGroup").orElse(null);
        assertNotNull(repLimitsGroup1);
        assertEquals(2, repLimitsGroup1.getPropertyNames().size());
        assertTrue(repLimitsGroup1.hasProperty(OLG_PROP2_NAME));
        assertTrue(repLimitsGroup1.hasProperty(OLG_PROP3_NAME));
        assertFalse(repLimitsGroup1.hasProperty(OLG_PROP1_NAME));
        assertEquals(OLG_PROP2_VALUE, repLimitsGroup1.getProperty(OLG_PROP2_NAME));
        assertEquals(OLG_PROP3_VALUE, repLimitsGroup1.getProperty(OLG_PROP3_NAME));

        OperationalLimitsGroup repLimitsGroup2 = line.getOperationalLimitsGroup2("NewLimitsGroup").orElse(null);
        assertNotNull(repLimitsGroup2);
        assertEquals(2, repLimitsGroup2.getPropertyNames().size());
        assertTrue(repLimitsGroup2.hasProperty(OLG_PROP2_NAME));
        assertTrue(repLimitsGroup2.hasProperty(OLG_PROP3_NAME));
        assertFalse(repLimitsGroup2.hasProperty(OLG_PROP1_NAME));
        assertEquals(OLG_PROP2_VALUE, repLimitsGroup2.getProperty(OLG_PROP2_NAME));
        assertEquals(OLG_PROP3_VALUE, repLimitsGroup2.getProperty(OLG_PROP3_NAME));
    }

}
