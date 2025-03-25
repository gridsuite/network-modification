/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.LoadingLimits.TemporaryLimit;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ThreeSides;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import com.powsybl.iidm.network.extensions.MeasurementsAdder;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
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
                .voltageLevelId1(new AttributeModification<>("v1", OperationType.SET))
                .voltageLevelId2(new AttributeModification<>("v4", OperationType.SET))
                .busOrBusbarSectionId1(new AttributeModification<>("1B", OperationType.SET))
                .busOrBusbarSectionId2(new AttributeModification<>("2B", OperationType.SET))
                .connectionName1(new AttributeModification<>("cn1Line1", OperationType.SET))
                .connectionName2(new AttributeModification<>("cn2Line1", OperationType.SET))
                .connectionDirection1(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionDirection2(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionPosition1(new AttributeModification<>(1, OperationType.SET))
                .connectionPosition2(new AttributeModification<>(1, OperationType.SET))
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
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .p1MeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
                .p1MeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
                .p2MeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
                .p2MeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
                .q1MeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
                .q1MeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
                .q2MeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
                .q2MeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
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
        assertEquals(12.0, modifiedLine.getNullableCurrentLimits1().getPermanentLimit());
        TemporaryLimit temporaryLimit = modifiedLine.getNullableCurrentLimits1().getTemporaryLimit(Integer.MAX_VALUE);
        assertEquals(Integer.MAX_VALUE, temporaryLimit.getAcceptableDuration());
        assertEquals("name31", temporaryLimit.getName());
        assertEquals(Double.MAX_VALUE, temporaryLimit.getValue());
        assertEquals(22.0, modifiedLine.getNullableCurrentLimits2().getPermanentLimit());
        temporaryLimit = modifiedLine.getNullableCurrentLimits2().getTemporaryLimit(32);
        assertEquals(32, temporaryLimit.getAcceptableDuration());
        assertEquals("name32", temporaryLimit.getName());
        assertEquals(42.0, temporaryLimit.getValue());
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
    void testDisconnection() throws Exception {
        changeLineConnectionState(getNetwork().getLine("line1"), false);
    }

    @Test
    void testConnection() throws Exception {
        changeLineConnectionState(getNetwork().getLine("line1"), true);
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
}
