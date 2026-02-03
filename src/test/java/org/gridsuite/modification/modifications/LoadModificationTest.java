/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ValidationException;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import com.powsybl.iidm.network.extensions.MeasurementsAdder;

import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LoadModificationTest extends AbstractInjectionModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final Double MEASUREMENT_P_VALUE = 10.0;
    private static final Double MEASUREMENT_Q_VALUE = -10.0;
    private static final Boolean MEASUREMENT_P_VALID = true;
    private static final Boolean MEASUREMENT_Q_VALID = false;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadModificationInfos.builder()
            .stashed(false)
            .equipmentId("v1load")
            .equipmentName(new AttributeModification<>("nameLoad1", OperationType.SET))
            .loadType(new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET))
            .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
            .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
            .p0(new AttributeModification<>(200.0, OperationType.SET))
            .q0(new AttributeModification<>(30.0, OperationType.SET))
            .pMeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
            .pMeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
            .qMeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
            .qMeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Load modifiedLoad = getNetwork().getLoad("v1load");
        assertNotNull(modifiedLoad);
        assertEquals(LoadType.FICTITIOUS, modifiedLoad.getLoadType());
        assertEquals(200.0, modifiedLoad.getP0(), 0.0);
        assertEquals(30.0, modifiedLoad.getQ0(), 0.0);
        assertEquals("nameLoad1", modifiedLoad.getNameOrId());
        assertEquals(PROPERTY_VALUE, modifiedLoad.getProperty(PROPERTY_NAME));
        assertMeasurements(modifiedLoad);
    }

    private void assertMeasurements(Load load) {
        Measurements<?> measurements = (Measurements<?>) load.getExtension(Measurements.class);
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
        // Unset an attribute that should not be null
        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
                .stashed(false)
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(null, OperationType.UNSET))
                .build();
        ValidationException exception = assertThrows(ValidationException.class, () -> loadModificationInfos.toModification().apply(getNetwork()));
        assertEquals("Load 'v1load': load type is null", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LOAD_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1load", createdValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        assertChangeConnectionState(getNetwork().getLoad("v1load"), false);
    }

    @Test
    void testConnection() throws Exception {
        assertChangeConnectionState(getNetwork().getLoad("v1load"), true);
    }

    @Test
    void testMeasurementUpsertCombinationsForLoad() {
        Load load = getNetwork().getLoad("v1load");
        Measurements<?> measurements = (Measurements<?>) load.getExtension(Measurements.class);
        if (measurements == null) {
            measurements = (Measurements<?>) load.newExtension(MeasurementsAdder.class).add();
        }

        Measurement activePowerMeasurement = measurements.newMeasurement()
            .setId(UUID.randomUUID().toString())
            .setType(Measurement.Type.ACTIVE_POWER)
            .setValue(5.0)
            .setValid(true)
            .add();
        activePowerMeasurement.putProperty("validity", "1");
        LoadModificationInfos updateActiveValidityTrue = LoadModificationInfos.builder()
            .equipmentId("v1load")
            .pMeasurementValidity(new AttributeModification<>(true, OperationType.SET))
            .build();
        updateActiveValidityTrue.toModification().apply(getNetwork());
        assertEquals("0", activePowerMeasurement.getProperty("validity"));

        activePowerMeasurement.putProperty("validity", "3");
        LoadModificationInfos updateActiveValidityTrueFromThree = LoadModificationInfos.builder()
            .equipmentId("v1load")
            .pMeasurementValidity(new AttributeModification<>(true, OperationType.SET))
            .build();
        updateActiveValidityTrueFromThree.toModification().apply(getNetwork());
        assertEquals("2", activePowerMeasurement.getProperty("validity"));

        Measurement reactivePowerMeasurement = measurements.newMeasurement()
            .setId(UUID.randomUUID().toString())
            .setType(Measurement.Type.REACTIVE_POWER)
            .setValue(-5.0)
            .setValid(false)
            .add();
        reactivePowerMeasurement.putProperty("validity", "0");
        LoadModificationInfos updateReactiveValidityFalse = LoadModificationInfos.builder()
            .equipmentId("v1load")
            .qMeasurementValidity(new AttributeModification<>(false, OperationType.SET))
            .build();
        updateReactiveValidityFalse.toModification().apply(getNetwork());
        assertEquals("1", reactivePowerMeasurement.getProperty("validity"));

        reactivePowerMeasurement.putProperty("validity", "2");
        LoadModificationInfos updateReactiveValidityFalseFromTwo = LoadModificationInfos.builder()
            .equipmentId("v1load")
            .qMeasurementValidity(new AttributeModification<>(false, OperationType.SET))
            .build();
        updateReactiveValidityFalseFromTwo.toModification().apply(getNetwork());
        assertEquals("3", reactivePowerMeasurement.getProperty("validity"));
    }
}
