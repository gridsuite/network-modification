/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createShuntCompensator;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 * @auther Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class ShuntCompensatorModificationTest extends AbstractInjectionModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected void checkModification() {
        var shuntCompensator = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("wrong id")
                .build();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> shuntCompensator.toModification().check(getNetwork()));
        assertEquals("Shunt compensator wrong id does not exist in network", exception.getMessage());

        // WrongMaximumSectionCount
        var shuntCompensator1 = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v5shunt")
                .sectionCount(new AttributeModification<>(3, OperationType.SET))
                .maximumSectionCount(new AttributeModification<>(-1, OperationType.SET))
                .build();
        exception = assertThrows(NetworkModificationException.class, () -> shuntCompensator1.toModification().check(getNetwork()));
        assertEquals("Unable to modify shunt compensator: Maximum section count should be greater or equal to 1", exception.getMessage());

        // testWrongSectionCount
        var shuntCompensator2 = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v5shunt")
                .sectionCount(new AttributeModification<>(3, OperationType.SET))
                .maximumSectionCount(new AttributeModification<>(1, OperationType.SET))
                .build();

        exception = assertThrows(NetworkModificationException.class, () -> shuntCompensator2.toModification().check(getNetwork()));
        assertEquals("Unable to modify shunt compensator: Section count should be between 0 and Maximum section count (1), actual : 3", exception.getMessage());

        // WrongSectionCountChangeSectionCount
        VoltageLevel v5 = getNetwork().getVoltageLevel("v5");
        createShuntCompensator(v5, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);

        var shuntCompensator3 = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator3.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        var shuntCompensatorModifications = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v7shunt")
                .sectionCount(new AttributeModification<>(3, OperationType.SET))
                .build();

        exception = assertThrows(NetworkModificationException.class, () -> shuntCompensatorModifications.toModification().check(getNetwork()));
        assertEquals("Unable to modify shunt compensator: Section count should be between 0 and Maximum section count (1), actual : 3", exception.getMessage());

        // WrongSectionCountChangeMaximumSectionCount
        var shuntCompensator4 = getNetwork().getShuntCompensator("v7shunt");
        var model1 = shuntCompensator4.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model1);

        var shuntCompensatorModifications1 = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v7shunt")
                .sectionCount(new AttributeModification<>(-1, OperationType.SET))
                .build();
        exception = assertThrows(NetworkModificationException.class, () -> shuntCompensatorModifications1.toModification().check(getNetwork()));
        assertEquals("Unable to modify shunt compensator: Section count should be between 0 and Maximum section count (1), actual : -1", exception.getMessage());

        // NegativeQmaxAtNominalV
        var shuntCompensator5 = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v5shunt")
                .maxQAtNominalV(new AttributeModification<>(-15.0, OperationType.SET))
                .build();
        exception = assertThrows(NetworkModificationException.class, () -> shuntCompensator5.toModification().apply(getNetwork()));
        assertEquals("Unable to modify shunt compensator: Qmax at nominal voltage should be greater or equal to 0", exception.getMessage());
    }

    @Test
    void testCreateModificationWithShuntCompensatorType() throws Exception {
        VoltageLevel v5 = getNetwork().getVoltageLevel("v5");
        createShuntCompensator(v5, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);

        var shuntCompensator = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        assertEquals(1.0, model.getBPerSection(), 0);
        ShuntCompensatorModificationInfos modificationInfos = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v7shunt")
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.REACTOR, OperationType.SET))
                .build();

        modificationInfos.toModification().apply(getNetwork());

        assertEquals(-1.0, model.getBPerSection(), 0);
    }

    @Test
    void testCreateModificationWithSusceptancePerSection() throws Exception {
        VoltageLevel v5 = getNetwork().getVoltageLevel("v5");
        createShuntCompensator(v5, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);

        var shuntCompensator = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        assertEquals(1.0, model.getBPerSection(), 0);
        ShuntCompensatorModificationInfos modificationInfos = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v7shunt")
                .maxSusceptance(AttributeModification.toAttributeModification(3.0, OperationType.SET))
                .build();

        modificationInfos.toModification().apply(getNetwork());

        assertEquals(3.0, model.getBPerSection(), 0);
    }

    @Test
    void testCreateModificationWithSections() throws Exception {
        var shuntCompensatorToModify = getNetwork().getShuntCompensator("v5shunt");
        var model = shuntCompensatorToModify.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        var shuntCompensator = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v5shunt")
                .maximumSectionCount(AttributeModification.toAttributeModification(3, OperationType.SET))
                .sectionCount(AttributeModification.toAttributeModification(2, OperationType.SET))
                .build();

        shuntCompensator.toModification().apply(getNetwork());

        assertEquals(3, shuntCompensatorToModify.getMaximumSectionCount());
        assertEquals(2, shuntCompensatorToModify.getSectionCount());
    }

    @Test
    void testCreateModificationWithQAtNominalV() throws Exception {
        VoltageLevel v5 = getNetwork().getVoltageLevel("v5");
        createShuntCompensator(v5, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);
        VoltageLevel v6 = getNetwork().getVoltageLevel("v6");
        createShuntCompensator(v6, "v8shunt", "v8shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v8shunt", 50, ConnectablePosition.Direction.BOTTOM);

        ShuntCompensatorModificationInfos modificationInfos1 = ShuntCompensatorModificationInfos.builder()
                        .stashed(false)
                        .equipmentId("v7shunt")
                        .maxQAtNominalV(new AttributeModification<>(30.5, OperationType.SET))
                        .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.REACTOR, OperationType.SET))
                        .build();

        ShuntCompensatorModificationInfos modificationInfos2 = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v8shunt")
                .maxQAtNominalV(new AttributeModification<>(30.5, OperationType.SET))
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.CAPACITOR, OperationType.SET))
                .build();

        modificationInfos1.toModification().apply(getNetwork());
        modificationInfos2.toModification().apply(getNetwork());

        var shuntCompensator1 = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator1.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(-2.1121E-4, model.getBPerSection(), 0.0001);

        var shuntCompensator2 = getNetwork().getShuntCompensator("v8shunt");
        var model2 = shuntCompensator2.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model2);
        assertEquals(2.1121E-4, model2.getBPerSection(), 0.0001);
    }

    @Override
    protected ModificationInfos buildModification() {
        VoltageLevel v2 = getNetwork().getVoltageLevel("v2");
        createShuntCompensator(v2, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);

        return ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v7shunt")
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.CAPACITOR, OperationType.SET))
                .maxQAtNominalV(new AttributeModification<>(15.0, OperationType.SET))
                .maximumSectionCount(new AttributeModification<>(1, OperationType.SET))
                .sectionCount(new AttributeModification<>(1, OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();

    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        var shuntCompensator = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(2.9629E-4, model.getBPerSection(), 0.0001);
        assertEquals(PROPERTY_VALUE, getNetwork().getShuntCompensator("v7shunt").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SHUNT_COMPENSATOR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v7shunt", createdValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        ShuntCompensatorModificationInfos shuntModificationInfos =
                ShuntCompensatorModificationInfos.builder()
                        .stashed(false)
                        .equipmentId("v2shunt")
                        .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
                        .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
                        .build();
        assertChangeConnectionState(getNetwork().getShuntCompensator("v2shunt"), shuntModificationInfos, false);
    }

    @Test
    void testConnection() throws Exception {
        ShuntCompensatorModificationInfos shuntModificationInfos =
                ShuntCompensatorModificationInfos.builder()
                        .stashed(false)
                        .equipmentId("v2shunt")
                        .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
                        .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
                        .build();
        assertChangeConnectionState(getNetwork().getShuntCompensator("v2shunt"), shuntModificationInfos, true);
    }
}
