/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ValidationException;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.BatteryCreationModel;
import org.gridsuite.modification.model.FreePropertyModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.ReactiveCapabilityCurvePointsModel;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class BatteryCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    public void checkModification() {
        Network network = getNetwork();
        BatteryCreationModel batteryCreationModel = (BatteryCreationModel) buildModification();
        batteryCreationModel.setBusOrBusbarSectionId("notFoundBus");
        BatteryCreation batteryCreation = (BatteryCreation) batteryCreationModel.toModification();
        assertThrows(NetworkModificationException.class, () -> batteryCreation.check(network));

        BatteryCreationModel batteryCreationModel1 = BatteryCreationModel.builder()
            .equipmentId("v4Battery")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .droop(101f)
            .build();
        BatteryCreation batteryCreation1 = (BatteryCreation) batteryCreationModel1.toModification();
        String message = assertThrows(NetworkModificationException.class,
            () -> batteryCreation1.check(network)).getMessage();
        assertEquals("CREATE_BATTERY_ERROR : Battery 'v4Battery' : must have Droop between 0 and 100", message);

        BatteryCreationModel batteryCreationModel2 = BatteryCreationModel.builder()
            .equipmentId("v4Battery")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .droop(-1f)
            .build();
        BatteryCreation batteryCreation2 = (BatteryCreation) batteryCreationModel2.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> batteryCreation2.check(network)).getMessage();
        assertEquals("CREATE_BATTERY_ERROR : Battery 'v4Battery' : must have Droop between 0 and 100", message);

        BatteryCreationModel batteryCreationModel3 = BatteryCreationModel.builder()
            .equipmentId("v3Battery")
            .build();
        BatteryCreation batteryCreation3 = (BatteryCreation) batteryCreationModel3.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> batteryCreation3.check(network)).getMessage();
        assertEquals("BATTERY_ALREADY_EXISTS : v3Battery", message);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationModel buildModification() {
        // create new battery in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        return BatteryCreationModel.builder()
            .equipmentId("idBattery1")
            .equipmentName("idBattery1")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .minP(100.0)
            .maxP(600.0)
            .targetP(400.)
            .targetQ(50.)
            .minQ(20.0)
            .maxQ(25.0)
            .droop(5f)
            .stepUpTransformerX(60.0)
            .directTransX(61.0)
            .participate(true)
            .reactiveCapabilityCurve(true)
            .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsModel(2.0, 3.0, 3.1),
                new ReactiveCapabilityCurvePointsModel(5.6, 9.8, 10.8)))
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getBattery("idBattery1"));
        assertEquals(1, getNetwork().getVoltageLevel("v2").getBatteryStream()
            .filter(transformer -> transformer.getId().equals("idBattery1")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getBattery("idBattery1").getProperty(PROPERTY_NAME));
        Battery battery = getNetwork().getBattery("idBattery1");
        assertNotNull(battery.getExtension(ActivePowerControl.class));
        ActivePowerControl activePowerControl = battery.getExtension(ActivePowerControl.class);
        assertEquals(5, activePowerControl.getDroop());
        assertTrue(activePowerControl.isParticipate());
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // invalid Battery id
        BatteryCreationModel batteryCreationModel = (BatteryCreationModel) buildModification();
        batteryCreationModel.setEquipmentId("");
        PowsyblException exception = assertThrows(PowsyblException.class, () -> batteryCreationModel.toModification().apply(getNetwork()));
        assertEquals("Invalid id ''", exception.getMessage());

        // not found voltage level
        batteryCreationModel.setEquipmentId("idBattery1");
        batteryCreationModel.setVoltageLevelId("notFoundVoltageLevelId");
        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationModel.toModification().check(getNetwork()));
        assertEquals("VOLTAGE_LEVEL_NOT_FOUND : notFoundVoltageLevelId", exception.getMessage());

        // not found busbar section
        batteryCreationModel.setVoltageLevelId("v2");
        batteryCreationModel.setBusOrBusbarSectionId("notFoundBusbarSection");
        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationModel.toModification().check(getNetwork()));
        assertEquals("BUSBAR_SECTION_NOT_FOUND : notFoundBusbarSection", exception.getMessage());

        // invalid min active power
        batteryCreationModel.setVoltageLevelId("v2");

        batteryCreationModel.setBusOrBusbarSectionId("1B");
        batteryCreationModel.setMinP(Double.NaN);
        exception = assertThrows(ValidationException.class, () -> batteryCreationModel.toModification().apply(getNetwork()));
        assertEquals("Battery 'idBattery1': invalid value (NaN) for minimum P", exception.getMessage());

        // invalid min max reactive limit
        BatteryCreationModel batteryCreationModel1 = (BatteryCreationModel) buildModification();
        batteryCreationModel1.setReactiveCapabilityCurve(false);
        batteryCreationModel1.setMinQ(Double.NaN);

        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationModel1.toModification().check(getNetwork()));
        assertEquals("CREATE_BATTERY_ERROR : Battery 'idBattery1' : minimum reactive power is not set", exception.getMessage());

        BatteryCreationModel batteryCreationModel2 = (BatteryCreationModel) buildModification();
        batteryCreationModel2.setReactiveCapabilityCurve(false);
        batteryCreationModel2.setMaxQ(Double.NaN);

        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationModel2.toModification().check(getNetwork()));
        assertEquals("CREATE_BATTERY_ERROR : Battery 'idBattery1' : maximum reactive power is not set", exception.getMessage());

        batteryCreationModel2.setReactiveCapabilityCurve(false);
        batteryCreationModel2.setMinQ(200.);
        batteryCreationModel2.setMaxQ(100.);
        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationModel2.toModification().check(getNetwork()));
        assertEquals("CREATE_BATTERY_ERROR : Battery 'idBattery1' : maximum reactive power is expected to be greater than or equal to minimum reactive power", exception.getMessage());

        // invalid reactive capability curve limit
        BatteryCreationModel batteryCreationModel3 = (BatteryCreationModel) buildModification();
        batteryCreationModel3.getReactiveCapabilityCurvePoints().get(0).setP(Double.NaN);

        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationModel3.toModification().check(getNetwork()));
        assertEquals("CREATE_BATTERY_ERROR : Battery 'idBattery1' : P is not set in a reactive capability curve limits point", exception.getMessage());
        // try to create an existing battery
        BatteryCreationModel batteryCreationModel4 = (BatteryCreationModel) buildModification();
        batteryCreationModel4.setEquipmentId("v3Battery");
        exception = assertThrows(NetworkModificationException.class, () -> batteryCreationModel4.toModification().check(getNetwork()));
        assertEquals("BATTERY_ALREADY_EXISTS : v3Battery", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        // assertEquals("BATTERY_CREATION", modificationModel.getMessageType());
        // Map<String, String> updatedValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals("idBattery1", updatedValues.get("equipmentId"));
    }

    @Test
    void testCreateWithShortCircuitErrors() {
        // invalid short circuit transient reactance
        BatteryCreationModel batteryCreationModel = (BatteryCreationModel) buildModification();
        batteryCreationModel.setDirectTransX(Double.NaN);

        ReportNode report = batteryCreationModel.createSubReportNode(ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test").build());
        batteryCreationModel.toModification().apply(getNetwork(), report);
        assertLogMessage("cannot add short-circuit extension on battery with id=idBattery1 : Undefined directTransX", "network.modification.ShortCircuitExtensionAddError", report);
    }

    @Test
    void testCreateWithDroopNull() {
        Network network = getNetwork();
        BatteryCreationModel batteryCreationModel = BatteryCreationModel.builder()
            .equipmentId("idBattery3")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .minP(100.0)
            .maxP(600.0)
            .targetP(400.)
            .targetQ(50.)
            .minQ(20.0)
            .maxQ(25.0)
            .droop(null)
            .participate(false)
            .reactiveCapabilityCurve(false)
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        batteryCreationModel.toModification().apply(network);
        Battery battery = network.getBattery("idBattery3");
        assertNotNull(battery.getExtension(ActivePowerControl.class));
        ActivePowerControl activePowerControl = battery.getExtension(ActivePowerControl.class);
        assertEquals(Double.NaN, activePowerControl.getDroop());
        assertFalse(activePowerControl.isParticipate());

        BatteryCreationModel batteryCreationModel2 = BatteryCreationModel.builder()
            .equipmentId("idBattery4")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .minP(100.0)
            .maxP(600.0)
            .targetP(400.)
            .targetQ(50.)
            .minQ(20.0)
            .maxQ(25.0)
            .droop(5f)
            .participate(null)
            .reactiveCapabilityCurve(false)
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        batteryCreationModel2.toModification().apply(network);
        Battery battery2 = network.getBattery("idBattery4");
        assertNull(battery2.getExtension(ActivePowerControl.class));
    }
}
