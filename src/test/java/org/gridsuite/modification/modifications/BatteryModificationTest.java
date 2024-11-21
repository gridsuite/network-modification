/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ReactiveCapabilityCurve;
import com.powsybl.iidm.network.ReactiveLimitsKind;
import com.powsybl.iidm.network.extensions.ActivePowerControl;

import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class BatteryModificationTest extends AbstractInjectionModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    public void checkModification() {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();
        batteryModificationInfos.setTargetP(new AttributeModification<>(-1.0, OperationType.SET));
        assertThrows(NetworkModificationException.class,
            () -> batteryModificationInfos.toModification().check(getNetwork()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected BatteryModificationInfos buildModification() {
        return BatteryModificationInfos.builder()
            .stashed(false)
            .equipmentId("v3Battery")
            .equipmentName(new AttributeModification<>("newV1Battery", OperationType.SET))
            .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
            .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
            .targetP(new AttributeModification<>(80.0, OperationType.SET))
            .targetQ(new AttributeModification<>(40.0, OperationType.SET))
            .minP(new AttributeModification<>(0., OperationType.SET))
            .maxP(new AttributeModification<>(100., OperationType.SET))
            .minQ(new AttributeModification<>(-100., OperationType.SET))
            .maxP(new AttributeModification<>(100., OperationType.SET))
            .reactiveCapabilityCurvePoints(List.of(
                            new ReactiveCapabilityCurveModificationInfos(0., 0., 100., 100., 0.,
                                            0.1),
                            new ReactiveCapabilityCurveModificationInfos(0., 0., 100., 100., 200.,
                                            150.)))
            .droop(new AttributeModification<>(0.1f, OperationType.SET))
            .participate(new AttributeModification<>(true, OperationType.SET))
            .reactiveCapabilityCurve(new AttributeModification<>(true, OperationType.SET))
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME)
                            .value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Battery modifiedBattery = getNetwork().getBattery("v3Battery");
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();
        assertEquals("newV1Battery", modifiedBattery.getNameOrId());
        assertEquals(80.0, modifiedBattery.getTargetP());
        assertEquals(40.0, modifiedBattery.getTargetQ());
        assertEquals(0., modifiedBattery.getMinP());
        assertEquals(100., modifiedBattery.getMaxP());
        assertEquals(0.1f, modifiedBattery.getExtension(ActivePowerControl.class).getDroop());
        assertTrue(modifiedBattery.getExtension(ActivePowerControl.class).isParticipate());
        assertEquals(ReactiveLimitsKind.CURVE, modifiedBattery.getReactiveLimits().getKind());
        Collection<ReactiveCapabilityCurve.Point> points = modifiedBattery
                        .getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
        List<ReactiveCapabilityCurve.Point> batteryPoints = new ArrayList<>(points);
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = batteryModificationInfos
                        .getReactiveCapabilityCurvePoints();
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, batteryPoints.size())
                  .forEach(i -> {
                      var point = batteryPoints.get(i);
                      var modificationPoint = modificationPoints.get(i);
                      assertEquals(modificationPoint.getMaxQ(), point.getMaxQ());
                      assertEquals(modificationPoint.getMinQ(), point.getMinQ());
                      assertEquals(modificationPoint.getP(), point.getP());
                  });
        }
        assertEquals(PROPERTY_VALUE, getNetwork().getBattery("v3Battery").getProperty(PROPERTY_NAME));
    }

    @Test
    void testImpactsAfterActivePowerControlModifications() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();
        batteryModificationInfos.toModification().apply(getNetwork());
        Battery battery = getNetwork().getBattery("v3Battery");
        assertEquals(0.1f, battery.getExtension(ActivePowerControl.class).getDroop());
        assertTrue(battery.getExtension(ActivePowerControl.class).isParticipate());
    }

    @Test
    void testActivePowerZeroOrBetweenMinAndMaxActivePower() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();
        Battery battery = getNetwork().getBattery("v3Battery");
        battery.setTargetP(80.)
                .setMinP(0.)
                .setMaxP(100.);
        batteryModificationInfos.setTargetP(new AttributeModification<>(155.0, OperationType.SET));

        Double minActivePower = batteryModificationInfos.getMinP() != null ? batteryModificationInfos.getMinP().getValue() : battery.getMinP();
        Double maxActivePower = batteryModificationInfos.getMaxP() != null ? batteryModificationInfos.getMaxP().getValue() : battery.getMaxP();
        Double activePower = batteryModificationInfos.getTargetP() != null ? batteryModificationInfos.getTargetP().getValue() : battery.getTargetP();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
                () -> batteryModificationInfos.toModification().check(getNetwork()));
        assertEquals("MODIFY_BATTERY_ERROR : Battery '" + "v3Battery" + "' : Active power " + activePower + " is expected to be equal to 0 or within the range of minimum active power and maximum active power: [" + minActivePower + ", " + maxActivePower + "]",
                exception.getMessage());

    }

    @Test
    void testMinQGreaterThanMaxQ() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();
        Battery battery = getNetwork().getBattery("v3Battery");
        battery.newReactiveCapabilityCurve()
                .beginPoint()
                .setP(0.)
                .setMaxQ(100.)
                .setMinQ(0.)
                .endPoint()
                .beginPoint()
                .setP(200.)
                .setMaxQ(150.)
                .setMinQ(0.)
                .endPoint()
                .add();
        Collection<ReactiveCapabilityCurve.Point> points = battery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
        List<ReactiveCapabilityCurve.Point> batteryPoints = new ArrayList<>(points);
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = batteryModificationInfos.getReactiveCapabilityCurvePoints();
        AtomicReference<Double> maxQ = new AtomicReference<>(Double.NaN);
        AtomicReference<Double> minQ = new AtomicReference<>(Double.NaN);
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, modificationPoints.size())
                    .forEach(i -> {
                        ReactiveCapabilityCurve.Point oldPoint = batteryPoints.get(i);
                        ReactiveCapabilityCurveModificationInfos newPoint = modificationPoints.get(i);
                        Double oldMaxQ = Double.NaN;
                        Double oldMinQ = Double.NaN;
                        if (oldPoint != null) {
                            oldMaxQ = oldPoint.getMaxQ();
                            oldMinQ = oldPoint.getMinQ();
                        }
                        newPoint.setMinQ(300.0);
                        newPoint.setOldMaxQ(250.0);
                        maxQ.set(newPoint.getMaxQ() != null ? newPoint.getMaxQ() : oldMaxQ);
                        minQ.set(newPoint.getMinQ() != null ? newPoint.getMinQ() : oldMinQ);
                    });
        }
        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
                () -> batteryModificationInfos.toModification().check(getNetwork()));
        assertEquals("MODIFY_BATTERY_ERROR : Battery '" + "v3Battery" + "' : maximum reactive power " + maxQ.get() + " is expected to be greater than or equal to minimum reactive power " + minQ.get(),
                exception.getMessage());
    }

    @Test
    void testDisconnection() throws Exception {
        assertChangeConnectionState(getNetwork().getBattery("v3Battery"), false);
    }

    @Test
    void testConnection() throws Exception {
        assertChangeConnectionState(getNetwork().getBattery("v3Battery"), true);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BATTERY_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v3Battery", updatedValues.get("equipmentId"));
    }
}
