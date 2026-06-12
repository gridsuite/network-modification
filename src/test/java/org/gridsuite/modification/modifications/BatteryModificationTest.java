/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ReactiveCapabilityCurve;
import com.powsybl.iidm.network.ReactiveLimitsKind;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.BatteryShortCircuit;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class BatteryModificationTest extends AbstractInjectionModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final Double MEASUREMENT_P_VALUE = 10.0;
    private static final Boolean MEASUREMENT_P_VALID = false;
    private static final Double MEASUREMENT_Q_VALUE = 0.5;
    private static final Boolean MEASUREMENT_Q_VALID = true;

    @Override
    public void checkModification() {
        Network network = getNetwork();
        BatteryModificationModel batteryModificationModel = (BatteryModificationModel) buildModification();
        batteryModificationModel.setTargetP(new AttributeModification<>(-1.0, OperationType.SET));
        BatteryModification batteryModification = (BatteryModification) batteryModificationModel.toModification();
        assertThrows(NetworkModificationException.class,
            () -> batteryModification.check(network));

        BatteryModificationModel batteryModificationModel1 = BatteryModificationModel.builder()
            .equipmentId("v3Battery")
            .droop(new AttributeModification<>(101f, OperationType.SET))
            .build();
        BatteryModification batteryModification1 = (BatteryModification) batteryModificationModel1.toModification();
        String message = assertThrows(NetworkModificationException.class,
            () -> batteryModification1.check(network)).getMessage();
        assertEquals("MODIFY_BATTERY_ERROR : Battery 'v3Battery' : must have Droop between 0 and 100", message);

        BatteryModificationModel batteryModificationModel2 = BatteryModificationModel.builder()
            .equipmentId("v3Battery")
            .droop(new AttributeModification<>(-1f, OperationType.SET))
            .build();
        BatteryModification batteryModification2 = (BatteryModification) batteryModificationModel2.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> batteryModification2.check(network)).getMessage();
        assertEquals("MODIFY_BATTERY_ERROR : Battery 'v3Battery' : must have Droop between 0 and 100", message);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationModel buildModification() {
        return BatteryModificationModel.builder()
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
                new ReactiveCapabilityCurvePointsModel(100., 100.,
                    0.1),
                new ReactiveCapabilityCurvePointsModel(100., 100.,
                    150.)))
            .droop(new AttributeModification<>(0.1f, OperationType.SET))
            .directTransX(new AttributeModification<>(0.1, OperationType.SET))
            .stepUpTransformerX(new AttributeModification<>(0.2, OperationType.SET))
            .participate(new AttributeModification<>(true, OperationType.SET))
            .reactiveCapabilityCurve(new AttributeModification<>(true, OperationType.SET))
            .pMeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
            .pMeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
            .qMeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
            .qMeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
            .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Battery modifiedBattery = getNetwork().getBattery("v3Battery");
        BatteryModificationModel batteryModificationModel = (BatteryModificationModel) buildModification();
        assertEquals("newV1Battery", modifiedBattery.getNameOrId());
        assertEquals(80.0, modifiedBattery.getTargetP());
        assertEquals(40.0, modifiedBattery.getTargetQ());
        assertEquals(0., modifiedBattery.getMinP());
        assertEquals(100., modifiedBattery.getMaxP());
        ActivePowerControl<Battery> activePowerControl = modifiedBattery.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl);
        assertEquals(0.1f, activePowerControl.getDroop());
        assertTrue(activePowerControl.isParticipate());
        assertEquals(ReactiveLimitsKind.CURVE, modifiedBattery.getReactiveLimits().getKind());
        Collection<ReactiveCapabilityCurve.Point> points = modifiedBattery
            .getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
        List<ReactiveCapabilityCurve.Point> batteryPoints = new ArrayList<>(points);
        List<ReactiveCapabilityCurvePointsModel> modificationPoints = batteryModificationModel
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
        BatteryShortCircuit batteryShortCircuit = modifiedBattery.getExtension(BatteryShortCircuit.class);
        assertNotNull(batteryShortCircuit);
        assertEquals(0.1, batteryShortCircuit.getDirectTransX());
        assertEquals(0.2, batteryShortCircuit.getStepUpTransformerX());
        assertMeasurements(modifiedBattery, MEASUREMENT_P_VALUE, MEASUREMENT_P_VALID, MEASUREMENT_Q_VALUE, MEASUREMENT_Q_VALID);
    }

    @Test
    void testImpactsAfterActivePowerControlModifications() {
        BatteryModificationModel batteryModificationModel = (BatteryModificationModel) buildModification();
        batteryModificationModel.toModification().apply(getNetwork());
        Battery battery = getNetwork().getBattery("v3Battery");
        ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl);
        assertEquals(0.1f, activePowerControl.getDroop());
        assertTrue(activePowerControl.isParticipate());
    }

    @Test
    void testActivePowerZeroOrBetweenMinAndMaxActivePower() {
        BatteryModificationModel batteryModificationModel = (BatteryModificationModel) buildModification();
        Battery battery = getNetwork().getBattery("v3Battery");
        battery.setTargetP(80.)
            .setMinP(0.)
            .setMaxP(100.);
        batteryModificationModel.setTargetP(new AttributeModification<>(155.0, OperationType.SET));

        Double minActivePower = batteryModificationModel.getMinP() != null ? batteryModificationModel.getMinP().getValue() : battery.getMinP();
        Double maxActivePower = batteryModificationModel.getMaxP() != null ? batteryModificationModel.getMaxP().getValue() : battery.getMaxP();
        Double activePower = batteryModificationModel.getTargetP() != null ? batteryModificationModel.getTargetP().getValue() : battery.getTargetP();

        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
            () -> batteryModificationModel.toModification().check(getNetwork()));
        assertEquals("MODIFY_BATTERY_ERROR : Battery '" + "v3Battery" + "' : Active power " + activePower + " is expected to be equal to 0 or within the range of minimum active power and maximum active power: [" + minActivePower + ", " + maxActivePower + "]",
            exception.getMessage());

    }

    @Test
    void testMinQGreaterThanMaxQ() {
        BatteryModificationModel batteryModificationModel = (BatteryModificationModel) buildModification();
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
        List<ReactiveCapabilityCurvePointsModel> modificationPoints = batteryModificationModel.getReactiveCapabilityCurvePoints();
        AtomicReference<Double> maxQ = new AtomicReference<>(Double.NaN);
        AtomicReference<Double> minQ = new AtomicReference<>(Double.NaN);
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, modificationPoints.size())
                .forEach(i -> {
                    ReactiveCapabilityCurvePointsModel newPoint = modificationPoints.get(i);
                    newPoint.setMinQ(300.0);
                    newPoint.setMaxQ(250.0);
                    maxQ.set(newPoint.getMaxQ());
                    minQ.set(newPoint.getMinQ());
                });
        }
        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
            () -> batteryModificationModel.toModification().check(getNetwork()));
        assertEquals("MODIFY_BATTERY_ERROR : Battery '" + "v3Battery" + "' : maximum reactive power " + maxQ.get()
            + " is expected to be greater than or equal to minimum reactive power " + minQ.get(), exception.getMessage());
    }

    @Test
    void testMeasurementsUpdatedWithNewMeasurements() {
        Network network = getNetwork();
        Double newPMeasurementValue = 100.0D;
        Boolean newPMeasurementValidity = true;
        Double newQMeasurementValue = 5.0;
        Boolean newQMeasurementValidity = false;
        ReportNode rootNode = ReportNode.newRootReportNode()
            .withMessageTemplate("test")
            .build();
        buildModification().toModification().apply(network);
        assertMeasurements(network.getBattery("v3Battery"), MEASUREMENT_P_VALUE, MEASUREMENT_P_VALID, MEASUREMENT_Q_VALUE, MEASUREMENT_Q_VALID);

        BatteryModificationModel batteryModificationModel = BatteryModificationModel.builder()
            .equipmentId("v3Battery")
            .pMeasurementValue(new AttributeModification<>(newPMeasurementValue, OperationType.SET))
            .pMeasurementValidity(new AttributeModification<>(newPMeasurementValidity, OperationType.SET))
            .qMeasurementValue(new AttributeModification<>(newQMeasurementValue, OperationType.SET))
            .qMeasurementValidity(new AttributeModification<>(newQMeasurementValidity, OperationType.SET))
            .build();
        batteryModificationModel.toModification().apply(network, rootNode);

        assertMeasurements(network.getBattery("v3Battery"), newPMeasurementValue, newPMeasurementValidity, newQMeasurementValue, newQMeasurementValidity);
        assertMeasurementsReportNodes(rootNode, MEASUREMENT_P_VALUE, newPMeasurementValue, MEASUREMENT_P_VALID, newPMeasurementValidity, MEASUREMENT_Q_VALUE, newQMeasurementValue, MEASUREMENT_Q_VALID, newQMeasurementValidity);
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
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        // assertEquals("BATTERY_MODIFICATION", modificationModel.getMessageType());
        // Map<String, String> updatedValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        // });
        // assertEquals("v3Battery", updatedValues.get("equipmentId"));
    }

    @Test
    void testConnectionError() {
        getNetwork().getSwitch("v3dBattery").setOpen(true);
        BatteryModificationModel batteryModificationModel = new BatteryModificationModel();
        batteryModificationModel.setEquipmentId("v3Battery");
        batteryModificationModel.setTerminalConnected(new AttributeModification<>(true, OperationType.SET));
        String message = assertThrows(NetworkModificationException.class, () -> batteryModificationModel.toModification().apply(getNetwork())).getMessage();
        assertEquals("INJECTION_MODIFICATION_ERROR : Could not connect equipment 'v3Battery'", message);
    }

    private void assertMeasurements(Battery battery, double expectedP, boolean expectedPValidity, double expectedQ, boolean expectedQValidity) {
        Measurements<?> measurements = (Measurements<?>) battery.getExtension(Measurements.class);
        assertNotNull(measurements);
        Collection<Measurement> activePowerMeasurements = measurements.getMeasurements(Measurement.Type.ACTIVE_POWER).stream().toList();
        Collection<Measurement> reactivePowerMeasurements = measurements.getMeasurements(Measurement.Type.REACTIVE_POWER).stream().toList();
        assertThat(activePowerMeasurements).isNotEmpty();
        assertThat(reactivePowerMeasurements).isNotEmpty();
        assertThat(activePowerMeasurements).allSatisfy(m -> {
            assertThat(m.getValue()).isEqualTo(expectedP);
            assertThat(m.isValid()).isEqualTo(expectedPValidity);
        });
        assertThat(reactivePowerMeasurements).allSatisfy(m -> {
            assertThat(m.getValue()).isEqualTo(expectedQ);
            assertThat(m.isValid()).isEqualTo(expectedQValidity);
        });
    }

    private void assertMeasurementsReportNodes(ReportNode rootNode, Double measurementPValue, Double newPMeasurementValue, Boolean measurementPValid, Boolean newPMeasurementValidity, Double measurementQValue, Double newQMeasurementValue, Boolean measurementQValid, Boolean newQMeasurementValidity) {
        Optional<ReportNode> stateEstimationDataNode = rootNode.getChildren().stream()
            .filter(node -> node.getMessageKey().equals("network.modification.stateEstimationData"))
            .findFirst();
        assertThat(stateEstimationDataNode).isPresent();
        Optional<ReportNode> expectedMeasurementsNodeOpt = stateEstimationDataNode.get().getChildren().stream()
            .filter(node -> node.getMessageKey().equals("network.modification.measurements"))
            .findFirst();
        assertThat(expectedMeasurementsNodeOpt).isPresent();
        ReportNode expectedMeasurementsNode = expectedMeasurementsNodeOpt.get();
        assertThat(expectedMeasurementsNode.getChildren()).hasSize(4);
        assertMeasurementReportNode(expectedMeasurementsNode, measurementPValue, newPMeasurementValue);
        assertMeasurementReportNode(expectedMeasurementsNode, measurementQValue, newQMeasurementValue);
        assertMeasurementReportNode(expectedMeasurementsNode, measurementPValid, newPMeasurementValidity);
        assertMeasurementReportNode(expectedMeasurementsNode, measurementQValid, newQMeasurementValidity);
    }

    private void assertMeasurementReportNode(ReportNode rootNode, Object expectedOldValue, Object expectedNewValue) {
        assertThat(rootNode.getChildren()).anySatisfy(node -> {
            assertThat(node.getMessageKey()).isEqualTo("network.modification.fieldModification");
            assertThat(node.getValues().get("oldValue")).hasToString(expectedOldValue.toString());
            assertThat(node.getValues().get("newValue")).hasToString(expectedNewValue.toString());
        });
    }
}
