/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuit;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class GeneratorModificationTest extends AbstractInjectionModificationTest {
    private static String PROPERTY_NAME = "property-name";
    private static String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return GeneratorModificationInfos.builder()
                .stashed(false)
                .equipmentId("idGenerator")
                .energySource(new AttributeModification<>(EnergySource.SOLAR, OperationType.SET))
                .equipmentName(new AttributeModification<>("newV1Generator", OperationType.SET))
                .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
                .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
                .connectionName(new AttributeModification<>("idGenerator", OperationType.SET))
                .connectionPosition(new AttributeModification<>(1, OperationType.SET))
                .connectionDirection(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .targetP(new AttributeModification<>(80.0, OperationType.SET))
                .targetQ(new AttributeModification<>(40.0, OperationType.SET))
                .targetV(new AttributeModification<>(48.0, OperationType.SET))
                .voltageRegulationOn(new AttributeModification<>(false, OperationType.SET))
                .minP(new AttributeModification<>(0., OperationType.SET))
                .maxP(new AttributeModification<>(100., OperationType.SET))
                .ratedS(new AttributeModification<>(220., OperationType.SET))
                .voltageRegulationType(
                        new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
                .plannedActivePowerSetPoint(new AttributeModification<>(10., OperationType.SET))
                .marginalCost(new AttributeModification<>(0.1, OperationType.SET))
                .plannedOutageRate(new AttributeModification<>(.30, OperationType.SET))
                .forcedOutageRate(new AttributeModification<>(.40, OperationType.SET))
                .minQ(new AttributeModification<>(-100., OperationType.SET))
                .maxQ(new AttributeModification<>(100., OperationType.SET))
                .reactiveCapabilityCurvePoints(List.of(
                        new ReactiveCapabilityCurvePointsInfos(100., 100., 0.1),
                        new ReactiveCapabilityCurvePointsInfos(100., 100., 150.)))
                .droop(new AttributeModification<>(0.1f, OperationType.SET))
                .participate(new AttributeModification<>(true, OperationType.SET))
                .directTransX(new AttributeModification<>(0.1, OperationType.SET))
                .stepUpTransformerX(new AttributeModification<>(0.1, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v2load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v1", OperationType.SET))
                .qPercent(new AttributeModification<>(0.1, OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(true, OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Generator modifiedGenerator = getNetwork().getGenerator("idGenerator");
        assertEquals("newV1Generator", modifiedGenerator.getNameOrId());
        assertEquals(EnergySource.SOLAR, modifiedGenerator.getEnergySource());
        assertEquals(80.0, modifiedGenerator.getTargetP());
        assertEquals(40.0, modifiedGenerator.getTargetQ());
        assertEquals(48.0, modifiedGenerator.getTargetV());
        assertFalse(modifiedGenerator.isVoltageRegulatorOn());
        assertEquals(0., modifiedGenerator.getMinP());
        assertEquals(100., modifiedGenerator.getMaxP());
        assertEquals(220., modifiedGenerator.getRatedS());
        assertEquals(0.1, modifiedGenerator.getExtension(GeneratorStartup.class).getMarginalCost());
        assertEquals(10., modifiedGenerator.getExtension(GeneratorStartup.class).getPlannedActivePowerSetpoint());
        assertEquals(0.30, modifiedGenerator.getExtension(GeneratorStartup.class).getPlannedOutageRate());
        assertEquals(0.40, modifiedGenerator.getExtension(GeneratorStartup.class).getForcedOutageRate());
        assertEquals(0.1f, modifiedGenerator.getExtension(ActivePowerControl.class).getDroop());
        assertTrue(modifiedGenerator.getExtension(ActivePowerControl.class).isParticipate());
        assertEquals(0.1, modifiedGenerator.getExtension(GeneratorShortCircuit.class).getDirectTransX());
        assertEquals(0.1, modifiedGenerator.getExtension(GeneratorShortCircuit.class).getStepUpTransformerX());
        assertEquals(ReactiveLimitsKind.CURVE, modifiedGenerator.getReactiveLimits().getKind());
        assertEquals(PROPERTY_VALUE, modifiedGenerator.getProperty(PROPERTY_NAME));
    }

    @Override
    protected void checkModification() {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        // Unset an attribute that should not be null
        generatorModificationInfos.setEnergySource(new AttributeModification<>(null, OperationType.UNSET));

        ValidationException exception = assertThrows(ValidationException.class,
                () -> generatorModificationInfos.toModification().apply(getNetwork()));
        assertEquals("Generator 'idGenerator': energy source is not set",
                exception.getMessage());

        // check regulating terminal
        GeneratorModificationInfos generatorModificationInfos2 = (GeneratorModificationInfos) buildModification();
        generatorModificationInfos2.setRegulatingTerminalId(new AttributeModification<>(null, OperationType.UNSET));
        NetworkModificationException exception2 = assertThrows(NetworkModificationException.class,
            () -> generatorModificationInfos2.toModification().check(getNetwork()));
        assertEquals("MODIFY_GENERATOR_ERROR : Generator 'idGenerator' : Regulation is set to Distant but regulating terminal information are incomplete",
            exception2.getMessage());

        // check regulating terminal
        GeneratorModificationInfos generatorModificationInfos3 = (GeneratorModificationInfos) buildModification();
        generatorModificationInfos3.setRegulatingTerminalVlId(new AttributeModification<>(null, OperationType.UNSET));
        generatorModificationInfos3.setRegulatingTerminalId(new AttributeModification<>(null, OperationType.UNSET));
        generatorModificationInfos3.setRegulatingTerminalType(new AttributeModification<>(null, OperationType.UNSET));
        NetworkModificationException exception3 = assertThrows(NetworkModificationException.class,
            () -> generatorModificationInfos3.toModification().check(getNetwork()));
        assertEquals("MODIFY_GENERATOR_ERROR : Generator 'idGenerator' : Regulation is set to Distant but regulating terminal is local and there is no modification about regulating terminal",
            exception3.getMessage());

        // check regulating terminal
        GeneratorModificationInfos generatorModificationInfos4 = (GeneratorModificationInfos) buildModification();
        generatorModificationInfos4.setRegulatingTerminalVlId(new AttributeModification<>(null, OperationType.UNSET));
        generatorModificationInfos4.setRegulatingTerminalId(new AttributeModification<>(null, OperationType.UNSET));
        generatorModificationInfos4.setRegulatingTerminalType(new AttributeModification<>(null, OperationType.UNSET));
        getNetwork().getGenerator("idGenerator").setRegulatingTerminal(getNetwork().getBusbarSection("1A1").getTerminal());
        assertDoesNotThrow(() -> generatorModificationInfos4.toModification().check(getNetwork()));

        GeneratorModificationInfos generatorModificationInfos5 = GeneratorModificationInfos.builder()
            .equipmentId("idGenerator")
            .droop(new AttributeModification<>(101f, OperationType.SET))
            .build();
        String message = assertThrows(NetworkModificationException.class,
            () -> generatorModificationInfos5.toModification().check(getNetwork())).getMessage();
        assertEquals("MODIFY_GENERATOR_ERROR : Generator 'idGenerator' : must have Droop between 0 and 100", message);

        GeneratorModificationInfos generatorModificationInfos6 = GeneratorModificationInfos.builder()
            .equipmentId("idGenerator")
            .droop(new AttributeModification<>(-1f, OperationType.SET))
            .build();
        message = assertThrows(NetworkModificationException.class,
            () -> generatorModificationInfos6.toModification().check(getNetwork())).getMessage();
        assertEquals("MODIFY_GENERATOR_ERROR : Generator 'idGenerator' : must have Droop between 0 and 100", message);

        GeneratorModificationInfos generatorModificationInfos7 = GeneratorModificationInfos.builder()
            .equipmentId("idGenerator")
            .targetV(new AttributeModification<>(-100d, OperationType.SET))
            .build();
        message = assertThrows(NetworkModificationException.class,
            () -> generatorModificationInfos7.toModification().check(getNetwork())).getMessage();
        assertEquals("MODIFY_GENERATOR_ERROR : Generator 'idGenerator' : can not have a negative value for Target Voltage", message);
    }

    @Test
    void testMinQGreaterThanMaxQ() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        Generator generator = getNetwork().getGenerator("idGenerator");
        generator.newReactiveCapabilityCurve()
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
        Collection<ReactiveCapabilityCurve.Point> points = generator.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
        List<ReactiveCapabilityCurvePointsInfos> modificationPoints = generatorModificationInfos.getReactiveCapabilityCurvePoints();
        AtomicReference<Double> maxQ = new AtomicReference<>(Double.NaN);
        AtomicReference<Double> minQ = new AtomicReference<>(Double.NaN);
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, modificationPoints.size())
                    .forEach(i -> {
                        ReactiveCapabilityCurvePointsInfos newPoint = modificationPoints.get(i);
                        newPoint.setMinQ(300.0);
                        maxQ.set(newPoint.getMaxQ());
                        minQ.set(newPoint.getMinQ());
                    });
        }
        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
                () -> generatorModificationInfos.toModification().check(getNetwork()));
        assertEquals("MODIFY_GENERATOR_ERROR : Generator 'idGenerator' : maximum reactive power 100.0 is expected to be greater than or equal to minimum reactive power 300.0",
                exception.getMessage());
    }

    @Test
    void testActivePowerZeroOrBetweenMinAndMaxActivePower() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        Generator generator = getNetwork().getGenerator("idGenerator");
        generator.setTargetP(80.)
                .setMinP(10.)
                .setMaxP(150.);

        generatorModificationInfos.setTargetP(new AttributeModification<>(110.0, OperationType.SET));

        NetworkModificationException exception = assertThrows(NetworkModificationException.class,
                () -> generatorModificationInfos.toModification().check(getNetwork()));
        assertEquals("MODIFY_GENERATOR_ERROR : Generator 'idGenerator' : Active power 110.0 is expected to be equal to 0 or within the range of minimum active power and maximum active power: [0.0, 100.0]",
                exception.getMessage());

    }

    @Test
    void testUnsetAttributes() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        // Unset TargetV
        generatorModificationInfos.setTargetV(new AttributeModification<>(null, OperationType.UNSET));

        generatorModificationInfos.toModification().apply(getNetwork());
        assertEquals(Double.NaN, getNetwork().getGenerator("idGenerator").getTargetV());

        //Unset TargetQ (voltage regulation needs to be turned on and voltage setpoint to have a value)
        generatorModificationInfos.setVoltageRegulationOn(new AttributeModification<>(true, OperationType.SET));
        generatorModificationInfos.setTargetV(new AttributeModification<>(44.0, OperationType.SET));
        generatorModificationInfos.setTargetQ(new AttributeModification<>(null, OperationType.UNSET));
        generatorModificationInfos.toModification().apply(getNetwork());
        assertEquals(Double.NaN, getNetwork().getGenerator("idGenerator").getTargetQ());

    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("GENERATOR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idGenerator", createdValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        assertChangeConnectionState(getNetwork().getGenerator("idGenerator"), false);
    }

    @Test
    void testConnection() throws Exception {
        assertChangeConnectionState(getNetwork().getGenerator("idGenerator"), true);
    }
}
