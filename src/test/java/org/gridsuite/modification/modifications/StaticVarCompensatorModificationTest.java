/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.extensions.StandbyAutomatonAdder;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.error.NetworkModificationExceptionType.STATIC_VAR_COMPENSATOR_NOT_FOUND;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
class StaticVarCompensatorModificationTest extends AbstractInjectionModificationTest {
    private static final String SVC_ID = "v3Compensator";
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return StaticVarCompensatorModificationInfos.builder()
                .equipmentId(SVC_ID)
                .equipmentName(new AttributeModification<>("newName", OperationType.SET))
                .minSusceptance(new AttributeModification<>(1., OperationType.SET))
                .maxSusceptance(new AttributeModification<>(40., OperationType.SET))
                .regulationMode(new AttributeModification<>(StaticVarCompensator.RegulationMode.REACTIVE_POWER, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .voltageSetpoint(new AttributeModification<>(390., OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(50., OperationType.SET))
                .voltageRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v3load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v3", OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        StaticVarCompensator svc = getSvc();
        assertEquals("newName", svc.getOptionalName().orElseThrow());
        assertEquals(1., svc.getBmin());
        assertEquals(40., svc.getBmax());
        assertEquals(StaticVarCompensator.RegulationMode.REACTIVE_POWER, svc.getRegulationMode());
        assertTrue(svc.isRegulating());
        assertEquals(390., svc.getVoltageSetpoint());
        assertEquals(50., svc.getReactivePowerSetpoint());
        assertEquals("v3load", svc.getRegulatingTerminal().getConnectable().getId());
        assertEquals(PROPERTY_VALUE, svc.getProperty(PROPERTY_NAME));
    }

    @Override
    protected void checkModification() {
        assertCheckFails(StaticVarCompensatorModificationInfos.builder().equipmentId("unknown").build(), STATIC_VAR_COMPENSATOR_NOT_FOUND.getMessage());
        assertCheckFails(builder().maxSusceptance(set(0.)).build(), "maximum susceptance is expected to be greater than or equal to minimum susceptance");
        assertCheckFails(builder().minSusceptance(unset()).build(), "minimum susceptance is not set");
        assertCheckFails(builder().maxSusceptance(unset()).build(), "maximum susceptance is not set");
        assertCheckFails(builder().voltageSetpoint(set(-1.)).build(), "voltage set point");
        assertCheckFails(builder().voltageSetpoint(unset()).build(), "Voltage setpoint is not set");
        assertCheckFails(builder().regulationMode(new AttributeModification<>(null, OperationType.UNSET)).build(), "Regulation mode can not be unset");
        assertCheckFails(builder().regulationMode(new AttributeModification<>(StaticVarCompensator.RegulationMode.REACTIVE_POWER, OperationType.SET))
                .reactivePowerSetpoint(unset()).build(), "Reactive power setpoint is not set");
        assertCheckFails(builder().voltageRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET)).build(),
                "Regulation is set to Distant but regulating terminal is local");
        assertCheckFails(builder().minSusceptance(set(30.)).maxQAtNominalV(set(3800000.)).build(), "maximum susceptance is expected to be greater than or equal to minimum susceptance");
    }

    @Test
    void testQAtNominalVIsConvertedToSusceptance() {
        double nominalV = getSvc().getTerminal().getVoltageLevel().getNominalV();
        builder().minQAtNominalV(set(nominalV * nominalV)).maxQAtNominalV(set(5 * nominalV * nominalV)).build().toModification().apply(getNetwork());
        assertEquals(1., getSvc().getBmin(), 1e-9);
        assertEquals(5., getSvc().getBmax(), 1e-9);
    }

    @Test
    void testSusceptanceTakesPrecedenceOverQAtNominalV() {
        builder().minSusceptance(set(3.)).minQAtNominalV(set(1.)).build().toModification().apply(getNetwork());
        assertEquals(3., getSvc().getBmin());
    }

    @Test
    void testPartialModificationKeepsOtherValues() {
        builder().maxSusceptance(set(35.)).build().toModification().apply(getNetwork());
        StaticVarCompensator svc = getSvc();
        assertEquals(2., svc.getBmin());
        assertEquals(35., svc.getBmax());
        assertEquals(StaticVarCompensator.RegulationMode.VOLTAGE, svc.getRegulationMode());
        assertTrue(svc.isRegulating());
        assertEquals(380., svc.getVoltageSetpoint());
        assertEquals(100., svc.getReactivePowerSetpoint());
        assertEquals(SVC_ID, svc.getRegulatingTerminal().getConnectable().getId());
    }

    @Test
    void testRegulationOff() {
        StaticVarCompensatorModificationInfos infos = builder().regulating(new AttributeModification<>(false, OperationType.SET)).voltageSetpoint(unset()).build();
        infos.toModification().check(getNetwork());
        infos.toModification().apply(getNetwork());
        StaticVarCompensator svc = getSvc();
        assertFalse(svc.isRegulating());
        assertEquals(StaticVarCompensator.RegulationMode.VOLTAGE, svc.getRegulationMode());
        assertTrue(Double.isNaN(svc.getVoltageSetpoint()));
    }

    @Test
    void testRegulationOnFromOff() {
        getSvc().setRegulating(false);
        builder().regulating(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(StaticVarCompensator.RegulationMode.REACTIVE_POWER, OperationType.SET))
                .reactivePowerSetpoint(set(12.))
                .build().toModification().apply(getNetwork());
        StaticVarCompensator svc = getSvc();
        assertTrue(svc.isRegulating());
        assertEquals(StaticVarCompensator.RegulationMode.REACTIVE_POWER, svc.getRegulationMode());
        assertEquals(12., svc.getReactivePowerSetpoint());
    }

    @Test
    void testDistantThenLocalRegulation() {
        buildModification().toModification().apply(getNetwork());
        assertEquals("v3load", getSvc().getRegulatingTerminal().getConnectable().getId());

        StaticVarCompensatorModificationInfos keepDistant = builder().voltageRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET)).build();
        keepDistant.toModification().check(getNetwork());
        keepDistant.toModification().apply(getNetwork());
        assertEquals("v3load", getSvc().getRegulatingTerminal().getConnectable().getId());

        builder().voltageRegulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET)).build().toModification().apply(getNetwork());
        assertEquals(SVC_ID, getSvc().getRegulatingTerminal().getConnectable().getId());
    }

    @Test
    void testDistantRegulationWithIncompleteTerminal() {
        assertCheckFails(builder().voltageRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v3load", OperationType.SET)).build(),
                "Regulation is set to Distant but regulating terminal information are incomplete");
    }

    @Test
    void testRegulationModeWithStandbyAutomaton() {
        getSvc().newExtension(StandbyAutomatonAdder.class)
                .withB0(5.)
                .withStandbyStatus(true)
                .withLowVoltageSetpoint(370.)
                .withHighVoltageSetpoint(390.)
                .withLowVoltageThreshold(360.)
                .withHighVoltageThreshold(400.)
                .add();
        assertCheckFails(builder().regulationMode(new AttributeModification<>(StaticVarCompensator.RegulationMode.REACTIVE_POWER, OperationType.SET)).build(),
                "Standby is only supported in Voltage Regulation mode");
    }

    @Test
    void testChangeConnectionState() throws Exception {
        assertChangeConnectionState(getSvc(), false);
        assertChangeConnectionState(getSvc(), true);
    }

    @Test
    void testMoveToAnotherVoltageLevelUsesItsNominalVoltage() {
        double targetNominalV = getNetwork().getVoltageLevel("v1").getNominalV();
        builder().voltageLevelId(new AttributeModification<>("v1", OperationType.SET))
                .busOrBusbarSectionId(new AttributeModification<>("1.1", OperationType.SET))
                .maxQAtNominalV(set(5 * targetNominalV * targetNominalV))
                .build().toModification().apply(getNetwork());
        assertEquals("v1", getSvc().getTerminal().getVoltageLevel().getId());
        assertEquals(5., getSvc().getBmax(), 1e-9);
    }

    private StaticVarCompensator getSvc() {
        return getNetwork().getStaticVarCompensator(SVC_ID);
    }

    private static StaticVarCompensatorModificationInfos.StaticVarCompensatorModificationInfosBuilder<?, ?> builder() {
        return StaticVarCompensatorModificationInfos.builder().equipmentId(SVC_ID);
    }

    private static AttributeModification<Double> set(double value) {
        return new AttributeModification<>(value, OperationType.SET);
    }

    private static AttributeModification<Double> unset() {
        return new AttributeModification<>(null, OperationType.UNSET);
    }

    private void assertCheckFails(StaticVarCompensatorModificationInfos infos, String expectedMessagePart) {
        AbstractModification modification = infos.toModification();
        Network network = getNetwork();
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> modification.check(network));
        assertTrue(exception.getMessage().contains(expectedMessagePart), exception.getMessage());
    }
}
