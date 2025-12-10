/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.StaticVarCompensatorCreationInfos;
import org.gridsuite.modification.dto.VoltageRegulationType;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class StaticVarCompensatorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return StaticVarCompensatorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idStaticVarCompensator1")
                .equipmentName("nameStaticVarCompensator1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .maxSusceptance(224.0)
                .minSusceptance(200.0)
                .maxQAtNominalV(null)
                .minQAtNominalV(null)
                .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
                .voltageSetpoint(120.0)
                .reactivePowerSetpoint(300.0)
                .voltageRegulationType(VoltageRegulationType.LOCAL)
                .standbyAutomatonOn(false)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getStaticVarCompensator("idStaticVarCompensator1"));
        assertEquals(1, getNetwork().getVoltageLevel("v2").getStaticVarCompensatorStream()
                .filter(transformer -> transformer.getId().equals("idStaticVarCompensator1")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getStaticVarCompensator("idStaticVarCompensator1").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void checkModification() {
        Network network = getNetwork();
        StaticVarCompensatorCreationInfos compensatorCreationInfos = (StaticVarCompensatorCreationInfos) buildModification();
        // try to create an existing cspr
        compensatorCreationInfos.setEquipmentId("v5Compensator");
        StaticVarCompensatorCreation staticVarCompensatorCreation = (StaticVarCompensatorCreation) compensatorCreationInfos.toModification();
        Exception exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation.check(network));
        assertEquals(new NetworkModificationRunException("v5Compensator").getMessage(),
                exception.getMessage());

        // not found voltage level
        compensatorCreationInfos.setEquipmentId("idStaticVarCompensator2");
        compensatorCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        StaticVarCompensatorCreation staticVarCompensatorCreation1 = (StaticVarCompensatorCreation) compensatorCreationInfos.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation1.check(network));
        assertEquals(new NetworkModificationRunException("notFoundVoltageLevelId").getMessage(),
                exception.getMessage());

        // not found busbar section
        compensatorCreationInfos.setVoltageLevelId("v2");
        compensatorCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        StaticVarCompensatorCreation staticVarCompensatorCreation2 = (StaticVarCompensatorCreation) compensatorCreationInfos.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation2.check(network));
        assertEquals(new NetworkModificationRunException("notFoundBusbarSection").getMessage(),
                exception.getMessage());

        // invalid min susceptance
        compensatorCreationInfos.setVoltageLevelId("v2");
        compensatorCreationInfos.setBusOrBusbarSectionId("1B");
        compensatorCreationInfos.setMinSusceptance(null);
        compensatorCreationInfos.setMinQAtNominalV(null);

        StaticVarCompensatorCreation staticVarCompensatorCreation3 = (StaticVarCompensatorCreation) compensatorCreationInfos.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation3.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : StaticVarCompensator 'idStaticVarCompensator2' : minimum susceptance is not set",
                exception.getMessage());
        compensatorCreationInfos.setMinSusceptance(200.0);
        compensatorCreationInfos.setMaxSusceptance(null);
        compensatorCreationInfos.setMaxQAtNominalV(null);
        compensatorCreationInfos.setMinQAtNominalV(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation4 = (StaticVarCompensatorCreation) compensatorCreationInfos.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation4.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : maximum susceptance is not set",
                exception.getMessage());

        compensatorCreationInfos.setMaxSusceptance(100.0);
        compensatorCreationInfos.setMinSusceptance(200.0);
        compensatorCreationInfos.setRegulationMode(StaticVarCompensator.RegulationMode.REACTIVE_POWER);
        compensatorCreationInfos.setReactivePowerSetpoint(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation5 = (StaticVarCompensatorCreation) compensatorCreationInfos.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation5.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : maximum susceptance is expected to be greater than or equal to minimum susceptance",
                exception.getMessage());
        compensatorCreationInfos.setMaxSusceptance(null);
        compensatorCreationInfos.setMinSusceptance(null);
        compensatorCreationInfos.setMaxQAtNominalV(200.0);
        compensatorCreationInfos.setMinQAtNominalV(300.0);
        StaticVarCompensatorCreation staticVarCompensatorCreation6 = (StaticVarCompensatorCreation) compensatorCreationInfos.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation6.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : maximum Q at nominal voltage is expected to be greater than or equal to minimum Q",
                exception.getMessage());
        compensatorCreationInfos.setMaxQAtNominalV(200.0);
        compensatorCreationInfos.setMinQAtNominalV(100.0);
        compensatorCreationInfos.setRegulationMode(StaticVarCompensator.RegulationMode.REACTIVE_POWER);
        compensatorCreationInfos.setReactivePowerSetpoint(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation7 = (StaticVarCompensatorCreation) compensatorCreationInfos.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation7.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : Reactive power setpoint is not set",
                exception.getMessage());

        compensatorCreationInfos.setRegulationMode(StaticVarCompensator.RegulationMode.VOLTAGE);
        compensatorCreationInfos.setVoltageSetpoint(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation8 = (StaticVarCompensatorCreation) compensatorCreationInfos.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation8.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : Voltage setpoint is not set",
                exception.getMessage());
        //CreateWithStandbyAutomatonErrors
        StaticVarCompensatorCreationInfos compensatorCreationInfos1 = (StaticVarCompensatorCreationInfos) buildModification();
        compensatorCreationInfos1.setStandbyAutomatonOn(true);
        compensatorCreationInfos1.setMaxSusceptance(null);
        compensatorCreationInfos1.setMinSusceptance(null);
        compensatorCreationInfos1.setMinQAtNominalV(200.0);
        compensatorCreationInfos1.setMaxQAtNominalV(300.0);
        compensatorCreationInfos1.setLowVoltageSetpoint(200.0);
        compensatorCreationInfos1.setHighVoltageSetpoint(400.0);
        compensatorCreationInfos1.setLowVoltageThreshold(250.0);
        compensatorCreationInfos1.setHighVoltageThreshold(300.0);
        compensatorCreationInfos1.setQ0(Double.NaN);
        ReportNode report = compensatorCreationInfos1.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        compensatorCreationInfos1.toModification().apply(getNetwork(), report);
        assertLogMessage("Cannot add standby automaton extension on Static var compensator 'idStaticVarCompensator1': b0 is invalid",
                "network.modification.StandbyAutomatonExtensionAddError", report);
        StaticVarCompensatorCreationInfos compensatorCreationInfos2 = (StaticVarCompensatorCreationInfos) buildModification();
        compensatorCreationInfos2.setEquipmentId("idStaticVarCompensator2");
        compensatorCreationInfos2.setStandbyAutomatonOn(true);
        compensatorCreationInfos2.setMaxSusceptance(null);
        compensatorCreationInfos2.setMinSusceptance(null);
        compensatorCreationInfos2.setMinQAtNominalV(200.0);
        compensatorCreationInfos2.setMaxQAtNominalV(300.0);
        compensatorCreationInfos2.setLowVoltageSetpoint(200.0);
        compensatorCreationInfos2.setHighVoltageSetpoint(400.0);
        compensatorCreationInfos2.setLowVoltageThreshold(250.0);
        compensatorCreationInfos2.setHighVoltageThreshold(300.0);
        compensatorCreationInfos2.setQ0(400.0);

        StaticVarCompensatorCreation staticVarCompensatorCreation9 = (StaticVarCompensatorCreation) compensatorCreationInfos2.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation9.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : q0 must be within the range of minimum Q and maximum Q",
                exception.getMessage());
        compensatorCreationInfos2.setMinQAtNominalV(null);
        compensatorCreationInfos2.setMaxQAtNominalV(null);
        compensatorCreationInfos2.setMaxSusceptance(300.0);
        compensatorCreationInfos2.setMinSusceptance(200.0);
        compensatorCreationInfos2.setB0(400.0);
        compensatorCreationInfos2.setQ0(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation10 = (StaticVarCompensatorCreation) compensatorCreationInfos2.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation10.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : b0 must be within the range of minimum susceptance and maximum susceptance",
                exception.getMessage());
        compensatorCreationInfos2.setB0(250.0);
        compensatorCreationInfos2.setRegulating(false);
        compensatorCreationInfos2.setRegulationMode(StaticVarCompensator.RegulationMode.REACTIVE_POWER);
        compensatorCreationInfos2.setStandby(true);

        StaticVarCompensatorCreation staticVarCompensatorCreation11 = (StaticVarCompensatorCreation) compensatorCreationInfos2.toModification();
        exception = assertThrows(NetworkModificationRunException.class, () -> staticVarCompensatorCreation11.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : Standby is only supported in Voltage Regulation mode",
                exception.getMessage());

    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("STATIC_VAR_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() {
        });
        assertEquals("idStaticVarCompensator1", updatedValues.get("equipmentId"));
    }

    @Test
    void testCreationInfoChecks() {
        Network network = getNetwork();
        StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos = StaticVarCompensatorCreationInfos.builder()
            .equipmentId("svc3")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .maxSusceptance(224.0)
            .minSusceptance(200.0)
            .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
            .reactivePowerSetpoint(300.0)
            .voltageRegulationType(VoltageRegulationType.LOCAL)
            .standbyAutomatonOn(false)
            .voltageSetpoint(-1d)
            .build();
        StaticVarCompensatorCreation staticVarCompensatorCreation = (StaticVarCompensatorCreation) staticVarCompensatorCreationInfos.toModification();
        String message = assertThrows(NetworkModificationRunException.class,
            () -> staticVarCompensatorCreation.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for voltage set point", message);

        StaticVarCompensatorCreationInfos staticVarCompensatorModificationInfos2 = StaticVarCompensatorCreationInfos.builder()
            .equipmentId("svc3")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .maxSusceptance(224.0)
            .minSusceptance(200.0)
            .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
            .reactivePowerSetpoint(300.0)
            .voltageRegulationType(VoltageRegulationType.LOCAL)
            .standbyAutomatonOn(false)
            .voltageSetpoint(100d)
            .highVoltageSetpoint(-1d)
            .build();
        StaticVarCompensatorCreation staticVarCompensatorCreation2 = (StaticVarCompensatorCreation) staticVarCompensatorModificationInfos2.toModification();
        message = assertThrows(NetworkModificationRunException.class,
            () -> staticVarCompensatorCreation2.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for high voltage set point", message);

        StaticVarCompensatorCreationInfos staticVarCompensatorModificationInfos3 = StaticVarCompensatorCreationInfos.builder()
            .equipmentId("svc3")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .maxSusceptance(224.0)
            .minSusceptance(200.0)
            .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
            .reactivePowerSetpoint(300.0)
            .voltageRegulationType(VoltageRegulationType.LOCAL)
            .standbyAutomatonOn(false)
            .voltageSetpoint(100d)
            .lowVoltageSetpoint(-1d)
            .build();
        StaticVarCompensatorCreation staticVarCompensatorCreation3 = (StaticVarCompensatorCreation) staticVarCompensatorModificationInfos3.toModification();
        message = assertThrows(NetworkModificationRunException.class,
            () -> staticVarCompensatorCreation3.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for low voltage set point", message);

        StaticVarCompensatorCreationInfos staticVarCompensatorModificationInfos4 = StaticVarCompensatorCreationInfos.builder()
            .equipmentId("svc3")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .maxSusceptance(224.0)
            .minSusceptance(200.0)
            .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
            .reactivePowerSetpoint(300.0)
            .voltageRegulationType(VoltageRegulationType.LOCAL)
            .standbyAutomatonOn(false)
            .voltageSetpoint(100d)
            .highVoltageThreshold(-1d)
            .build();
        StaticVarCompensatorCreation staticVarCompensatorCreation4 = (StaticVarCompensatorCreation) staticVarCompensatorModificationInfos4.toModification();
        message = assertThrows(NetworkModificationRunException.class,
            () -> staticVarCompensatorCreation4.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for high voltage threshold", message);

        StaticVarCompensatorCreationInfos staticVarCompensatorModificationInfos5 = StaticVarCompensatorCreationInfos.builder()
            .equipmentId("svc3")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .maxSusceptance(224.0)
            .minSusceptance(200.0)
            .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
            .reactivePowerSetpoint(300.0)
            .voltageRegulationType(VoltageRegulationType.LOCAL)
            .standbyAutomatonOn(false)
            .voltageSetpoint(100d)
            .lowVoltageThreshold(-1d)
            .build();
        StaticVarCompensatorCreation staticVarCompensatorCreation5 = (StaticVarCompensatorCreation) staticVarCompensatorModificationInfos5.toModification();
        message = assertThrows(NetworkModificationRunException.class,
            () -> staticVarCompensatorCreation5.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for low voltage threshold", message);
    }
}
