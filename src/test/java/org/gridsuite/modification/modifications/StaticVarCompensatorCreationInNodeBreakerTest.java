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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.FreePropertyModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.StaticVarCompensatorCreationModel;
import org.gridsuite.modification.model.VoltageRegulationType;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
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
    protected ModificationModel buildModification() {
        return StaticVarCompensatorCreationModel.builder()
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
            .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
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
        StaticVarCompensatorCreationModel compensatorCreationModel = (StaticVarCompensatorCreationModel) buildModification();
        // try to create an existing cspr
        compensatorCreationModel.setEquipmentId("v5Compensator");
        StaticVarCompensatorCreation staticVarCompensatorCreation = (StaticVarCompensatorCreation) compensatorCreationModel.toModification();
        Exception exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation.check(network));
        assertEquals(new NetworkModificationException(STATIC_VAR_COMPENSATOR_ALREADY_EXISTS, "v5Compensator").getMessage(),
            exception.getMessage());

        // not found voltage level
        compensatorCreationModel.setEquipmentId("idStaticVarCompensator2");
        compensatorCreationModel.setVoltageLevelId("notFoundVoltageLevelId");
        StaticVarCompensatorCreation staticVarCompensatorCreation1 = (StaticVarCompensatorCreation) compensatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation1.check(network));
        assertEquals(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(),
            exception.getMessage());

        // not found busbar section
        compensatorCreationModel.setVoltageLevelId("v2");
        compensatorCreationModel.setBusOrBusbarSectionId("notFoundBusbarSection");
        StaticVarCompensatorCreation staticVarCompensatorCreation2 = (StaticVarCompensatorCreation) compensatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation2.check(network));
        assertEquals(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage(),
            exception.getMessage());

        // invalid min susceptance
        compensatorCreationModel.setVoltageLevelId("v2");
        compensatorCreationModel.setBusOrBusbarSectionId("1B");
        compensatorCreationModel.setMinSusceptance(null);
        compensatorCreationModel.setMinQAtNominalV(null);

        StaticVarCompensatorCreation staticVarCompensatorCreation3 = (StaticVarCompensatorCreation) compensatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation3.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : StaticVarCompensator 'idStaticVarCompensator2' : minimum susceptance is not set",
            exception.getMessage());
        compensatorCreationModel.setMinSusceptance(200.0);
        compensatorCreationModel.setMaxSusceptance(null);
        compensatorCreationModel.setMaxQAtNominalV(null);
        compensatorCreationModel.setMinQAtNominalV(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation4 = (StaticVarCompensatorCreation) compensatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation4.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                "StaticVarCompensator 'idStaticVarCompensator2' : maximum susceptance is not set",
            exception.getMessage());

        compensatorCreationModel.setMaxSusceptance(100.0);
        compensatorCreationModel.setMinSusceptance(200.0);
        compensatorCreationModel.setRegulationMode(StaticVarCompensator.RegulationMode.REACTIVE_POWER);
        compensatorCreationModel.setReactivePowerSetpoint(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation5 = (StaticVarCompensatorCreation) compensatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation5.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                "StaticVarCompensator 'idStaticVarCompensator2' : maximum susceptance is expected to be greater than or equal to minimum susceptance",
            exception.getMessage());
        compensatorCreationModel.setMaxSusceptance(null);
        compensatorCreationModel.setMinSusceptance(null);
        compensatorCreationModel.setMaxQAtNominalV(200.0);
        compensatorCreationModel.setMinQAtNominalV(300.0);
        StaticVarCompensatorCreation staticVarCompensatorCreation6 = (StaticVarCompensatorCreation) compensatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation6.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                "StaticVarCompensator 'idStaticVarCompensator2' : maximum Q at nominal voltage is expected to be greater than or equal to minimum Q",
            exception.getMessage());
        compensatorCreationModel.setMaxQAtNominalV(200.0);
        compensatorCreationModel.setMinQAtNominalV(100.0);
        compensatorCreationModel.setRegulationMode(StaticVarCompensator.RegulationMode.REACTIVE_POWER);
        compensatorCreationModel.setReactivePowerSetpoint(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation7 = (StaticVarCompensatorCreation) compensatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation7.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                "StaticVarCompensator 'idStaticVarCompensator2' : Reactive power setpoint is not set",
            exception.getMessage());

        compensatorCreationModel.setRegulationMode(StaticVarCompensator.RegulationMode.VOLTAGE);
        compensatorCreationModel.setVoltageSetpoint(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation8 = (StaticVarCompensatorCreation) compensatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation8.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                "StaticVarCompensator 'idStaticVarCompensator2' : Voltage setpoint is not set",
            exception.getMessage());
        //CreateWithStandbyAutomatonErrors
        StaticVarCompensatorCreationModel compensatorCreationModel1 = (StaticVarCompensatorCreationModel) buildModification();
        compensatorCreationModel1.setStandbyAutomatonOn(true);
        compensatorCreationModel1.setMaxSusceptance(null);
        compensatorCreationModel1.setMinSusceptance(null);
        compensatorCreationModel1.setMinQAtNominalV(200.0);
        compensatorCreationModel1.setMaxQAtNominalV(300.0);
        compensatorCreationModel1.setLowVoltageSetpoint(200.0);
        compensatorCreationModel1.setHighVoltageSetpoint(400.0);
        compensatorCreationModel1.setLowVoltageThreshold(250.0);
        compensatorCreationModel1.setHighVoltageThreshold(300.0);
        compensatorCreationModel1.setQ0(Double.NaN);
        ReportNode report = compensatorCreationModel1.createSubReportNode(ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test").build());
        compensatorCreationModel1.toModification().apply(getNetwork(), report);
        assertLogMessage("Cannot add standby automaton extension on Static var compensator 'idStaticVarCompensator1': b0 is invalid",
            "network.modification.StandbyAutomatonExtensionAddError", report);
        StaticVarCompensatorCreationModel compensatorCreationModel2 = (StaticVarCompensatorCreationModel) buildModification();
        compensatorCreationModel2.setEquipmentId("idStaticVarCompensator2");
        compensatorCreationModel2.setStandbyAutomatonOn(true);
        compensatorCreationModel2.setMaxSusceptance(null);
        compensatorCreationModel2.setMinSusceptance(null);
        compensatorCreationModel2.setMinQAtNominalV(200.0);
        compensatorCreationModel2.setMaxQAtNominalV(300.0);
        compensatorCreationModel2.setLowVoltageSetpoint(200.0);
        compensatorCreationModel2.setHighVoltageSetpoint(400.0);
        compensatorCreationModel2.setLowVoltageThreshold(250.0);
        compensatorCreationModel2.setHighVoltageThreshold(300.0);
        compensatorCreationModel2.setQ0(400.0);

        StaticVarCompensatorCreation staticVarCompensatorCreation9 = (StaticVarCompensatorCreation) compensatorCreationModel2.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation9.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                "StaticVarCompensator 'idStaticVarCompensator2' : q0 must be within the range of minimum Q and maximum Q",
            exception.getMessage());
        compensatorCreationModel2.setMinQAtNominalV(null);
        compensatorCreationModel2.setMaxQAtNominalV(null);
        compensatorCreationModel2.setMaxSusceptance(300.0);
        compensatorCreationModel2.setMinSusceptance(200.0);
        compensatorCreationModel2.setB0(400.0);
        compensatorCreationModel2.setQ0(null);
        StaticVarCompensatorCreation staticVarCompensatorCreation10 = (StaticVarCompensatorCreation) compensatorCreationModel2.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation10.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                "StaticVarCompensator 'idStaticVarCompensator2' : b0 must be within the range of minimum susceptance and maximum susceptance",
            exception.getMessage());
        compensatorCreationModel2.setB0(250.0);
        compensatorCreationModel2.setRegulating(false);
        compensatorCreationModel2.setRegulationMode(StaticVarCompensator.RegulationMode.REACTIVE_POWER);
        compensatorCreationModel2.setStandby(true);

        StaticVarCompensatorCreation staticVarCompensatorCreation11 = (StaticVarCompensatorCreation) compensatorCreationModel2.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> staticVarCompensatorCreation11.check(network));
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                "StaticVarCompensator 'idStaticVarCompensator2' : Standby is only supported in Voltage Regulation mode",
            exception.getMessage());

    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("STATIC_VAR_COMPENSATOR_CREATION", modificationModel.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() {
        });
        assertEquals("idStaticVarCompensator1", updatedValues.get("equipmentId"));
    }

    @Test
    void testCreationInfoChecks() {
        Network network = getNetwork();
        StaticVarCompensatorCreationModel staticVarCompensatorCreationModel = StaticVarCompensatorCreationModel.builder()
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
        StaticVarCompensatorCreation staticVarCompensatorCreation = (StaticVarCompensatorCreation) staticVarCompensatorCreationModel.toModification();
        String message = assertThrows(NetworkModificationException.class,
            () -> staticVarCompensatorCreation.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for voltage set point", message);

        StaticVarCompensatorCreationModel staticVarCompensatorModificationModel2 = StaticVarCompensatorCreationModel.builder()
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
        StaticVarCompensatorCreation staticVarCompensatorCreation2 = (StaticVarCompensatorCreation) staticVarCompensatorModificationModel2.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> staticVarCompensatorCreation2.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for high voltage set point", message);

        StaticVarCompensatorCreationModel staticVarCompensatorModificationModel3 = StaticVarCompensatorCreationModel.builder()
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
        StaticVarCompensatorCreation staticVarCompensatorCreation3 = (StaticVarCompensatorCreation) staticVarCompensatorModificationModel3.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> staticVarCompensatorCreation3.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for low voltage set point", message);

        StaticVarCompensatorCreationModel staticVarCompensatorModificationModel4 = StaticVarCompensatorCreationModel.builder()
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
        StaticVarCompensatorCreation staticVarCompensatorCreation4 = (StaticVarCompensatorCreation) staticVarCompensatorModificationModel4.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> staticVarCompensatorCreation4.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for high voltage threshold", message);

        StaticVarCompensatorCreationModel staticVarCompensatorModificationModel5 = StaticVarCompensatorCreationModel.builder()
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
        StaticVarCompensatorCreation staticVarCompensatorCreation5 = (StaticVarCompensatorCreation) staticVarCompensatorModificationModel5.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> staticVarCompensatorCreation5.check(network)).getMessage();
        assertEquals("CREATE_STATIC_VAR_COMPENSATOR_ERROR : Static var compensator 'svc3' : can not have a negative value for low voltage threshold", message);
    }
}
