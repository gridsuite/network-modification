/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ValidationException;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.GeneratorCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class GeneratorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static String PROPERTY_NAME = "property-name";
    private static String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new generator in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        return GeneratorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idGenerator1")
                .equipmentName("idGenerator1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .energySource(EnergySource.HYDRO)
                .minP(100.0)
                .maxP(600.0)
                .ratedS(10.)
                .targetP(400.)
                .targetQ(50.)
                .voltageRegulationOn(true)
                .targetV(225.)
                .stepUpTransformerX(60.0)
                .directTransX(61.0)
                .minQ(20.0)
                .maxQ(25.0)
                .plannedActivePowerSetPoint(111.)
                .marginalCost(0.40)
                .plannedOutageRate(.45)
                .forcedOutageRate(.66)
                .droop(5f)
                .participate(true)
                .regulatingTerminalId("v2load")
                .regulatingTerminalType("LOAD")
                .regulatingTerminalVlId("v1")
                .qPercent(25.)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsInfos(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurvePointsInfos(5.6, 9.8, 10.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getGenerator("idGenerator1"));
        assertEquals(1, getNetwork().getVoltageLevel("v2").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator1")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getGenerator("idGenerator1").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void checkModification() {
        // invalid Generator id
        GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setEquipmentId("");
        PowsyblException exception = assertThrows(PowsyblException.class, () -> generatorCreationInfos.toModification().apply(getNetwork()));
        assertEquals("Invalid id ''", exception.getMessage());

        // not found voltage level
        generatorCreationInfos.setEquipmentId("idGenerator1");
        generatorCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        exception = assertThrows(NetworkModificationException.class, () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("VOLTAGE_LEVEL_NOT_FOUND : notFoundVoltageLevelId", exception.getMessage());

        // not found busbar section
        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        exception = assertThrows(NetworkModificationException.class, () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("BUSBAR_SECTION_NOT_FOUND : notFoundBusbarSection", exception.getMessage());

        // invalid min active power
        generatorCreationInfos.setVoltageLevelId("v2");

        generatorCreationInfos.setBusOrBusbarSectionId("1B");
        generatorCreationInfos.setMinP(Double.NaN);
        exception = assertThrows(ValidationException.class, () -> generatorCreationInfos.toModification().apply(getNetwork()));
        assertEquals("Generator 'idGenerator1': invalid value (NaN) for minimum P", exception.getMessage());

        // invalid min max reactive limit
        generatorCreationInfos.setMinP(0.0);
        generatorCreationInfos.setReactiveCapabilityCurve(false);
        generatorCreationInfos.setMinQ(Double.NaN);

        exception = assertThrows(NetworkModificationException.class, () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'idGenerator1' : minimum reactive power is not set", exception.getMessage());

        generatorCreationInfos.setMinQ(0.0);
        generatorCreationInfos.setReactiveCapabilityCurve(false);
        generatorCreationInfos.setMaxQ(Double.NaN);

        exception = assertThrows(NetworkModificationException.class, () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'idGenerator1' : maximum reactive power is not set", exception.getMessage());

        generatorCreationInfos.setReactiveCapabilityCurve(false);
        generatorCreationInfos.setMinQ(200.);
        generatorCreationInfos.setMaxQ(100.);

        exception = assertThrows(NetworkModificationException.class, () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'idGenerator1' : maximum reactive power is expected to be greater than or equal to minimum reactive power", exception.getMessage());

        // invalid reactive capability curve limit
        generatorCreationInfos.setMinQ(20.0);
        generatorCreationInfos.setReactiveCapabilityCurve(true);
        generatorCreationInfos.getReactiveCapabilityCurvePoints().get(0).setP(Double.NaN);

        exception = assertThrows(NetworkModificationException.class, () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'idGenerator1' : P is not set in a reactive capability curve limits point", exception.getMessage());

        // // try to create an existing generator
        generatorCreationInfos.setEquipmentId("v5generator");
        exception = assertThrows(NetworkModificationException.class, () -> generatorCreationInfos.toModification().check(getNetwork()));
        assertEquals("GENERATOR_ALREADY_EXISTS : v5generator", exception.getMessage());

        GeneratorCreationInfos generatorCreationInfos1 = GeneratorCreationInfos.builder()
            .equipmentId("v4Generator")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .droop(101f)
            .build();
        String message = assertThrows(NetworkModificationException.class,
            () -> generatorCreationInfos1.toModification().check(getNetwork())).getMessage();
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'v4Generator' : must have Droop between 0 and 100", message);

        GeneratorCreationInfos generatorModificationInfos2 = GeneratorCreationInfos.builder()
            .equipmentId("v4Generator")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .droop(-1f)
            .build();
        message = assertThrows(NetworkModificationException.class,
            () -> generatorModificationInfos2.toModification().check(getNetwork())).getMessage();
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'v4Generator' : must have Droop between 0 and 100", message);
    }

    @Test
    void testCreateWithShortCircuitErrors() throws Exception {
        // invalid short circuit transient reactance
        GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setDirectTransX(Double.NaN);

        ReportNode report = generatorCreationInfos.createSubReportNode(ReportNode.newRootReportNode().withMessageTemplate("", "").build());
        generatorCreationInfos.toModification().apply(getNetwork(), report);
        assertLogMessage("cannot add short-circuit extension on generator with id=idGenerator1 : Undefined directTransX", "ShortCircuitExtensionAddError", report);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("GENERATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idGenerator1", updatedValues.get("equipmentId"));
    }
}
