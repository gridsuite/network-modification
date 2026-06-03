/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ValidationException;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.FreePropertyModel;
import org.gridsuite.modification.model.GeneratorCreationModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.ReactiveCapabilityCurvePointsModel;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
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
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationModel buildModification() {
        // create new generator in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        return GeneratorCreationModel.builder()
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
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsModel(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurvePointsModel(5.6, 9.8, 10.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getGenerator("idGenerator1"));
        assertEquals(1, getNetwork().getVoltageLevel("v2").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator1")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getGenerator("idGenerator1").getProperty(PROPERTY_NAME));
        Generator generator = getNetwork().getGenerator("idGenerator1");
        assertEquals(225.0, generator.getTargetV());
        assertTrue(Double.isNaN(generator.getEquivalentLocalTargetV()));
    }

    @Override
    protected void checkModification() {
        Network network = getNetwork();
        // invalid Generator id
        GeneratorCreationModel generatorCreationModel = (GeneratorCreationModel) buildModification();
        generatorCreationModel.setEquipmentId("");
        GeneratorCreation generatorCreation = (GeneratorCreation) generatorCreationModel.toModification();
        PowsyblException exception = assertThrows(PowsyblException.class, () -> generatorCreation.apply(network));
        assertEquals("Invalid id ''", exception.getMessage());

        // not found voltage level
        generatorCreationModel.setEquipmentId("idGenerator1");
        generatorCreationModel.setVoltageLevelId("notFoundVoltageLevelId");
        GeneratorCreation generatorCreation1 = (GeneratorCreation) generatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> generatorCreation1.check(network));
        assertEquals("VOLTAGE_LEVEL_NOT_FOUND : notFoundVoltageLevelId", exception.getMessage());

        // not found busbar section
        generatorCreationModel.setVoltageLevelId("v2");
        generatorCreationModel.setBusOrBusbarSectionId("notFoundBusbarSection");
        GeneratorCreation generatorCreation2 = (GeneratorCreation) generatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> generatorCreation2.check(network));
        assertEquals("BUSBAR_SECTION_NOT_FOUND : notFoundBusbarSection", exception.getMessage());

        // invalid min active power
        generatorCreationModel.setVoltageLevelId("v2");

        generatorCreationModel.setBusOrBusbarSectionId("1B");
        generatorCreationModel.setMinP(Double.NaN);
        GeneratorCreation generatorCreation3 = (GeneratorCreation) generatorCreationModel.toModification();
        exception = assertThrows(ValidationException.class, () -> generatorCreation3.apply(network));
        assertEquals("Generator 'idGenerator1': invalid value (NaN) for minimum P", exception.getMessage());

        // invalid min max reactive limit
        generatorCreationModel.setMinP(0.0);
        generatorCreationModel.setReactiveCapabilityCurve(false);
        generatorCreationModel.setMinQ(Double.NaN);
        GeneratorCreation generatorCreation4 = (GeneratorCreation) generatorCreationModel.toModification();

        exception = assertThrows(NetworkModificationException.class, () -> generatorCreation4.check(network));
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'idGenerator1' : minimum reactive power is not set", exception.getMessage());

        generatorCreationModel.setMinQ(0.0);
        generatorCreationModel.setReactiveCapabilityCurve(false);
        generatorCreationModel.setMaxQ(Double.NaN);
        GeneratorCreation generatorCreation5 = (GeneratorCreation) generatorCreationModel.toModification();

        exception = assertThrows(NetworkModificationException.class, () -> generatorCreation5.check(network));
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'idGenerator1' : maximum reactive power is not set", exception.getMessage());

        generatorCreationModel.setReactiveCapabilityCurve(false);
        generatorCreationModel.setMinQ(200.);
        generatorCreationModel.setMaxQ(100.);
        GeneratorCreation generatorCreation6 = (GeneratorCreation) generatorCreationModel.toModification();

        exception = assertThrows(NetworkModificationException.class, () -> generatorCreation6.check(network));
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'idGenerator1' : maximum reactive power is expected to be greater than or equal to minimum reactive power", exception.getMessage());

        // invalid reactive capability curve limit
        generatorCreationModel.setMinQ(20.0);
        generatorCreationModel.setReactiveCapabilityCurve(true);
        generatorCreationModel.getReactiveCapabilityCurvePoints().getFirst().setP(Double.NaN);
        GeneratorCreation generatorCreation7 = (GeneratorCreation) generatorCreationModel.toModification();

        exception = assertThrows(NetworkModificationException.class, () -> generatorCreation7.check(network));
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'idGenerator1' : P is not set in a reactive capability curve limits point", exception.getMessage());

        // // try to create an existing generator
        generatorCreationModel.setEquipmentId("v5generator");
        GeneratorCreation generatorCreation8 = (GeneratorCreation) generatorCreationModel.toModification();
        exception = assertThrows(NetworkModificationException.class, () -> generatorCreation8.check(network));
        assertEquals("GENERATOR_ALREADY_EXISTS : v5generator", exception.getMessage());

        GeneratorCreationModel generatorCreationModel1 = GeneratorCreationModel.builder()
            .equipmentId("v4Generator")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .droop(101f)
            .build();
        GeneratorCreation generatorCreation9 = (GeneratorCreation) generatorCreationModel1.toModification();
        String message = assertThrows(NetworkModificationException.class,
            () -> generatorCreation9.check(network)).getMessage();
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'v4Generator' : must have Droop between 0 and 100", message);

        GeneratorCreationModel generatorCreationModel2 = GeneratorCreationModel.builder()
            .equipmentId("v4Generator")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .droop(-1f)
            .build();
        GeneratorCreation generatorCreation10 = (GeneratorCreation) generatorCreationModel2.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> generatorCreation10.check(network)).getMessage();
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'v4Generator' : must have Droop between 0 and 100", message);

        GeneratorCreationModel generatorCreationModel3 = GeneratorCreationModel.builder()
            .equipmentId("v4Generator")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .targetV(-100d)
            .build();
        GeneratorCreation generatorCreation11 = (GeneratorCreation) generatorCreationModel3.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> generatorCreation11.check(network)).getMessage();
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'v4Generator' : can not have a negative value for Target Voltage", message);

        GeneratorCreationModel generatorCreationModel4 = GeneratorCreationModel.builder()
            .equipmentId("v4Generator")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .ratedS(-100d)
            .build();
        GeneratorCreation generatorCreation12 = (GeneratorCreation) generatorCreationModel4.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> generatorCreation12.check(network)).getMessage();
        assertEquals("CREATE_GENERATOR_ERROR : Generator 'v4Generator' : can not have a negative value for Rated apparent power", message);
    }

    @Test
    void testCreateWithShortCircuitErrors() {
        // invalid short circuit transient reactance
        GeneratorCreationModel generatorCreationModel = (GeneratorCreationModel) buildModification();
        generatorCreationModel.setDirectTransX(Double.NaN);

        ReportNode report = generatorCreationModel.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        generatorCreationModel.toModification().apply(getNetwork(), report);
        assertLogMessage("cannot add short-circuit extension on generator with id=idGenerator1 : Undefined directTransX", "network.modification.ShortCircuitExtensionAddError", report);
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("GENERATOR_CREATION", modificationModel.getType().toString());
        Map<String, String> updatedValues = modificationModel.getMapMessageValues();
        assertEquals("idGenerator1", updatedValues.get("equipmentId"));
    }
}
