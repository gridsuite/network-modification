/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.formula;

import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.*;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.byfilter.equipmentfield.GeneratorField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createGenerator;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class GeneratorByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String GENERATOR_ID_1 = "idGenerator";
    private static final String GENERATOR_ID_2 = "v5generator";
    private static final String GENERATOR_ID_3 = "v6generator";
    private static final String GENERATOR_ID_4 = "gen4";
    private static final String GENERATOR_ID_5 = "gen5";
    private static final String GENERATOR_ID_6 = "gen6";
    private static final String GENERATOR_ID_7 = "gen7";
    private static final String GENERATOR_ID_8 = "gen8";
    private static final String GENERATOR_ID_9 = "gen9";
    private static final String GENERATOR_ID_10 = "gen10";

    @Test
    void testCreateWithWarning() throws Exception {
        IdentifiableAttributes identifiableAttributes = new IdentifiableAttributes(GENERATOR_ID_1, getIdentifiableType(), 1.0);
        FilterEquipments filter = FilterEquipments.builder().filterId(FILTER_WITH_ONE_WRONG_ID)
                .identifiableAttributes(List.of(identifiableAttributes))
                .notFoundEquipments(List.of("wrongId"))
                .build();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(Map.of(FILTER_WITH_ONE_WRONG_ID, filter));

        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filterWithOneWrongId))
                .editedField(GeneratorField.ACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(55.).build())
                .operator(Operator.ADDITION)
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(20.).build())
                .build();

        ByFormulaModificationInfos modificationInfos = ByFormulaModificationInfos.builder()
                .identifiableType(getIdentifiableType())
                .formulaInfosList(List.of(formulaInfos))
                .stashed(false)
                .build();
        apply(modificationInfos);
        assertEquals(75, getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 0);
    }

    protected void createEquipments() {
        getNetwork().getGenerator(GENERATOR_ID_1)
                .setTargetP(100)
                .setMaxP(500)
                .setMinP(0)
                .newExtension(GeneratorStartupAdder.class)
                .withMarginalCost(30.)
                .withPlannedOutageRate(0.25)
                .withPlannedActivePowerSetpoint(40.)
                .withForcedOutageRate(0.55)
                .add();

        getNetwork().getGenerator(GENERATOR_ID_2)
                .setTargetP(200)
                .setMaxP(2000)
                .setMinP(50)
                .newExtension(GeneratorStartupAdder.class)
                .withMarginalCost(30.)
                .withPlannedOutageRate(0.25)
                .withPlannedActivePowerSetpoint(40.)
                .withForcedOutageRate(0.55)
                .add();

        getNetwork().getGenerator(GENERATOR_ID_3)
                .setTargetP(300)
                .setMaxP(2000)
                .setMinP(70)
                .newExtension(GeneratorShortCircuitAdder.class)
                .withDirectTransX(40.)
                .withStepUpTransformerX(38.)
                .add();

        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_4, 3, 400, 1.0, "cn10", 11, ConnectablePosition.Direction.TOP, 700, 110);
        getNetwork().getGenerator(GENERATOR_ID_4)
                        .newExtension(GeneratorShortCircuitAdder.class)
                        .withDirectTransX(46.)
                        .withStepUpTransformerX(50.)
                        .add();

        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_5, 20, 200, 1.0, "cn10", 12, ConnectablePosition.Direction.TOP, 2000, 50);
        getNetwork().getGenerator(GENERATOR_ID_5).newExtension(ActivePowerControlAdder.class).withDroop(2).add();

        createGenerator(getNetwork().getVoltageLevel("v2"), GENERATOR_ID_6, 11, 100, 1.0, "cn10", 13, ConnectablePosition.Direction.TOP, 500, 20);
        getNetwork().getGenerator(GENERATOR_ID_6).newExtension(ActivePowerControlAdder.class).withDroop(3).add();

        createGenerator(getNetwork().getVoltageLevel("v6"), GENERATOR_ID_7, 10, 200, 1.0, "cn10", 14, ConnectablePosition.Direction.TOP, 2000, 50);
        getNetwork().getGenerator(GENERATOR_ID_7).newExtension(CoordinatedReactiveControlAdder.class)
                        .withQPercent(6)
                        .add();
        getNetwork().getGenerator(GENERATOR_ID_7).newExtension(GeneratorStartupAdder.class).withMarginalCost(50).add();

        createGenerator(getNetwork().getVoltageLevel("v3"), GENERATOR_ID_8, 10, 100, 1.0, "cn10", 15, ConnectablePosition.Direction.TOP, 500, 20);
        getNetwork().getGenerator(GENERATOR_ID_8).newExtension(CoordinatedReactiveControlAdder.class)
                .withQPercent(12)
                .add();
        getNetwork().getGenerator(GENERATOR_ID_8).newExtension(GeneratorStartupAdder.class).withMarginalCost(60).add();

        createGenerator(getNetwork().getVoltageLevel("v4"), GENERATOR_ID_9, 10, 200, 1.0, "cn10", 16, ConnectablePosition.Direction.TOP, 2000, 50);
        getNetwork().getGenerator(GENERATOR_ID_9).setRatedS(60.);

        createGenerator(getNetwork().getVoltageLevel("v5"), GENERATOR_ID_10, 10, 100, 1.0, "cn10", 17, ConnectablePosition.Direction.TOP, 500, 20);
        getNetwork().getGenerator(GENERATOR_ID_10).setRatedS(30.);
    }

    @Override
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
            new IdentifiableAttributes(GENERATOR_ID_1, getIdentifiableType(), 1.0),
            new IdentifiableAttributes(GENERATOR_ID_2, getIdentifiableType(), 2.0)))
                .build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(
            new IdentifiableAttributes(GENERATOR_ID_3, getIdentifiableType(), 2.0),
            new IdentifiableAttributes(GENERATOR_ID_4, getIdentifiableType(), 5.0)))
                .build();

        FilterEquipments filter3 = FilterEquipments.builder().filterId(FILTER_ID_3).identifiableAttributes(List.of(
            new IdentifiableAttributes(GENERATOR_ID_5, getIdentifiableType(), 6.0),
            new IdentifiableAttributes(GENERATOR_ID_6, getIdentifiableType(), 7.0)))
                .build();

        FilterEquipments filter4 = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(
            new IdentifiableAttributes(GENERATOR_ID_7, getIdentifiableType(), 3.0),
            new IdentifiableAttributes(GENERATOR_ID_8, getIdentifiableType(), 8.0)))
                .build();

        FilterEquipments filter5 = FilterEquipments.builder().filterId(FILTER_ID_5).identifiableAttributes(List.of(
            new IdentifiableAttributes(GENERATOR_ID_9, getIdentifiableType(), 0.0),
            new IdentifiableAttributes(GENERATOR_ID_10, getIdentifiableType(), 10.0)))
                .build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2, FILTER_ID_3, filter3, FILTER_ID_4, filter4, FILTER_ID_5, filter5);
    }

    @Override
    protected List<FormulaInfos> getFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(GeneratorField.ACTIVE_POWER_SET_POINT.name(),
                List.of(filter1, filter2),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MINIMUM_ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(50.).build());

        FormulaInfos formulaInfos2 = getFormulaInfo(GeneratorField.DROOP.name(),
                List.of(filter3),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.DROOP.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos3 = getFormulaInfo(GeneratorField.RATED_NOMINAL_POWER.name(),
                List.of(filter5),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MAXIMUM_ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MINIMUM_ACTIVE_POWER.name()).build());

        FormulaInfos formulaInfos4 = getFormulaInfo(GeneratorField.MARGINAL_COST.name(),
                List.of(filter1),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MARGINAL_COST.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos5 = getFormulaInfo(GeneratorField.VOLTAGE_SET_POINT.name(),
                List.of(filter4),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.VOLTAGE_SET_POINT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos6 = getFormulaInfo(GeneratorField.PLANNED_ACTIVE_POWER_SET_POINT.name(),
                List.of(filter1),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.PLANNED_ACTIVE_POWER_SET_POINT.name()).build(),
                ReferenceFieldOrValue.builder().value(10.).build());

        FormulaInfos formulaInfos7 = getFormulaInfo(GeneratorField.MINIMUM_ACTIVE_POWER.name(),
                List.of(filter1, filter2),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MINIMUM_ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(50.).build());

        FormulaInfos formulaInfos8 = getFormulaInfo(GeneratorField.PLANNED_OUTAGE_RATE.name(),
                List.of(filter1),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.PLANNED_OUTAGE_RATE.name()).build(),
                ReferenceFieldOrValue.builder().value(0.1).build());

        FormulaInfos formulaInfos9 = getFormulaInfo(GeneratorField.FORCED_OUTAGE_RATE.name(),
                List.of(filter1),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.FORCED_OUTAGE_RATE.name()).build(),
                ReferenceFieldOrValue.builder().value(10.).build());

        FormulaInfos formulaInfos10 = getFormulaInfo(GeneratorField.MAXIMUM_ACTIVE_POWER.name(),
                List.of(filter1, filter2, filter3, filter4, filter5),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MAXIMUM_ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos11 = getFormulaInfo(GeneratorField.TRANSIENT_REACTANCE.name(),
                List.of(filter2),
                Operator.SUBTRACTION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.TRANSIENT_REACTANCE.name()).build(),
                ReferenceFieldOrValue.builder().value(0.2).build());

        FormulaInfos formulaInfos12 = getFormulaInfo(GeneratorField.STEP_UP_TRANSFORMER_REACTANCE.name(),
                List.of(filter2),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.STEP_UP_TRANSFORMER_REACTANCE.name()).build(),
                ReferenceFieldOrValue.builder().value(0.3).build());

        FormulaInfos formulaInfos13 = getFormulaInfo(GeneratorField.Q_PERCENT.name(),
                List.of(filter4),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.Q_PERCENT.name()).build(),
                ReferenceFieldOrValue.builder().value(0.25).build());

        return List.of(formulaInfos1,
                formulaInfos2,
                formulaInfos3,
                formulaInfos4,
                formulaInfos5,
                formulaInfos6,
                formulaInfos7,
                formulaInfos8,
                formulaInfos9,
                formulaInfos10,
                formulaInfos11,
                formulaInfos12,
                formulaInfos13);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Generator generator1 = getNetwork().getGenerator(GENERATOR_ID_1);
        GeneratorStartup generatorStartup1 = generator1.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup1);
        assertEquals(50, generator1.getTargetP(), 0);
        assertEquals(15, generatorStartup1.getMarginalCost(), 0);
        assertEquals(0.025, generatorStartup1.getPlannedOutageRate(), 0);
        assertEquals(0.055, generatorStartup1.getForcedOutageRate(), 0);
        assertEquals(50, generatorStartup1.getPlannedActivePowerSetpoint(), 0);
        assertEquals(502, generator1.getMaxP(), 0);
        assertEquals(50, generator1.getMinP(), 0);

        Generator generator2 = getNetwork().getGenerator(GENERATOR_ID_2);
        GeneratorStartup generatorStartup2 = generator2.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup2);
        assertEquals(100, generator2.getTargetP(), 0);
        assertEquals(15, generatorStartup2.getMarginalCost(), 0);
        assertEquals(0.025, generatorStartup2.getPlannedOutageRate(), 0);
        assertEquals(0.055, generatorStartup2.getForcedOutageRate(), 0);
        assertEquals(50, generatorStartup2.getPlannedActivePowerSetpoint(), 0);
        assertEquals(2002, generator2.getMaxP(), 0);
        //value doesn't change anymore since MinP value can't be smaller than plannedActivePowerSetpoint
        assertEquals(50, generator2.getMinP(), 0);

        Generator generator3 = getNetwork().getGenerator(GENERATOR_ID_3);
        GeneratorShortCircuit generatorShortCircuit3 = generator3.getExtension(GeneratorShortCircuit.class);
        assertNotNull(generatorShortCircuit3);
        assertEquals(120, generator3.getTargetP(), 0);
        assertEquals(39.8, generatorShortCircuit3.getDirectTransX(), 0);
        assertEquals(11.4, generatorShortCircuit3.getStepUpTransformerX(), 0);
        assertEquals(2002, generator3.getMaxP(), 0);
        assertEquals(120, generator3.getMinP(), 0);

        Generator generator4 = getNetwork().getGenerator(GENERATOR_ID_4);
        GeneratorShortCircuit generatorShortCircuit4 = generator4.getExtension(GeneratorShortCircuit.class);
        assertNotNull(generatorShortCircuit4);
        assertEquals(45.8, generatorShortCircuit4.getDirectTransX(), 0);
        assertEquals(15.0, generatorShortCircuit4.getStepUpTransformerX(), 0);
        assertEquals(160, generator4.getTargetP(), 0);
        assertEquals(702, generator4.getMaxP(), 0);
        assertEquals(160, generator4.getMinP(), 0);

        Generator generator5 = getNetwork().getGenerator(GENERATOR_ID_5);
        ActivePowerControl activePowerControl5 = generator5.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(2002, generator5.getMaxP(), 0);
        assertEquals(4, activePowerControl5.getDroop(), 0);

        Generator generator6 = getNetwork().getGenerator(GENERATOR_ID_6);
        ActivePowerControl activePowerControl6 = generator6.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl6);
        assertEquals(502, generator6.getMaxP(), 0);
        assertEquals(6, activePowerControl6.getDroop(), 0);

        Generator generator7 = getNetwork().getGenerator(GENERATOR_ID_7);
        CoordinatedReactiveControl coordinatedReactiveControl7 = generator7.getExtension(CoordinatedReactiveControl.class);
        assertNotNull(coordinatedReactiveControl7);
        GeneratorStartup generatorStartup7 = generator7.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup7);
        assertEquals(50, generatorStartup7.getMarginalCost(), 0);
        assertEquals(24, coordinatedReactiveControl7.getQPercent(), 0);

        Generator generator8 = getNetwork().getGenerator(GENERATOR_ID_8);
        CoordinatedReactiveControl coordinatedReactiveControl8 = generator8.getExtension(CoordinatedReactiveControl.class);
        assertNotNull(coordinatedReactiveControl8);
        GeneratorStartup generatorStartup8 = generator8.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup8);
        assertEquals(60, generatorStartup8.getMarginalCost(), 0);
        assertEquals(48, coordinatedReactiveControl8.getQPercent(), 0);

        assertEquals(40, getNetwork().getGenerator(GENERATOR_ID_9).getRatedS(), 0);
        assertEquals(25, getNetwork().getGenerator(GENERATOR_ID_10).getRatedS(), 0);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.GENERATOR;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.GENERATOR;
    }
}
