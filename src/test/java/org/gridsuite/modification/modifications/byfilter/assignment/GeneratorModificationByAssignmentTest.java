/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.assignment;

import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.*;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.BooleanAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.GeneratorField;
import org.gridsuite.modification.utils.TestUtils;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.MODIFY_GENERATOR_ERROR;
import static org.gridsuite.modification.utils.NetworkUtil.createGenerator;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class GeneratorModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
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
    private static final String GENERATOR_ID_11 = "gen11";
    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(GENERATOR_ID_1, GENERATOR_ID_2),
            FILTER_ID_2, Set.of(GENERATOR_ID_3, GENERATOR_ID_4),
            FILTER_ID_3, Set.of(GENERATOR_ID_5, GENERATOR_ID_6),
            FILTER_ID_4, Set.of(GENERATOR_ID_7, GENERATOR_ID_8),
            FILTER_ID_5, Set.of(GENERATOR_ID_9, GENERATOR_ID_10),
            FILTER_ID_6, Set.of(GENERATOR_ID_11)
    );

    @Override
    public Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
    }

    @Override
    protected void createEquipments() {
        getNetwork().getGenerator(GENERATOR_ID_1)
                .setTargetP(100)
                .setMaxP(500)
                .setMinP(0)
                .setTargetV(10)
                .setTargetQ(20)
                .newExtension(GeneratorStartupAdder.class)
                .withMarginalCost(30.)
                .withPlannedOutageRate(0.25)
                .withPlannedActivePowerSetpoint(40.)
                .withForcedOutageRate(0.55)
                .add();

        getNetwork().getGenerator(GENERATOR_ID_2)
                .setTargetP(200)
                .setMaxP(2000)
                .setMinP(10)
                .setTargetV(10)
                .setTargetQ(20)
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

        // use to get warning
        createGenerator(getNetwork().getVoltageLevel("v5"), GENERATOR_ID_11, 12, 100, 1.0, "cn10", 19, ConnectablePosition.Direction.TOP, 500, 20);
        getNetwork().getGenerator(GENERATOR_ID_11)
                .setTargetV(10)
                .newExtension(GeneratorStartupAdder.class)
                .withMarginalCost(30.)
                .withPlannedOutageRate(0.25)
                .withPlannedActivePowerSetpoint(40.)
                .withForcedOutageRate(0.55)
                .add();
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {

        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.REACTIVE_POWER_SET_POINT.name())
                .value(50.)
                .filters(List.of(filter1, filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.DROOP.name())
                .value(2.)
                .filters(List.of(filter3))
                .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.RATED_NOMINAL_POWER.name())
                .value(2.)
                .filters(List.of(filter5))
                .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.MARGINAL_COST.name())
                .value(2.)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos5 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.VOLTAGE_SET_POINT.name())
                .value(2.)
                .filters(List.of(filter4))
                .build();

        DoubleAssignmentInfos assignmentInfos6 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.PLANNED_ACTIVE_POWER_SET_POINT.name())
                .value(10.)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos7 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.MINIMUM_ACTIVE_POWER.name())
                .value(2.)
                .filters(List.of(filter1, filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos8 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.PLANNED_OUTAGE_RATE.name())
                .value(0.1)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos9 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.FORCED_OUTAGE_RATE.name())
                .value(0.05)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos10 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.MAXIMUM_ACTIVE_POWER.name())
                .value(300.)
                .filters(List.of(filter1, filter2, filter3, filter4, filter5))
                .build();

        DoubleAssignmentInfos assignmentInfos11 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.TRANSIENT_REACTANCE.name())
                .value(0.2)
                .filters(List.of(filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos12 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.STEP_UP_TRANSFORMER_REACTANCE.name())
                .value(0.3)
                .filters(List.of(filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos13 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.Q_PERCENT.name())
                .value(0.25)
                .filters(List.of(filter4))
                .build();

        BooleanAssignmentInfos assignmentInfos14 = BooleanAssignmentInfos.builder()
                .editedField(GeneratorField.VOLTAGE_REGULATOR_ON.name())
                .value(true)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos15 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.TRANSIENT_REACTANCE.name())
                .value(Double.NaN)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos16 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.STEP_UP_TRANSFORMER_REACTANCE.name())
                .value(Double.NaN)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos17 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.PLANNED_OUTAGE_RATE.name())
                .value(10.)
                .filters(List.of(filter6))
                .build();

        DoubleAssignmentInfos assignmentInfos18 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.FORCED_OUTAGE_RATE.name())
                .value(11.)
                .filters(List.of(filter6))
                .build();

        DoubleAssignmentInfos assignmentInfos19 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.Q_PERCENT.name())
                .value(120.)
                .filters(List.of(filter6))
                .build();

        DoubleAssignmentInfos assignmentInfos20 = DoubleAssignmentInfos.builder()
            .editedField(GeneratorField.Q_PERCENT.name())
            .value(120.)
            .filters(List.of(new FilterInfos(UUID.randomUUID(), "filterNotFound")))
            .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(
                assignmentInfos1,
                assignmentInfos2,
                assignmentInfos3,
                assignmentInfos4,
                assignmentInfos5,
                assignmentInfos6,
                assignmentInfos7,
                assignmentInfos8,
                assignmentInfos9,
                assignmentInfos10,
                assignmentInfos11,
                assignmentInfos12,
                assignmentInfos13,
                assignmentInfos14,
                assignmentInfos15,
                assignmentInfos16,
                assignmentInfos17,
                assignmentInfos18,
                assignmentInfos19,
                assignmentInfos20
        ));

        return infosList;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Generator generator1 = getNetwork().getGenerator(GENERATOR_ID_1);
        GeneratorStartup generatorStartup1 = generator1.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup1);
        assertThat(generator1.getProperty("propertyName")).isEqualTo("propertyValue");
        assertEquals(100, generator1.getTargetP(), 0);
        assertEquals(2, generatorStartup1.getMarginalCost(), 0);
        assertEquals(0.1, generatorStartup1.getPlannedOutageRate(), 0);
        assertEquals(0.05, generatorStartup1.getForcedOutageRate(), 0);
        assertEquals(10, generatorStartup1.getPlannedActivePowerSetpoint(), 0);
        assertEquals(300., generator1.getMaxP(), 0);
        assertEquals(2, generator1.getMinP(), 0);
        assertTrue(generator1.isVoltageRegulatorOn());
        ActivePowerControl<Generator> activePowerControl1 = generator1.getExtension(ActivePowerControl.class);
        assertNull(activePowerControl1);

        Generator generator2 = getNetwork().getGenerator(GENERATOR_ID_2);
        GeneratorStartup generatorStartup2 = generator2.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup2);
        assertThat(generator2.getProperty("propertyName")).isEqualTo("propertyValue");
        assertEquals(200, generator2.getTargetP(), 0);
        assertEquals(2, generatorStartup2.getMarginalCost(), 0);
        assertEquals(0.1, generatorStartup2.getPlannedOutageRate(), 0);
        assertEquals(0.05, generatorStartup2.getForcedOutageRate(), 0);
        assertEquals(10, generatorStartup2.getPlannedActivePowerSetpoint(), 0);
        assertEquals(300., generator2.getMaxP(), 0);
        assertEquals(2, generator2.getMinP(), 0);

        Generator generator3 = getNetwork().getGenerator(GENERATOR_ID_3);
        GeneratorShortCircuit generatorShortCircuit3 = generator3.getExtension(GeneratorShortCircuit.class);
        assertNotNull(generatorShortCircuit3);
        assertEquals(300, generator3.getTargetP(), 0);
        assertEquals(0.2, generatorShortCircuit3.getDirectTransX(), 0);
        assertEquals(0.3, generatorShortCircuit3.getStepUpTransformerX(), 0);
        assertEquals(300., generator3.getMaxP(), 0);
        assertEquals(2, generator3.getMinP(), 0);

        Generator generator4 = getNetwork().getGenerator(GENERATOR_ID_4);
        GeneratorShortCircuit generatorShortCircuit4 = generator4.getExtension(GeneratorShortCircuit.class);
        assertNotNull(generatorShortCircuit4);
        assertEquals(0.2, generatorShortCircuit4.getDirectTransX(), 0);
        assertEquals(0.3, generatorShortCircuit4.getStepUpTransformerX(), 0);
        assertEquals(400, generator4.getTargetP(), 0);
        //targetP is 400 MaxP won't change
        assertEquals(700.0, generator4.getMaxP(), 0);
        assertEquals(2, generator4.getMinP(), 0);

        Generator generator5 = getNetwork().getGenerator(GENERATOR_ID_5);
        ActivePowerControl<Generator> activePowerControl5 = generator5.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(300., generator5.getMaxP(), 0);
        assertEquals(2, activePowerControl5.getDroop(), 0);

        Generator generator6 = getNetwork().getGenerator(GENERATOR_ID_6);
        ActivePowerControl<Generator> activePowerControl6 = generator6.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl6);
        assertEquals(300., generator6.getMaxP(), 0);
        assertEquals(2, activePowerControl6.getDroop(), 0);

        Generator generator7 = getNetwork().getGenerator(GENERATOR_ID_7);
        CoordinatedReactiveControl coordinatedReactiveControl7 = generator7.getExtension(CoordinatedReactiveControl.class);
        assertNotNull(coordinatedReactiveControl7);
        GeneratorStartup generatorStartup7 = generator7.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup7);
        assertEquals(50, generatorStartup7.getMarginalCost(), 0);
        assertEquals(0.25, coordinatedReactiveControl7.getQPercent(), 0);

        Generator generator8 = getNetwork().getGenerator(GENERATOR_ID_8);
        CoordinatedReactiveControl coordinatedReactiveControl8 = generator8.getExtension(CoordinatedReactiveControl.class);
        assertNotNull(coordinatedReactiveControl8);
        GeneratorStartup generatorStartup8 = generator8.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup8);
        assertEquals(60, generatorStartup8.getMarginalCost(), 0);
        assertEquals(0.25, coordinatedReactiveControl8.getQPercent(), 0);

        assertEquals(2, getNetwork().getGenerator(GENERATOR_ID_9).getRatedS(), 0);
        assertEquals(2, getNetwork().getGenerator(GENERATOR_ID_10).getRatedS(), 0);

        // check failed with filter 6 and generator 11
        List<String> filter6OnPlannedOutageRateLogs = TestUtils.getAllMessages(reportNode.getChildren().getFirst().getChildren().get(17));
        assertTrue(filter6OnPlannedOutageRateLogs.contains("Edited field : PLANNED_OUTAGE_RATE"));
        assertTrue(filter6OnPlannedOutageRateLogs.contains("No equipment have been modified"));
        assertTrue(filter6OnPlannedOutageRateLogs.contains("Cannot modify equipment gen11 : "
                + MODIFY_GENERATOR_ERROR.getMessage()
                + " : Generator 'gen11' : must have PLANNED_OUTAGE_RATE between 0 and 1"));

        List<String> filter6OnForcedOutageRateLogs = TestUtils.getAllMessages(reportNode.getChildren().getFirst().getChildren().get(18));
        assertTrue(filter6OnForcedOutageRateLogs.contains("Edited field : FORCED_OUTAGE_RATE"));
        assertTrue(filter6OnForcedOutageRateLogs.contains("No equipment have been modified"));
        assertTrue(filter6OnForcedOutageRateLogs.contains("Cannot modify equipment gen11 : "
                + MODIFY_GENERATOR_ERROR.getMessage()
                + " : Generator 'gen11' : must have FORCED_OUTAGE_RATE between 0 and 1"));

        List<String> filter6OnQPercentLogs = TestUtils.getAllMessages(reportNode.getChildren().getFirst().getChildren().get(19));
        assertTrue(filter6OnQPercentLogs.contains("Edited field : Q_PERCENT"));
        assertTrue(filter6OnQPercentLogs.contains("No equipment have been modified"));
        assertTrue(filter6OnQPercentLogs.contains("Cannot modify equipment gen11 : "
                + MODIFY_GENERATOR_ERROR.getMessage()
                + " : Generator 'gen11' : must have Q_Percent between 0 and 100"));
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
