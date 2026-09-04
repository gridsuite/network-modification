/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.VoltageLevelField;
import org.gridsuite.modification.utils.TestUtils;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.error.NetworkModificationExceptionType.MODIFY_VOLTAGE_LEVEL_ERROR;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class VoltageLevelModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
    private static final String VOLTAGE_LEVEL_ID_1 = "v1";
    private static final String VOLTAGE_LEVEL_ID_2 = "v2";
    private static final String VOLTAGE_LEVEL_ID_3 = "v3";
    private static final String VOLTAGE_LEVEL_ID_4 = "v4";
    private static final String VOLTAGE_LEVEL_ID_5 = "v5";
    private static final String VOLTAGE_LEVEL_ID_6 = "v6";
    private static final String VOLTAGE_LEVEL_ID_7 = "v7";
    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(VOLTAGE_LEVEL_ID_1, VOLTAGE_LEVEL_ID_2),
            FILTER_ID_2, Set.of(VOLTAGE_LEVEL_ID_3, VOLTAGE_LEVEL_ID_4),
            FILTER_ID_3, Set.of(VOLTAGE_LEVEL_ID_5, VOLTAGE_LEVEL_ID_6),
            FILTER_ID_4, Set.of(VOLTAGE_LEVEL_ID_2, VOLTAGE_LEVEL_ID_5),
            FILTER_ID_5, Set.of(VOLTAGE_LEVEL_ID_4, VOLTAGE_LEVEL_ID_6),
            FILTER_ID_6, Set.of(VOLTAGE_LEVEL_ID_7)
    );

    @Override
    public Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
    }

    @Override
    protected void createEquipments() {
        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_1).setNominalV(400)
                .setHighVoltageLimit(200)
                .setLowVoltageLimit(100)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(10).withIpMax(120).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_2).setNominalV(150)
                .setLowVoltageLimit(100)
                .setHighVoltageLimit(1000)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(10).withIpMax(120).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_3).setNominalV(70)
                .setLowVoltageLimit(50)
                .setHighVoltageLimit(250)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(50).withIpMax(150).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_4).setNominalV(100)
                .setLowVoltageLimit(70)
                .setHighVoltageLimit(300)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(10).withIpMax(100).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_5).setNominalV(210)
                .setLowVoltageLimit(10)
                .setHighVoltageLimit(500)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(25).withIpMax(75).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_6).setNominalV(750)
                .setHighVoltageLimit(1000)
                .setLowVoltageLimit(90)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(100).withIpMax(200).add();

        // to test exceptions
        getNetwork().newVoltageLevel()
                .setId(VOLTAGE_LEVEL_ID_7)
                .setName(VOLTAGE_LEVEL_ID_7)
                .setTopologyKind(TopologyKind.NODE_BREAKER)
                .setNominalV(400)
                .setHighVoltageLimit(430)
                .setLowVoltageLimit(370)
                .add();
        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_7)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(100).withIpMax(200).add();
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
            .editedField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name())
            .value(10.)
            .filters(List.of(filter1, filter2))
            .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
            .editedField(VoltageLevelField.HIGH_VOLTAGE_LIMIT.name())
            .value(120.)
            .filters(List.of(filter3))
            .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
            .editedField(VoltageLevelField.NOMINAL_VOLTAGE.name())
            .value(150.)
            .filters(List.of(filter4))
            .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.LOW_SHORT_CIRCUIT_CURRENT_LIMIT.name())
                .value(2.)
                .filters(List.of(filter5))
                .build();

        DoubleAssignmentInfos assignmentInfos5 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.HIGH_SHORT_CIRCUIT_CURRENT_LIMIT.name())
                .value(80.)
                .filters(List.of(filter4, filter5))
                .build();

        DoubleAssignmentInfos assignmentInfos6 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name())
                .value(440.)
                .filters(List.of(filter6))
                .build();

        DoubleAssignmentInfos assignmentInfos7 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.HIGH_VOLTAGE_LIMIT.name())
                .value(360.)
                .filters(List.of(filter6))
                .build();

        DoubleAssignmentInfos assignmentInfos8 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.LOW_SHORT_CIRCUIT_CURRENT_LIMIT.name())
                .value(220.)
                .filters(List.of(filter6))
                .build();

        DoubleAssignmentInfos assignmentInfos9 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.HIGH_SHORT_CIRCUIT_CURRENT_LIMIT.name())
                .value(80.)
                .filters(List.of(filter6))
                .build();

        return List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3, assignmentInfos4, assignmentInfos5,
                assignmentInfos6, assignmentInfos7, assignmentInfos8, assignmentInfos9);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.VOLTAGE_LEVEL;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.VOLTAGE_LEVEL;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(10, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_1).getLowVoltageLimit(), 0);
        assertEquals(10, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_2).getLowVoltageLimit(), 0);
        assertEquals(10, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_3).getLowVoltageLimit(), 0);

        VoltageLevel voltageLevel4 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_4);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit4 = voltageLevel4.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit4);
        assertEquals(10, voltageLevel4.getLowVoltageLimit(), 0);
        assertEquals(2, identifiableShortCircuit4.getIpMin(), 0);
        assertEquals(80, identifiableShortCircuit4.getIpMax(), 0);

        VoltageLevel voltageLevel5 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_5);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit5 = voltageLevel5.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit5);
        assertEquals(120, voltageLevel5.getHighVoltageLimit(), 0);
        assertEquals(150, voltageLevel5.getNominalV(), 0);
        assertEquals(80, identifiableShortCircuit5.getIpMax(), 0);

        VoltageLevel voltageLevel6 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_6);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit6 = voltageLevel6.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit6);
        assertEquals(120, voltageLevel6.getHighVoltageLimit(), 0);
        assertEquals(2, identifiableShortCircuit6.getIpMin(), 0);
        assertEquals(80, identifiableShortCircuit6.getIpMax(), 0);

        // check failed with filter 6 and voltage level 7
        List<String> filter6OnLowVoltageLimitLogs = TestUtils.getAllMessages(reportNode.getChildren().getFirst().getChildren().get(5));
        assertTrue(filter6OnLowVoltageLimitLogs.contains("Edited field : LOW_VOLTAGE_LIMIT"));
        assertTrue(filter6OnLowVoltageLimitLogs.contains("No equipment have been modified"));
        assertTrue(filter6OnLowVoltageLimitLogs.contains("Cannot modify equipment v7 : "
                + MODIFY_VOLTAGE_LEVEL_ERROR.getMessage()
                + " : Voltage level 'v7' :  Low voltage limit (440.0) must be inferior to High voltage limit (430.0)"));

        List<String> filter6OnHighVoltageLimitLogs = TestUtils.getAllMessages(reportNode.getChildren().getFirst().getChildren().get(6));
        assertTrue(filter6OnHighVoltageLimitLogs.contains("Edited field : HIGH_VOLTAGE_LIMIT"));
        assertTrue(filter6OnHighVoltageLimitLogs.contains("No equipment have been modified"));
        assertTrue(filter6OnHighVoltageLimitLogs.contains("Cannot modify equipment v7 : "
                + MODIFY_VOLTAGE_LEVEL_ERROR.getMessage()
                + " : Voltage level 'v7' :  High voltage limit (360.0) must be superior to Low voltage limit (370.0)"));

        List<String> filter6OnLowShortCircuitCurrentLimitLogs = TestUtils.getAllMessages(reportNode.getChildren().getFirst().getChildren().get(7));
        assertTrue(filter6OnLowShortCircuitCurrentLimitLogs.contains("Edited field : LOW_SHORT_CIRCUIT_CURRENT_LIMIT"));
        assertTrue(filter6OnLowShortCircuitCurrentLimitLogs.contains("No equipment have been modified"));
        assertTrue(filter6OnLowShortCircuitCurrentLimitLogs.contains("Cannot modify equipment v7 : "
                + MODIFY_VOLTAGE_LEVEL_ERROR.getMessage()
                + " : Voltage level 'v7' :  Low short circuit current limit (220.0) must be inferior to High short circuit current limit (200.0)"));

        List<String> filter6OnHighShortCircuitCurrentLimitLogs = TestUtils.getAllMessages(reportNode.getChildren().getFirst().getChildren().get(8));
        assertTrue(filter6OnHighShortCircuitCurrentLimitLogs.contains("Edited field : HIGH_SHORT_CIRCUIT_CURRENT_LIMIT"));
        assertTrue(filter6OnHighShortCircuitCurrentLimitLogs.contains("No equipment have been modified"));
        assertTrue(filter6OnHighShortCircuitCurrentLimitLogs.contains("Cannot modify equipment v7 : "
                + MODIFY_VOLTAGE_LEVEL_ERROR.getMessage()
                + " : Voltage level 'v7' :  High short circuit current limit (80.0) must be superior to Low short circuit current limit (100.0)"));
    }

}
