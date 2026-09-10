/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.assignment;

import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.BatteryShortCircuit;
import com.powsybl.iidm.network.extensions.VoltageRegulation;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.BooleanAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.BatteryField;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createBattery;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class BatteryModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
    private static final String BATTERY_ID_1 = "v3Battery";
    private static final String BATTERY_ID_2 = "battery2";
    private static final String BATTERY_ID_3 = "battery3";
    private static final String BATTERY_ID_4 = "battery4";
    private static final String BATTERY_ID_5 = "battery5";
    private static final String BATTERY_ID_6 = "battery6";
    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(BATTERY_ID_1, BATTERY_ID_2),
            FILTER_ID_2, Set.of(BATTERY_ID_3, BATTERY_ID_4),
            FILTER_ID_3, Set.of(BATTERY_ID_5, BATTERY_ID_6),
            FILTER_ID_4, Set.of(BATTERY_ID_1, BATTERY_ID_5),
            FILTER_ID_5, Set.of(BATTERY_ID_2, BATTERY_ID_3)
    );

    @Override
    public Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
    }

    @Override
    protected void createEquipments() {
        getNetwork().getBattery(BATTERY_ID_1).setTargetP(100).setMaxP(500).setMinP(0).setTargetQ(80);
        getNetwork().getBattery(BATTERY_ID_1).newExtension(ActivePowerControlAdder.class).withDroop(1).add();

        createBattery(getNetwork().getVoltageLevel("v2"), BATTERY_ID_2, "v2Battery2", 20, 50, 2000, 200, 50);
        createBattery(getNetwork().getVoltageLevel("v3"), BATTERY_ID_3, "v3Battery3", 30, 70, 400, 300, 50);

        createBattery(getNetwork().getVoltageLevel("v4"), BATTERY_ID_4, "v4Battery4", 40, 25, 350, 70, 50);

        createBattery(getNetwork().getVoltageLevel("v5"), BATTERY_ID_5, "v5Battery5", 50, 50, 600, 55, 140);
        getNetwork().getBattery(BATTERY_ID_5).newExtension(ActivePowerControlAdder.class).withDroop(4).add();

        createBattery(getNetwork().getVoltageLevel("v6"), BATTERY_ID_6, "v6Battery6", 60, 200, 700, 250, 210);
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1, filter2))
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .value(80.)
                .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter3))
                .editedField(BatteryField.MINIMUM_ACTIVE_POWER.name())
                .value(30.)
                .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter5))
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .value(75.)
                .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(BatteryField.REACTIVE_POWER_SET_POINT.name())
                .value(2.)
                .build();

        DoubleAssignmentInfos assignmentInfos5 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(BatteryField.DROOP.name())
                .value(2.)
                .build();

        DoubleAssignmentInfos assignmentInfos6 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(BatteryField.TRANSIENT_REACTANCE.name())
                .value(3.)
                .build();

        DoubleAssignmentInfos assignmentInfos7 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(BatteryField.STEP_UP_TRANSFORMER_REACTANCE.name())
                .value(4.)
                .build();

        DoubleAssignmentInfos assignmentInfos8 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(BatteryField.VOLTAGE_SET_POINT.name())
                .value(400.)
                .build();

        BooleanAssignmentInfos assignmentInfos9 = BooleanAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(BatteryField.VOLTAGE_REGULATOR_ON.name())
                .value(true)
                .build();

        BooleanAssignmentInfos assignmentInfos10 = BooleanAssignmentInfos.builder()
                .filters(List.of(filter2))
                .editedField(BatteryField.VOLTAGE_REGULATOR_ON.name())
                .value(null)
                .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3, assignmentInfos4,
                assignmentInfos5, assignmentInfos6, assignmentInfos7, assignmentInfos8, assignmentInfos9, assignmentInfos10));

        return infosList;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(80, getNetwork().getBattery(BATTERY_ID_1).getMaxP(), 0);
        assertEquals(2, getNetwork().getBattery(BATTERY_ID_1).getTargetQ(), 0);
        ActivePowerControl<Battery> activePowerControl1 = getNetwork().getBattery(BATTERY_ID_1).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl1);
        assertEquals(2, activePowerControl1.getDroop(), 0);

        assertEquals(80, getNetwork().getBattery(BATTERY_ID_2).getMaxP(), 0);
        assertEquals(75, getNetwork().getBattery(BATTERY_ID_2).getTargetP(), 0);
        assertEquals(80, getNetwork().getBattery(BATTERY_ID_3).getMaxP(), 0);
        assertEquals(75, getNetwork().getBattery(BATTERY_ID_3).getTargetP(), 0);
        assertEquals(80, getNetwork().getBattery(BATTERY_ID_4).getMaxP(), 0);

        assertEquals(30, getNetwork().getBattery(BATTERY_ID_5).getMinP(), 0);
        assertEquals(2, getNetwork().getBattery(BATTERY_ID_5).getTargetQ(), 0);
        ActivePowerControl<Battery> activePowerControl5 = getNetwork().getBattery(BATTERY_ID_5).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(2, activePowerControl5.getDroop(), 0);

        assertEquals(30, getNetwork().getBattery(BATTERY_ID_6).getMinP(), 0);

        assertNotNull(getNetwork().getBattery(BATTERY_ID_5).getExtension(BatteryShortCircuit.class));
        BatteryShortCircuit batteryShortCircuit5 = getNetwork().getBattery(BATTERY_ID_5).getExtension(BatteryShortCircuit.class);
        assertEquals(3, batteryShortCircuit5.getDirectTransX());
        assertEquals(4, batteryShortCircuit5.getStepUpTransformerX());

        assertNotNull(getNetwork().getBattery(BATTERY_ID_1).getExtension(BatteryShortCircuit.class));
        BatteryShortCircuit batteryShortCircuit1 = getNetwork().getBattery(BATTERY_ID_5).getExtension(BatteryShortCircuit.class);
        assertEquals(3, batteryShortCircuit1.getDirectTransX());
        assertEquals(4, batteryShortCircuit1.getStepUpTransformerX());

        VoltageRegulation voltageRegulation = getNetwork().getBattery(BATTERY_ID_1).getExtension(VoltageRegulation.class);
        assertNotNull(voltageRegulation);
        assertTrue(voltageRegulation.isVoltageRegulatorOn());
        assertEquals(400.0, voltageRegulation.getTargetV());

        VoltageRegulation voltageRegulation2 = getNetwork().getBattery(BATTERY_ID_5).getExtension(VoltageRegulation.class);
        assertNotNull(voltageRegulation2);
        assertTrue(voltageRegulation2.isVoltageRegulatorOn());
        assertEquals(400.0, voltageRegulation2.getTargetV());

        VoltageRegulation voltageRegulation3 = getNetwork().getBattery(BATTERY_ID_2).getExtension(VoltageRegulation.class);
        assertNull(voltageRegulation3);
        VoltageRegulation voltageRegulation4 = getNetwork().getBattery(BATTERY_ID_3).getExtension(VoltageRegulation.class);
        assertNull(voltageRegulation4);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.BATTERY;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.BATTERY;
    }
}
