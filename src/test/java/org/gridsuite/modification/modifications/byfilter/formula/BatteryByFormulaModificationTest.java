/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.formula;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.BatteryField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.modifications.data.assignment.Operator;
import org.gridsuite.modification.modifications.data.assignment.ReferenceFieldOrValue;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

import static org.gridsuite.modification.utils.NetworkUtil.createBattery;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class BatteryByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String BATTERY_ID_1 = "v3Battery";
    private static final String BATTERY_ID_2 = "battery2";
    private static final String BATTERY_ID_3 = "battery3";
    private static final String BATTERY_ID_4 = "battery4";
    private static final String BATTERY_ID_5 = "battery5";
    private static final String BATTERY_ID_6 = "battery6";

    @Test
    void testCreateWithWarning() {
        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filterWithOneWrongId))
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(55.).build())
                .operator(Operator.ADDITION)
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(20.).build())
                .build();

        ByFormulaModificationInfos modificationInfos = ByFormulaModificationInfos.builder()
                .identifiableType(getIdentifiableType())
                .formulaInfosList(List.of(formulaInfos))
                .stashed(false)
                .build();
        apply(modificationInfos, _ -> List.of(equipmentFilter(BATTERY_ID_1)));
        assertEquals(75, getNetwork().getBattery(BATTERY_ID_1).getTargetP(), 0);
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
    public List<Filter> loadFilters(List<UUID> filterUuids) {
        return filterUuids.stream().flatMap(filterUuid -> {
            if (filterUuid.equals(FILTER_ID_1)) {
                return Stream.of(equipmentFilter(BATTERY_ID_1), equipmentFilter(BATTERY_ID_2));
            } else if (filterUuid.equals(FILTER_ID_2)) {
                return Stream.of(equipmentFilter(BATTERY_ID_3), equipmentFilter(BATTERY_ID_4));
            } else if (filterUuid.equals(FILTER_ID_3)) {
                return Stream.of(equipmentFilter(BATTERY_ID_5), equipmentFilter(BATTERY_ID_6));
            } else if (filterUuid.equals(FILTER_ID_4)) {
                return Stream.of(equipmentFilter(BATTERY_ID_1), equipmentFilter(BATTERY_ID_5));
            } else if (filterUuid.equals(FILTER_ID_5)) {
                return Stream.of(equipmentFilter(BATTERY_ID_2), equipmentFilter(BATTERY_ID_3));
            } else {
                return Stream.empty();
            }
        }).toList();
    }

    private Filter equipmentFilter(String equipmentId) {
        return IdentifierListFilter.builder()
                .equipmentType(EquipmentType.BATTERY)
                .equipmentIds(Set.of(equipmentId))
                .build();
    }

    @Override
    protected List<FormulaInfos> getFormulaInfos() {
        ReferenceFieldOrValue maxActivePowerRef = ReferenceFieldOrValue.builder().equipmentField(BatteryField.MAXIMUM_ACTIVE_POWER.name()).build();
        ReferenceFieldOrValue minActivePowerRef = ReferenceFieldOrValue.builder().equipmentField(BatteryField.MINIMUM_ACTIVE_POWER.name()).build();

        FormulaInfos formulaInfos1 = getFormulaInfo(BatteryField.MAXIMUM_ACTIVE_POWER.name(),
                List.of(filter1, filter2),
                Operator.ADDITION,
                maxActivePowerRef,
                ReferenceFieldOrValue.builder().value(50.).build());

        FormulaInfos formulaInfos2 = getFormulaInfo(BatteryField.MINIMUM_ACTIVE_POWER.name(),
                List.of(filter3),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(30.).build(),
                minActivePowerRef);

        FormulaInfos formulaInfos3 = getFormulaInfo(BatteryField.ACTIVE_POWER_SET_POINT.name(),
                List.of(filter5),
                Operator.SUBTRACTION,
                maxActivePowerRef,
                minActivePowerRef);

        FormulaInfos formulaInfos4 = getFormulaInfo(BatteryField.REACTIVE_POWER_SET_POINT.name(),
                List.of(filter4),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(BatteryField.REACTIVE_POWER_SET_POINT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos5 = getFormulaInfo(BatteryField.DROOP.name(),
                List.of(filter4),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(BatteryField.DROOP.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3, formulaInfos4, formulaInfos5);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(550, getNetwork().getBattery(BATTERY_ID_1).getMaxP(), 0);
        assertEquals(40, getNetwork().getBattery(BATTERY_ID_1).getTargetQ(), 0);
        ActivePowerControl activePowerControl1 = getNetwork().getBattery(BATTERY_ID_1).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl1);
        assertEquals(2, activePowerControl1.getDroop(), 0);

        assertEquals(2050, getNetwork().getBattery(BATTERY_ID_2).getMaxP(), 0);
        assertEquals(2000, getNetwork().getBattery(BATTERY_ID_2).getTargetP(), 0);
        assertEquals(450, getNetwork().getBattery(BATTERY_ID_3).getMaxP(), 0);
        assertEquals(380, getNetwork().getBattery(BATTERY_ID_3).getTargetP(), 0);
        assertEquals(400, getNetwork().getBattery(BATTERY_ID_4).getMaxP(), 0);

        assertEquals(15, getNetwork().getBattery(BATTERY_ID_5).getMinP(), 0);
        assertEquals(70, getNetwork().getBattery(BATTERY_ID_5).getTargetQ(), 0);
        ActivePowerControl activePowerControl5 = getNetwork().getBattery(BATTERY_ID_5).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(8, activePowerControl5.getDroop(), 0);

        assertEquals(60, getNetwork().getBattery(BATTERY_ID_6).getMinP(), 0);
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
