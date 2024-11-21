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
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.byfilter.equipmentfield.BatteryField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createBattery;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

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
    void testCreateWithWarning() throws Exception {
        IdentifiableAttributes identifiableAttributes = new IdentifiableAttributes(BATTERY_ID_1, getIdentifiableType(), 1.0);
        FilterEquipments filter = FilterEquipments.builder().filterId(FILTER_WITH_ONE_WRONG_ID)
                .identifiableAttributes(List.of(identifiableAttributes))
                .notFoundEquipments(List.of("wrongId"))
                .build();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(Map.of(FILTER_WITH_ONE_WRONG_ID, filter));
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
        apply(modificationInfos);
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
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
            new IdentifiableAttributes(BATTERY_ID_1, getIdentifiableType(), 1.0),
            new IdentifiableAttributes(BATTERY_ID_2, getIdentifiableType(), 2.0)
        )).build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(
            new IdentifiableAttributes(BATTERY_ID_3, getIdentifiableType(), 2.0),
            new IdentifiableAttributes(BATTERY_ID_4, getIdentifiableType(), 5.0)
        )).build();

        FilterEquipments filter3 = FilterEquipments.builder().filterId(FILTER_ID_3).identifiableAttributes(List.of(
            new IdentifiableAttributes(BATTERY_ID_5, getIdentifiableType(), 6.0),
            new IdentifiableAttributes(BATTERY_ID_6, getIdentifiableType(), 7.0)
        )).build();

        FilterEquipments filter4 = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(
            new IdentifiableAttributes(BATTERY_ID_1, getIdentifiableType(), 1.0),
            new IdentifiableAttributes(BATTERY_ID_5, getIdentifiableType(), 6.0)
        )).build();

        FilterEquipments filter5 = FilterEquipments.builder().filterId(FILTER_ID_5).identifiableAttributes(List.of(
            new IdentifiableAttributes(BATTERY_ID_2, getIdentifiableType(), 2.0),
            new IdentifiableAttributes(BATTERY_ID_3, getIdentifiableType(), 3.0)
        )).build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2, FILTER_ID_3, filter3, FILTER_ID_4, filter4, FILTER_ID_5, filter5);
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
