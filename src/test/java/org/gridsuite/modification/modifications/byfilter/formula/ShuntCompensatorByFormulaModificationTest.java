/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.formula;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.byfilter.equipmentfield.ShuntCompensatorField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createShuntCompensator;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class ShuntCompensatorByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String SHUNT_COMPENSATOR_ID_1 = "v1shunt";
    private static final String SHUNT_COMPENSATOR_ID_2 = "v2shunt";
    private static final String SHUNT_COMPENSATOR_ID_3 = "v3shunt";
    private static final String SHUNT_COMPENSATOR_ID_4 = "v4shunt";
    private static final String SHUNT_COMPENSATOR_ID_5 = "v5shunt";
    private static final String SHUNT_COMPENSATOR_ID_6 = "v6shunt";

    @Test
    void testCreateWithWarning() throws Exception {
        IdentifiableAttributes identifiableAttributes = new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_1, getIdentifiableType(), 1.0);
        FilterEquipments filter = FilterEquipments.builder().filterId(FILTER_WITH_ONE_WRONG_ID)
                .identifiableAttributes(List.of(identifiableAttributes))
                .notFoundEquipments(List.of("wrongId"))
                .build();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(Map.of(FILTER_WITH_ONE_WRONG_ID, filter));

        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filterWithOneWrongId))
                .editedField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(2.).build())
                .operator(Operator.ADDITION)
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(3.).build())
                .build();

        ByFormulaModificationInfos modificationInfos = ByFormulaModificationInfos.builder()
                .identifiableType(getIdentifiableType())
                .formulaInfosList(List.of(formulaInfos))
                .stashed(false)
                .build();
        apply(modificationInfos);
        assertEquals(5, getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_1).getMaximumSectionCount(), 0);
    }

    @Override
    protected void createEquipments() {
        createShuntCompensator(getNetwork().getVoltageLevel("v1"), SHUNT_COMPENSATOR_ID_1, "v1shunt", 8, 225., 10, true, 4, 2, 3, 2, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
        createShuntCompensator(getNetwork().getVoltageLevel("v3"), SHUNT_COMPENSATOR_ID_3, "v3shunt", 10, 305., 20, true, 6, 3, 3, 4, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
        createShuntCompensator(getNetwork().getVoltageLevel("v4"), SHUNT_COMPENSATOR_ID_4, "v3shunt", 10, 305., 20, true, 15, 4, 3, 10, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
    }

    @Override
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_1, getIdentifiableType(), 1.0),
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_2, getIdentifiableType(), 2.0)))
            .build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_3, getIdentifiableType(), 2.0),
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_6, getIdentifiableType(), 7.0)))
            .build();

        FilterEquipments filter3 = FilterEquipments.builder().filterId(FILTER_ID_3).identifiableAttributes(List.of(
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_4, getIdentifiableType(), 5.0),
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_5, getIdentifiableType(), 6.0)))
            .build();

        FilterEquipments filter4 = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_1, getIdentifiableType(), 1.0),
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_5, getIdentifiableType(), 6.0)))
            .build();

        FilterEquipments filter5 = FilterEquipments.builder().filterId(FILTER_ID_5).identifiableAttributes(List.of(
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_3, getIdentifiableType(), 2.0),
            new IdentifiableAttributes(SHUNT_COMPENSATOR_ID_2, getIdentifiableType(), 2.0)))
            .build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2, FILTER_ID_3, filter3, FILTER_ID_4, filter4, FILTER_ID_5, filter5);
    }

    @Override
    protected List<FormulaInfos> getFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name(),
                List.of(filter1, filter2),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(200.).build(),
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name()).build());

        FormulaInfos formulaInfos2 = getFormulaInfo(ShuntCompensatorField.SECTION_COUNT.name(),
                List.of(filter3),
                Operator.SUBTRACTION,
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos3 = getFormulaInfo(ShuntCompensatorField.MAXIMUM_SUSCEPTANCE.name(),
                List.of(filter4),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.MAXIMUM_SUSCEPTANCE.name()).build(),
                ReferenceFieldOrValue.builder().value(5.).build());

        FormulaInfos formulaInfos4 = getFormulaInfo(ShuntCompensatorField.MAXIMUM_Q_AT_NOMINAL_VOLTAGE.name(),
                List.of(filter5),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.MAXIMUM_Q_AT_NOMINAL_VOLTAGE.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3, formulaInfos4);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        ShuntCompensator shuntCompensator1 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_1);
        assertEquals(8, shuntCompensator1.getMaximumSectionCount());
        assertEquals(1.625, shuntCompensator1.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator2 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_2);
        assertEquals(6, shuntCompensator2.getMaximumSectionCount());
        assertEquals(0.25, shuntCompensator2.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator3 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_3);
        assertEquals(12, shuntCompensator3.getMaximumSectionCount());
        assertEquals(0.75, shuntCompensator3.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator4 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_4);
        assertEquals(13, shuntCompensator4.getSectionCount());

        ShuntCompensator shuntCompensator5 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_5);
        assertEquals(1, shuntCompensator5.getSectionCount());

        ShuntCompensator shuntCompensator6 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_6);
        assertEquals(6, shuntCompensator6.getMaximumSectionCount());
        assertEquals(0.5, shuntCompensator6.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.SHUNT_COMPENSATOR;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.SHUNT_COMPENSATOR;
    }
}
