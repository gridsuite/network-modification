/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.formula;

import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.byfilter.equipmentfield.LoadField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createLoad;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LoadByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String LOAD_ID_1 = "load1";
    private static final String LOAD_ID_2 = "load2";
    private static final String LOAD_ID_3 = "load3";
    private static final String LOAD_ID_4 = "load4";

    @Override
    protected void createEquipments() {
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_1, "load1", 100, 100, 120, null, 5, null);
        createLoad(getNetwork().getVoltageLevel("v2"), LOAD_ID_2, "load2", 200, 80, 90, null, 5, null);
        createLoad(getNetwork().getVoltageLevel("v3"), LOAD_ID_3, "load3", 300, 100, 70, null, 5, null);
        createLoad(getNetwork().getVoltageLevel("v4"), LOAD_ID_4, "load4", 400, 50, 150, null, 5, null);
    }

    @Override
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
            new IdentifiableAttributes(LOAD_ID_1, getIdentifiableType(), 1.0),
            new IdentifiableAttributes(LOAD_ID_2, getIdentifiableType(), 2.0)))
            .build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(
            new IdentifiableAttributes(LOAD_ID_3, getIdentifiableType(), 2.0),
            new IdentifiableAttributes(LOAD_ID_4, getIdentifiableType(), 5.0)))
            .build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2);
    }

    @Override
    protected List<FormulaInfos> getFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(LoadField.ACTIVE_POWER.name(),
                List.of(filter1),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(LoadField.ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(25.).build()
        );

        FormulaInfos formulaInfos2 = getFormulaInfo(LoadField.REACTIVE_POWER.name(),
                List.of(filter2),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(LoadField.REACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(2.5).build()
        );
        return List.of(formulaInfos1, formulaInfos2);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.LOAD;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.LOAD;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(125, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(105, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(175, getNetwork().getLoad(LOAD_ID_3).getQ0(), 0);
        assertEquals(375, getNetwork().getLoad(LOAD_ID_4).getQ0(), 0);
    }
}
