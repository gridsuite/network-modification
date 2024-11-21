/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.LoadType;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.EnumAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.LoadField;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.utils.NetworkUtil.createLoad;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LoadModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
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
            new IdentifiableAttributes(LOAD_ID_1, IdentifiableType.LOAD, 1.0),
            new IdentifiableAttributes(LOAD_ID_2, IdentifiableType.LOAD, 2.0)))
            .build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(
            new IdentifiableAttributes(LOAD_ID_3, IdentifiableType.LOAD, 2.0),
            new IdentifiableAttributes(LOAD_ID_4, IdentifiableType.LOAD, 5.0)))
            .build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2);
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .editedField(LoadField.ACTIVE_POWER.name())
                .value(25.)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .editedField(LoadField.REACTIVE_POWER.name())
                .value(2.5)
                .filters(List.of(filter2))
                .build();

        EnumAssignmentInfos assignmentInfos3 = EnumAssignmentInfos.builder()
                .editedField(LoadField.LOAD_TYPE.name())
                .value(LoadType.AUXILIARY.name())
                .filters(List.of(filter1))
                .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3));

        return infosList;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(25, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(25, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(2.5, getNetwork().getLoad(LOAD_ID_3).getQ0(), 0);
        assertEquals(2.5, getNetwork().getLoad(LOAD_ID_4).getQ0(), 0);
        assertThat(getNetwork().getLoad(LOAD_ID_1).getLoadType()).isEqualTo(LoadType.AUXILIARY);
        assertThat(getNetwork().getLoad(LOAD_ID_2).getLoadType()).isEqualTo(LoadType.AUXILIARY);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.LOAD;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.LOAD;
    }

}
