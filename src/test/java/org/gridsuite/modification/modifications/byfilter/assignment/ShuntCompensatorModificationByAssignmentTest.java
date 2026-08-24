/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.IntegerAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.ShuntCompensatorField;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

import static org.gridsuite.modification.utils.NetworkUtil.createShuntCompensator;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class ShuntCompensatorModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
    private static final String SHUNT_COMPENSATOR_ID_1 = "v1shunt";
    private static final String SHUNT_COMPENSATOR_ID_2 = "v2shunt";
    private static final String SHUNT_COMPENSATOR_ID_3 = "v3shunt";
    private static final String SHUNT_COMPENSATOR_ID_4 = "v4shunt";
    private static final String SHUNT_COMPENSATOR_ID_5 = "v5shunt";

    @Test
    void testCreateWithWarning() {
        IntegerAssignmentInfos assignmentInfos = IntegerAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name())
                .value(2)
                .filters(List.of(filterWithOneWrongId))
                .build();

        ModificationByAssignmentInfos modificationInfos = ModificationByAssignmentInfos.builder()
                .equipmentType(getIdentifiableType())
                .assignmentInfosList(List.of(assignmentInfos))
                .stashed(false)
                .build();
        apply(modificationInfos, _ -> List.of(equipmentFilter(SHUNT_COMPENSATOR_ID_1)));
        assertEquals(2, getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_1).getMaximumSectionCount(), 0);
    }

    @Override
    protected void createEquipments() {
        createShuntCompensator(getNetwork().getVoltageLevel("v1"), SHUNT_COMPENSATOR_ID_1, "v1shunt", 8, 225., 10, true, 4, 2, 3, 2, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
        createShuntCompensator(getNetwork().getVoltageLevel("v3"), SHUNT_COMPENSATOR_ID_3, "v3shunt", 10, 305., 20, true, 6, 3, 3, 4, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
        createShuntCompensator(getNetwork().getVoltageLevel("v4"), SHUNT_COMPENSATOR_ID_4, "v3shunt", 10, 305., 20, true, 15, 4, 3, 10, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
    }

    @Override
    public List<Filter> loadFilters(List<UUID> filterUuids) {
        return filterUuids.stream().flatMap(filterUuid -> {
            if (filterUuid.equals(FILTER_ID_1)) {
                return Stream.of(equipmentFilter(SHUNT_COMPENSATOR_ID_1));
            } else if (filterUuid.equals(FILTER_ID_2)) {
                return Stream.of(equipmentFilter(SHUNT_COMPENSATOR_ID_2));
            } else if (filterUuid.equals(FILTER_ID_3)) {
                return Stream.of(equipmentFilter(SHUNT_COMPENSATOR_ID_3));
            } else if (filterUuid.equals(FILTER_ID_4)) {
                return Stream.of(equipmentFilter(SHUNT_COMPENSATOR_ID_4));
            } else if (filterUuid.equals(FILTER_ID_5)) {
                return Stream.of(equipmentFilter(SHUNT_COMPENSATOR_ID_5));
            } else {
                return Stream.empty();
            }
        }).toList();
    }

    private Filter equipmentFilter(String equipmentId) {
        return IdentifierListFilter.builder()
                .equipmentType(EquipmentType.SHUNT_COMPENSATOR)
                .equipmentIds(Set.of(equipmentId))
                .build();
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        IntegerAssignmentInfos assignmentInfos1 = IntegerAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name())
                .value(8)
                .filters(List.of(filter1, filter2))
                .build();

        IntegerAssignmentInfos assignmentInfos2 = IntegerAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.SECTION_COUNT.name())
                .value(2)
                .filters(List.of(filter3))
                .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.MAX_SUSCEPTANCE.name())
                .value(5.)
                .filters(List.of(filter4))
                .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.MAX_Q_AT_NOMINAL_V.name())
                .value(10.)
                .filters(List.of(filter5))
                .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3, assignmentInfos4));

        return infosList;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        ShuntCompensator shuntCompensator1 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_1);
        assertEquals(8, shuntCompensator1.getMaximumSectionCount());
        assertEquals(1, shuntCompensator1.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator2 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_2);
        assertEquals(8, shuntCompensator2.getMaximumSectionCount());
        assertEquals(0.375, shuntCompensator2.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator3 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_3);
        assertEquals(6, shuntCompensator3.getMaximumSectionCount());
        assertEquals(3, shuntCompensator3.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator4 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_4);
        assertEquals(10, shuntCompensator4.getSectionCount());

        ShuntCompensator shuntCompensator5 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_5);
        assertEquals(2, shuntCompensator5.getSectionCount());
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
