/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.IntegerAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.StringAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.LineField;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

import static org.gridsuite.modification.utils.NetworkUtil.createLineWithLimits;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Etienne LESOT <etienne.lesot at rte-france.com>
 */
class LineModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
    private static final String LINE_ID_1 = "line_1";
    private static final String LINE_ID_2 = "line_2";
    private static final String LINE_ID_3 = "line_3";
    private static final String LINE_ID_4 = "line_4";
    private static final String LINE_ID_5 = "line_5";
    private static final String LINE_ID_6 = "line_6";

    @Test
    void testModifyLineWithWarning() {
        IntegerAssignmentInfos assignmentInfos = IntegerAssignmentInfos.builder()
                .filters(List.of(filter1, filter4))
                .editedField(LineField.R.name())
                .value(4)
                .build();

        ModificationByAssignmentInfos modificationInfos = ModificationByAssignmentInfos.builder()
                .equipmentType(getIdentifiableType())
                .assignmentInfosList(List.of(assignmentInfos))
                .stashed(false)
                .build();

        apply(modificationInfos, _ -> List.of(IdentifierListFilter.builder().equipmentType(EquipmentType.LINE).equipmentIds(Set.of(LINE_ID_1, LINE_ID_2, LINE_ID_4, LINE_ID_6)).build()));

        assertEquals(4, getNetwork().getLine(LINE_ID_1).getR());
        assertEquals(4, getNetwork().getLine(LINE_ID_2).getR());
    }

    @Override
    protected void createEquipments() {
        createLineWithLimits(getNetwork(), LINE_ID_1, LINE_ID_1, "v1", "v2", 21, 21, 2,
            1, 3, 4, 0.001, 0.0015,
                "line_1", 11, ConnectablePosition.Direction.TOP,
                "line_1", 22, ConnectablePosition.Direction.BOTTOM);

        createLineWithLimits(getNetwork(), LINE_ID_2, LINE_ID_2, "v1", "v2", 33, 44, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_2", 33, ConnectablePosition.Direction.TOP,
            "line_2", 44, ConnectablePosition.Direction.BOTTOM);

        createLineWithLimits(getNetwork(), LINE_ID_3, LINE_ID_3, "v2", "v4", 33, 44, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_3", 10, ConnectablePosition.Direction.TOP,
            "line_3", 20, ConnectablePosition.Direction.BOTTOM);

        createLineWithLimits(getNetwork(), LINE_ID_4, LINE_ID_4, "v2", "v4", 35, 45, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_4", 11, ConnectablePosition.Direction.TOP,
            "line_4", 21, ConnectablePosition.Direction.BOTTOM);

        createLineWithLimits(getNetwork(), LINE_ID_5, LINE_ID_5, "v2", "v4", 45, 55, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_5", 12, ConnectablePosition.Direction.TOP,
            "line_5", 22, ConnectablePosition.Direction.BOTTOM);

        createLineWithLimits(getNetwork(), LINE_ID_6, LINE_ID_6, "v2", "v4", 55, 65, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_6", 13, ConnectablePosition.Direction.TOP,
            "line_6", 23, ConnectablePosition.Direction.BOTTOM);
    }

    @Override
    public List<Filter> loadFilters(List<UUID> filterUuids) {
        return filterUuids.stream().flatMap(filterUuid -> {
            if (filterUuid.equals(FILTER_ID_1)) {
                return Stream.of(equipmentFilter(LINE_ID_1), equipmentFilter(LINE_ID_2));
            } else if (filterUuid.equals(FILTER_ID_2)) {
                return Stream.of(equipmentFilter(LINE_ID_1), equipmentFilter(LINE_ID_3));
            } else if (filterUuid.equals(FILTER_ID_3)) {
                return Stream.of(equipmentFilter(LINE_ID_4), equipmentFilter(LINE_ID_5));
            } else if (filterUuid.equals(FILTER_ID_4)) {
                return Stream.of(equipmentFilter(LINE_ID_4), equipmentFilter(LINE_ID_6));
            } else {
                return Stream.empty();
            }
        }).toList();
    }

    private Filter equipmentFilter(String equipmentId) {
        return IdentifierListFilter.builder()
                .equipmentType(EquipmentType.LINE)
                .equipmentIds(Set.of(equipmentId))
                .build();
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1))
                .editedField(LineField.X.name())
                .value(20.)
                .build();

        IntegerAssignmentInfos assignmentInfos2 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter2))
                .editedField(LineField.R.name())
                .value(40)
                .build();

        IntegerAssignmentInfos assignmentInfos3 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter2))
                .editedField(LineField.G1.name())
                .value(35)
                .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1))
                .editedField(LineField.G2.name())
                .value(10.)
                .build();

        DoubleAssignmentInfos assignmentInfos5 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(LineField.B1.name())
                .value(21.)
                .build();

        IntegerAssignmentInfos assignmentInfos6 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter3))
                .editedField(LineField.B2.name())
                .value(90)
                .build();

        StringAssignmentInfos assignmentInfos7 = StringAssignmentInfos.builder()
            .filters(List.of(filter1))
            .editedField(LineField.SELECTED_OPERATIONAL_LIMITS_GROUP_ID1.name())
            .value("group1")
            .build();

        StringAssignmentInfos assignmentInfos8 = StringAssignmentInfos.builder()
            .filters(List.of(filter2))
            .editedField(LineField.SELECTED_OPERATIONAL_LIMITS_GROUP_ID2.name())
            .value("group2")
            .build();

        StringAssignmentInfos assignmentInfos9 = StringAssignmentInfos.builder()
            .filters(List.of(filter3))
            .editedField(LineField.SELECTED_OPERATIONAL_LIMITS_GROUP_ID1.name())
            .value(null)
            .build();

        StringAssignmentInfos assignmentInfos10 = StringAssignmentInfos.builder()
            .filters(List.of(filter3))
            .editedField(LineField.SELECTED_OPERATIONAL_LIMITS_GROUP_ID2.name())
            .value("")
            .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1,
                assignmentInfos2,
                assignmentInfos3,
                assignmentInfos4,
                assignmentInfos5,
                assignmentInfos6,
                assignmentInfos7,
                assignmentInfos8,
                assignmentInfos9,
                assignmentInfos10));

        return infosList;
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.LINE;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.LINE;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Line line1 = getNetwork().getLine(LINE_ID_1);
        assertEquals(40, line1.getR(), 0);
        assertEquals(20, line1.getX(), 0);
        assertEquals(0.001, line1.getB1(), 0);
        assertEquals(0.0015, line1.getB2(), 0);
        assertEquals(35, line1.getG1(), 0);
        assertEquals(10, line1.getG2(), 0);
        assertTrue(line1.getSelectedOperationalLimitsGroupId1().isPresent());
        assertEquals("group1", line1.getSelectedOperationalLimitsGroupId1().get());
        assertTrue(line1.getSelectedOperationalLimitsGroupId2().isPresent());
        assertEquals("group2", line1.getSelectedOperationalLimitsGroupId2().get());

        Line line2 = getNetwork().getLine(LINE_ID_2);
        assertEquals(3, line2.getR(), 0);
        assertEquals(20, line2.getX(), 0);
        assertEquals(0.002, line2.getB1(), 0);
        assertEquals(0.0025, line2.getB2(), 0);
        assertEquals(5, line2.getG1(), 0);
        assertEquals(10, line2.getG2(), 0);
        assertTrue(line2.getSelectedOperationalLimitsGroupId1().isPresent());
        assertEquals("group1", line2.getSelectedOperationalLimitsGroupId1().get());
        assertTrue(line2.getSelectedOperationalLimitsGroupId2().isPresent());
        assertEquals("group0", line2.getSelectedOperationalLimitsGroupId2().get());

        Line line3 = getNetwork().getLine(LINE_ID_3);
        assertEquals(40, line3.getR(), 0);
        assertEquals(3, line3.getX(), 0);
        assertEquals(0.002, line3.getB1(), 0);
        assertEquals(0.0025, line3.getB2(), 0);
        assertEquals(35, line3.getG1(), 0);
        assertEquals(1, line3.getG2(), 0);
        assertTrue(line3.getSelectedOperationalLimitsGroupId1().isPresent());
        assertEquals("group0", line3.getSelectedOperationalLimitsGroupId1().get());
        assertTrue(line3.getSelectedOperationalLimitsGroupId2().isPresent());
        assertEquals("group2", line3.getSelectedOperationalLimitsGroupId2().get());

        Line line4 = getNetwork().getLine(LINE_ID_4);
        assertEquals(3, line4.getR(), 0);
        assertEquals(3, line4.getX(), 0);
        assertEquals(21, line4.getB1(), 0);
        assertEquals(90, line4.getB2(), 0);
        assertEquals(5, line4.getG1(), 0);
        assertEquals(1, line4.getG2(), 0);
        assertFalse(line4.getSelectedOperationalLimitsGroupId1().isPresent());
        assertFalse(line4.getSelectedOperationalLimitsGroupId2().isPresent());

        Line line5 = getNetwork().getLine(LINE_ID_5);
        assertEquals(3, line5.getR(), 0);
        assertEquals(3, line5.getX(), 0);
        assertEquals(0.002, line5.getB1(), 0);
        assertEquals(90, line5.getB2(), 0);
        assertEquals(5, line5.getG1(), 0);
        assertEquals(1, line5.getG2(), 0);
        assertFalse(line5.getSelectedOperationalLimitsGroupId1().isPresent());
        assertFalse(line5.getSelectedOperationalLimitsGroupId2().isPresent());

        Line line6 = getNetwork().getLine(LINE_ID_6);
        assertEquals(3, line6.getR(), 0);
        assertEquals(3, line6.getX(), 0);
        assertEquals(21, line6.getB1(), 0);
        assertEquals(0.0025, line6.getB2(), 0);
        assertEquals(5, line6.getG1(), 0);
        assertEquals(1, line6.getG2(), 0);
    }

}
