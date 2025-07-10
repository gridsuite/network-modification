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
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.IntegerAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.StringAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.LineField;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createLine;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

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
        IdentifiableAttributes identifiableAttributes1 = new IdentifiableAttributes(LINE_ID_1, getIdentifiableType(), 1.);
        IdentifiableAttributes identifiableAttributes2 = new IdentifiableAttributes(LINE_ID_2, getIdentifiableType(), 1.);
        IdentifiableAttributes identifiableAttributes3 = new IdentifiableAttributes(LINE_ID_4, getIdentifiableType(), 1.);
        IdentifiableAttributes identifiableAttributes4 = new IdentifiableAttributes(LINE_ID_6, getIdentifiableType(), 1.);
        FilterEquipments filterLine1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(identifiableAttributes1, identifiableAttributes2)).build();
        FilterEquipments filterLine2 = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(identifiableAttributes3, identifiableAttributes4)).build();

        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(Map.of(FILTER_ID_1, filterLine1, FILTER_ID_4, filterLine2));

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

        apply(modificationInfos);

        assertEquals(4, getNetwork().getLine(LINE_ID_1).getR());
        assertEquals(4, getNetwork().getLine(LINE_ID_2).getR());
    }

    @Override
    protected void createEquipments() {
        createLine(getNetwork(), LINE_ID_1, LINE_ID_1, "v1", "v2", 21, 21, 2,
            1, 3, 4, 0.001, 0.0015,
                "line_1", 11, ConnectablePosition.Direction.TOP,
                "line_1", 22, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_2, LINE_ID_2, "v1", "v2", 33, 44, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_2", 33, ConnectablePosition.Direction.TOP,
            "line_2", 44, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_3, LINE_ID_3, "v2", "v4", 33, 44, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_3", 10, ConnectablePosition.Direction.TOP,
            "line_3", 20, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_4, LINE_ID_4, "v2", "v4", 35, 45, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_4", 11, ConnectablePosition.Direction.TOP,
            "line_4", 21, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_5, LINE_ID_5, "v2", "v4", 45, 55, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_5", 12, ConnectablePosition.Direction.TOP,
            "line_5", 22, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_6, LINE_ID_6, "v2", "v4", 55, 65, 3,
            3, 5, 1, 0.002, 0.0025,
            "line_6", 13, ConnectablePosition.Direction.TOP,
            "line_6", 23, ConnectablePosition.Direction.BOTTOM);
    }

    @Override
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
            new IdentifiableAttributes(LINE_ID_1, IdentifiableType.LINE, 1.0),
            new IdentifiableAttributes(LINE_ID_2, IdentifiableType.LINE, 2.0)
        )).build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(
            new IdentifiableAttributes(LINE_ID_1, IdentifiableType.LINE, 1.0),
            new IdentifiableAttributes(LINE_ID_3, IdentifiableType.LINE, 2.0)
        )).build();

        FilterEquipments filter3 = FilterEquipments.builder().filterId(FILTER_ID_3).identifiableAttributes(List.of(
            new IdentifiableAttributes(LINE_ID_4, IdentifiableType.LINE, 5.0),
            new IdentifiableAttributes(LINE_ID_5, IdentifiableType.LINE, 6.0)
        )).build();

        FilterEquipments filter4 = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(
            new IdentifiableAttributes(LINE_ID_4, IdentifiableType.LINE, 5.0),
            new IdentifiableAttributes(LINE_ID_6, IdentifiableType.LINE, 7.0)
        )).build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2, FILTER_ID_3, filter3, FILTER_ID_4, filter4);
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
            .editedField(LineField.SELECTED_OPERATIONAL_LIMITS_GROUP_1.name())
            .value("group1")
            .build();

        StringAssignmentInfos assignmentInfos8 = StringAssignmentInfos.builder()
            .filters(List.of(filter2))
            .editedField(LineField.SELECTED_OPERATIONAL_LIMITS_GROUP_2.name())
            .value("group2")
            .build();

        StringAssignmentInfos assignmentInfos9 = StringAssignmentInfos.builder()
            .filters(List.of(filter3))
            .editedField(LineField.SELECTED_OPERATIONAL_LIMITS_GROUP_1.name())
            .value(null)
            .build();

        StringAssignmentInfos assignmentInfos10 = StringAssignmentInfos.builder()
            .filters(List.of(filter3))
            .editedField(LineField.SELECTED_OPERATIONAL_LIMITS_GROUP_2.name())
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
