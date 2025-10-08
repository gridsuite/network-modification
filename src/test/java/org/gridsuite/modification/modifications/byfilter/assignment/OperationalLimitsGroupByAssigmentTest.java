/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.PropertyAssignmentInfos;
import org.junit.jupiter.api.Assertions;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField.OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES;
import static org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField.OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES;
import static org.gridsuite.modification.utils.NetworkUtil.*;

/**
 * @author Etienne LESOT <etienne.lesot at rte-france.com>
 */
public class OperationalLimitsGroupByAssigmentTest extends AbstractModificationByAssignmentTest {

    private static final String LINE_ID_1 = "line_1";
    private static final String LINE_ID_2 = "line_2";
    private static final String LINE_ID_3 = "line_3";
    private static final String LINE_ID_4 = "line_4";
    private static final String LINE_ID_5 = "line_5";

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

        createLineWithLimits(getNetwork(), LINE_ID_3, LINE_ID_3, "v1", "v2", 45, 56, 3,
                3, 5, 1, 0.002, 0.0025,
                "line_3", 45, ConnectablePosition.Direction.TOP,
                "line_3", 56, ConnectablePosition.Direction.BOTTOM);

        createLine(getNetwork(), LINE_ID_4, LINE_ID_4, "v1", "v2", 46, 57, 3,
                3, 5, 1, 0.002, 0.0025,
                "line_3", 46, ConnectablePosition.Direction.TOP,
                "line_3", 57, ConnectablePosition.Direction.BOTTOM);
        addLimitsWithIdenticalProperties(getNetwork().getLine(LINE_ID_4));
        createLine(getNetwork(), LINE_ID_5, LINE_ID_5, "v1", "v2", 47, 58, 3,
                3, 5, 1, 0.002, 0.0025,
                "line_3", 45, ConnectablePosition.Direction.TOP,
                "line_3", 58, ConnectablePosition.Direction.BOTTOM);
        addLimitsWithoutProperties(getNetwork().getLine(LINE_ID_5));
    }

    @Override
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
                new IdentifiableAttributes(LINE_ID_1, IdentifiableType.LINE, 1.0),
                new IdentifiableAttributes(LINE_ID_2, IdentifiableType.LINE, 2.0)
        )).build();
        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(
                new IdentifiableAttributes(LINE_ID_3, IdentifiableType.LINE, 1.0)
        )).build();
        FilterEquipments filter3 = FilterEquipments.builder().filterId(FILTER_ID_3).identifiableAttributes(List.of(
                new IdentifiableAttributes(LINE_ID_4, IdentifiableType.LINE, 1.0),
                new IdentifiableAttributes(LINE_ID_5, IdentifiableType.LINE, 2.0)
        )).build();
        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2, FILTER_ID_3, filter3);
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        PropertyAssignmentInfos assignmentInfos1 = PropertyAssignmentInfos.builder()
                .filters(List.of(filter1))
                .editedField(OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES.name())
                .propertyName("property1")
                .value("value1")
                .build();
        PropertyAssignmentInfos assignmentInfos2 = PropertyAssignmentInfos.builder()
                .filters(List.of(filter2))
                .editedField(OPERATIONAL_LIMITS_GROUP_2_WITH_PROPERTIES.name())
                .propertyName("property2")
                .value("value2")
                .build();
        PropertyAssignmentInfos assignmentInfos3 = PropertyAssignmentInfos.builder()
                .filters(List.of(filter3))
                .editedField(OPERATIONAL_LIMITS_GROUP_1_WITH_PROPERTIES.name())
                .propertyName("property0")
                .value("value0")
                .build();
        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3));
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
        Network network = getNetwork();
        Line line1 = network.getLine(LINE_ID_1);
        Line line2 = network.getLine(LINE_ID_2);
        Line line3 = network.getLine(LINE_ID_3);

        Assertions.assertTrue(line1.getSelectedOperationalLimitsGroupId1().isPresent());
        Assertions.assertEquals("group1", line1.getSelectedOperationalLimitsGroupId1().get());
        Assertions.assertTrue(line1.getSelectedOperationalLimitsGroupId2().isPresent());
        Assertions.assertEquals("group0", line1.getSelectedOperationalLimitsGroupId2().get());
        Assertions.assertTrue(line2.getSelectedOperationalLimitsGroupId1().isPresent());
        Assertions.assertEquals("group1", line2.getSelectedOperationalLimitsGroupId1().get());
        Assertions.assertTrue(line2.getSelectedOperationalLimitsGroupId2().isPresent());
        Assertions.assertEquals("group0", line2.getSelectedOperationalLimitsGroupId2().get());

        Assertions.assertTrue(line3.getSelectedOperationalLimitsGroupId1().isPresent());
        Assertions.assertEquals("group0", line3.getSelectedOperationalLimitsGroupId1().get());
        Assertions.assertTrue(line3.getSelectedOperationalLimitsGroupId2().isPresent());
        Assertions.assertEquals("group3", line3.getSelectedOperationalLimitsGroupId2().get());
        List<String> allLogs = new ArrayList<>();
        reportNode.getChildren().forEach(node -> node.getChildren().forEach(child -> child.getChildren().forEach(reportNode1 -> allLogs.add(reportNode1.getMessage()))));
        Assertions.assertTrue(allLogs.contains("Cannot modify equipment line_4 : multiple limit sets applicable on side 1 with property named property0 and with value value0 : equipment ignored"));
        Assertions.assertTrue(allLogs.contains("Cannot modify equipment line_5 : missing limit set applicable on side 1 with property named property0 and with value value0 : equipment ignored"));
    }
}
