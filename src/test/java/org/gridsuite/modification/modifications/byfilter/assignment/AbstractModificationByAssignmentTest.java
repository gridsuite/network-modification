/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.assignment;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import lombok.Getter;
import org.gridsuite.filter.report.FilterReportResourceBundle;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.context.FilterLoader;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.byfilter.assignment.*;
import org.gridsuite.modification.dto.byfilter.equipmentfield.BatteryField;
import org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.modifications.byfilter.ModificationByAssignment;
import org.gridsuite.modification.modifications.data.assignment.DataType;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.gridsuite.modification.utils.TestUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import java.time.Instant;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
abstract class AbstractModificationByAssignmentTest extends AbstractNetworkModificationTest {
    protected static final UUID FILTER_ID_1 = UUID.randomUUID();
    protected static final UUID FILTER_ID_2 = UUID.randomUUID();
    protected static final UUID FILTER_ID_3 = UUID.randomUUID();
    protected static final UUID FILTER_ID_4 = UUID.randomUUID();
    protected static final UUID FILTER_ID_5 = UUID.randomUUID();
    protected static final UUID FILTER_ID_6 = UUID.randomUUID();
    protected final FilterInfos filter1 = new FilterInfos(FILTER_ID_1, "filter1");
    protected final FilterInfos filter2 = new FilterInfos(FILTER_ID_2, "filter2");
    protected final FilterInfos filter3 = new FilterInfos(FILTER_ID_3, "filter3");
    protected final FilterInfos filter4 = new FilterInfos(FILTER_ID_4, "filter4");
    protected final FilterInfos filter5 = new FilterInfos(FILTER_ID_5, "filter5");
    protected final FilterInfos filter6 = new FilterInfos(FILTER_ID_6, "filter6");
    protected final ReportNode reportNode = ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME, FilterReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test")
            .build();
    @Getter
    private final FilterLoader filterLoader = TestUtils.createFilterLoader(getEquipmentType(), getFilterMapping());

    public abstract Map<UUID, Set<String>> getFilterMapping();

    public abstract Map<UUID, Set<String>> getFilterMapping();

    @Getter
    private final FilterLoader filterLoader = TestUtils.createFilterLoader(getEquipmentType(), getFilterMapping());

    @BeforeEach
    void specificSetUp() {
        MockitoAnnotations.openMocks(this);
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        createEquipments();
    }

    @Test
    @Override
    public void testApply() throws Exception {
        ModificationInfos modificationInfo = buildModification();
        ModificationContext modificationContext = ModificationContext.builder().filterLoader(this::loadFilters).build();
        AbstractModification modification = modificationInfo.toModification(modificationContext);
        modification.apply(getNetwork(), reportNode);
        assertAfterNetworkModificationApplication();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationByAssignmentInfos buildModification() {
        return ModificationByAssignmentInfos.builder()
                .equipmentType(getIdentifiableType())
                .assignmentInfosList(getAssignmentInfos())
                .stashed(false)
                .date(Instant.now())
                .build();
    }

    @Override
    protected void checkModification() {
    }

    protected void apply(ModificationByAssignmentInfos modificationByAssignmentInfos, FilterLoader filterLoader) {
        ModificationContext modificationContext = ModificationContext.builder().filterLoader(filterLoader).build();
        AbstractModification modification = modificationByAssignmentInfos.toModification(modificationContext);
        modification.apply(getNetwork());
    }

    protected abstract void createEquipments();

    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        PropertyAssignmentInfos spyAssignmentInfos = spy(PropertyAssignmentInfos.builder()
                .editedField(PropertyField.FREE_PROPERTIES.name())
                .propertyName("propertyName")
                .value("propertyValue")
                .filters(List.of(filter1))
                .build());
        doReturn(DataType.PROPERTY).when(spyAssignmentInfos).getDataType();
        return new ArrayList<>(List.of(spyAssignmentInfos));
    }

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract EquipmentType getEquipmentType();

    @Test
    void testApplyWithDuplicateFilters() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1, filter1)) // Same filter
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .value(80.)
                .build();
        EnumAssignmentInfos assignmentInfos2 = EnumAssignmentInfos.builder()
                .filters(List.of(filter1, filter1)) // Same filter
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .value("enum")
                .build();
        StringAssignmentInfos assignmentInfos3 = StringAssignmentInfos.builder()
                .filters(List.of(filter1, filter1)) // Same filter
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .value("string")
                .build();
        BooleanAssignmentInfos assignmentInfos4 = BooleanAssignmentInfos.builder()
                .filters(List.of(filter1, filter1)) // Same filter
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .value(true)
                .build();
        IntegerAssignmentInfos assignmentInfos5 = IntegerAssignmentInfos.builder()
                .filters(List.of(filter1, filter1)) // Same filter
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .value(2)
                .build();
        PropertyAssignmentInfos assignmentInfos6 = PropertyAssignmentInfos.builder()
                .filters(List.of(filter1, filter1)) // Same filter
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .value("property")
                .build();
        ModificationInfos modificationInfos = ModificationByAssignmentInfos.builder()
                .equipmentType(getIdentifiableType())
                .assignmentInfosList(List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3,
                        assignmentInfos4, assignmentInfos5, assignmentInfos6))
                .stashed(false)
                .date(Instant.now())
                .build();

        ModificationContext modificationContext = ModificationContext.builder().filterLoader(getFilterLoader()).build();
        ModificationByAssignment modificationByAssignment = (ModificationByAssignment) modificationInfos.toModification(modificationContext);
        modificationByAssignment.getAssignments().forEach(assignment ->
                assertEquals(1, assignment.getFilters().size()));
    }
}
