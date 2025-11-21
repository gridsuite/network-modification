/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilterdeletion;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.modifications.ByFilterDeletion;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
abstract class AbstractByFilterDeletionTest extends AbstractNetworkModificationTest {
    protected static final UUID FILTER_ID_1 = UUID.randomUUID();
    protected static final UUID FILTER_ID_2 = UUID.randomUUID();
    protected static final String EQUIPMENT_WRONG_ID_1 = "wrongId1";

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract EquipmentType getEquipmentType();

    protected abstract String getExistingId();

    protected abstract Map<UUID, FilterEquipments> getTestFilters();

    @Mock
    protected IFilterService filterService;

    @BeforeEach
    void specificSetUp() {
        MockitoAnnotations.openMocks(this);
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
    }

    @Test
    @Override
    public void testApply() throws Exception {
        ModificationInfos modificationInfo = buildModification();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(getTestFilters());
        AbstractModification modification = modificationInfo.toModification();
        modification.initApplicationContext(filterService, null);
        modification.apply(getNetwork());
        assertAfterNetworkModificationApplication();
    }

    @Override
    protected void checkModification() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ByFilterDeletionInfos byFilterDeletionInfos = ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1))
                .build();

        Map<UUID, FilterEquipments> filterEquipments = Map.of(
                FILTER_ID_1,
                FilterEquipments.builder().filterId(FILTER_ID_1).filterName("filter1").identifiableAttributes(List.of(
                        new IdentifiableAttributes(getExistingId(), getIdentifiableType(), null)))
                        .notFoundEquipments(List.of(EQUIPMENT_WRONG_ID_1)).build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filterEquipments);

        ByFilterDeletion byFilterDeletion = (ByFilterDeletion) byFilterDeletionInfos.toModification();
        byFilterDeletion.initApplicationContext(filterService, null);
        ReportNode report = byFilterDeletionInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        byFilterDeletion.apply(getNetwork(), report);

        assertLogMessage("Cannot find the following equipments " + EQUIPMENT_WRONG_ID_1 + " in filter filter1",
            "network.modification.filterEquipmentsNotFound.inFilter", report);
    }

    @Test
    void testCreateAllFiltersWrong() throws Exception {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ByFilterDeletionInfos byFilterDeletionInfos = ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1))
                .build();

        Map<UUID, FilterEquipments> filters = Map.of(
                FILTER_ID_1, FilterEquipments.builder().identifiableAttributes(List.of())
                        .notFoundEquipments(List.of(EQUIPMENT_WRONG_ID_1)).build());
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(filters);

        ByFilterDeletion byFilterDeletion = (ByFilterDeletion) byFilterDeletionInfos.toModification();
        byFilterDeletion.initApplicationContext(filterService, null);
        ReportNode report = byFilterDeletionInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        byFilterDeletion.apply(getNetwork(), report);
        assertLogMessage(byFilterDeletionInfos.getErrorType().name() + ": There is no valid equipment ID among the provided filter(s)",
            "network.modification.invalidFilters", report);
    }

    @Override
    protected ModificationInfos buildModification() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter2")
                .build();

        return ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1, filter2))
                .build();
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BY_FILTER_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(getIdentifiableType().name(), createdValues.get("equipmentType"));
    }
}
