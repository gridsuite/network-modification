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
import lombok.Getter;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.FilterLoader;
import org.gridsuite.modification.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.modifications.ByFilterDeletion;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.TestUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
abstract class AbstractByFilterDeletionTest extends AbstractNetworkModificationTest {
    protected static final UUID FILTER_ID_1 = UUID.randomUUID();
    protected static final UUID FILTER_ID_2 = UUID.randomUUID();

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract EquipmentType getEquipmentType();

    public abstract Map<UUID, Set<String>> getFilterMapping();

    public abstract Set<String> getExistingEquipments();

    @Getter
    private final FilterLoader filterLoader = TestUtils.createFilterLoader(getEquipmentType(), getFilterMapping());

    @BeforeEach
    void specificSetUp() {
        MockitoAnnotations.openMocks(this);
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
    }

    @Test
    @Override
    public void testApply() throws Exception {
        ModificationInfos modificationInfo = buildModification();
        AbstractModification modification = modificationInfo.toModification(filterLoader);
        modification.apply(getNetwork());
        assertAfterNetworkModificationApplication();
    }

    @Override
    protected void checkModification() {
    }

    @Test
    void testCreateAllFiltersWrong() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ByFilterDeletionInfos byFilterDeletionInfos = ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1))
                .build();

        ByFilterDeletion byFilterDeletion = (ByFilterDeletion) byFilterDeletionInfos.toModification(_ -> List.of());
        ReportNode report = byFilterDeletionInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        byFilterDeletion.apply(getNetwork(), report);
        assertLogMessage("No equipment will be removed",
            "network.modification.byFilterDeletion.noEquipmentToRemove", report);
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

    @Test
    void testApplyWithDuplicateFilters() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ModificationInfos modificationInfos = ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1, filter1))
                .build();

        ByFilterDeletion byFilterDeletion = (ByFilterDeletion) modificationInfos.toModification(getFilterLoader());
        assertEquals(1, byFilterDeletion.getFilters().size());
    }

    @Test
    void testApplyWithDuplicateFilteredElements() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter2")
                .build();

        ModificationInfos modificationInfos = ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1, filter2))
                .build();

        ByFilterDeletion byFilterDeletion = (ByFilterDeletion) modificationInfos.toModification(TestUtils.createFilterLoader(getEquipmentType(), Map.of(
                FILTER_ID_1, getExistingEquipments(), // duplicating existing equipments in 2 different filters
                FILTER_ID_2, getExistingEquipments()
        )));
        ReportNode rootReportNode = ReportNode.newRootReportNode().withAllResourceBundlesFromClasspath().withMessageTemplate("test").build();
        byFilterDeletion.apply(getNetwork(), rootReportNode);
        assertEquals("%d equipments of type=%s will be removed".formatted(getExistingEquipments().size(), getEquipmentType()), rootReportNode.getChildren().get(3).getMessage());
    }
}
