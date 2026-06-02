/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.mapper;

import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.GeneratorModificationInfos;
import org.gridsuite.modification.dto.LoadModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.model.CompositeModificationModel;
import org.gridsuite.modification.model.GeneratorModificationModel;
import org.gridsuite.modification.model.LoadModificationModel;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Guards the dto -> model mapping: business fields are preserved, server metadata is dropped,
 * the JavaBeans/Lombok edge-case fields (measurements, qPercent) are copied, and composite children
 * are filtered by activation/stash status.
 */
class ModificationModelMapperTest {
    private final ModificationModelMapper mapper = ModificationModelMapper.INSTANCE;

    @Test
    void toModelPreservesBusinessFieldsAndMapsMeasurements() {
        LoadModificationInfos infos = LoadModificationInfos.builder()
                .equipmentId("load1")
                .p0(new AttributeModification<>(100.0, OperationType.SET))
                .pMeasurementValue(new AttributeModification<>(42.0, OperationType.SET))
                .pMeasurementValidity(new AttributeModification<>(true, OperationType.SET))
                .build();
        // server metadata that must NOT leak into the model
        infos.setUuid(UUID.randomUUID());
        infos.setDate(Instant.now());

        LoadModificationModel model = (LoadModificationModel) mapper.toModel(infos);

        assertEquals("load1", model.getEquipmentId());
        assertNotNull(model.getP0());
        assertEquals(100.0, model.getP0().getValue().doubleValue());
        // regression guard for the @AfterMapping measurement copy (MapStruct skips these by default)
        assertNotNull(model.getPMeasurementValue());
        assertEquals(42.0, model.getPMeasurementValue().getValue().doubleValue());
        assertNotNull(model.getPMeasurementValidity());
    }

    @Test
    void toModelMapsGeneratorQPercent() {
        GeneratorModificationInfos infos = GeneratorModificationInfos.builder()
                .equipmentId("gen1")
                .qPercent(new AttributeModification<>(75.0, OperationType.SET))
                .build();

        GeneratorModificationModel model = (GeneratorModificationModel) mapper.toModel(infos);

        assertNotNull(model.getQPercent());
        assertEquals(75.0, model.getQPercent().getValue().doubleValue());
    }

    @Test
    void compositeMappingKeepsOnlyActiveNonStashedChildren() {
        ModificationInfos active = LoadModificationInfos.builder().equipmentId("active").build();
        active.setActivated(true);
        active.setStashed(false);

        ModificationInfos inactive = LoadModificationInfos.builder().equipmentId("inactive").build();
        inactive.setActivated(false);
        inactive.setStashed(false);

        ModificationInfos stashed = LoadModificationInfos.builder().equipmentId("stashed").build();
        stashed.setActivated(true);
        stashed.setStashed(true);

        CompositeModificationInfos composite = CompositeModificationInfos.builder()
                .modificationsInfos(List.of(active, inactive, stashed))
                .build();

        CompositeModificationModel model = (CompositeModificationModel) mapper.toModel(composite);

        assertEquals(1, model.getModificationsInfos().size());
        assertEquals("active", ((LoadModificationModel) model.getModificationsInfos().get(0)).getEquipmentId());
    }
}
