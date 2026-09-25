/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.dto;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
class ModificationReferenceInfosTest {
    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void anUnresolvedPermissionIsLeftOutOfTheJson() throws JsonProcessingException {
        assertFalse(mapper.writeValueAsString(buildReference(null)).contains("permission"));
    }

    @Test
    void aResolvedPermissionIsSerialized() throws JsonProcessingException {
        assertTrue(mapper.writeValueAsString(buildReference(PermissionType.WRITE)).contains("\"permission\":\"WRITE\""));
        assertTrue(mapper.writeValueAsString(buildReference(PermissionType.READ)).contains("\"permission\":\"READ\""));
    }

    @Test
    void aJsonWithoutPermissionLeavesItUnresolved() throws JsonProcessingException {
        String json = mapper.writeValueAsString(buildReference(null));
        assertNull(mapper.readValue(json, ModificationReferenceInfos.class).getPermission());
    }

    @Test
    void aPermissionGrantsTheWeakerOnes() {
        assertTrue(PermissionType.MANAGE.grants(PermissionType.WRITE));
        assertTrue(PermissionType.WRITE.grants(PermissionType.WRITE));
        assertTrue(PermissionType.WRITE.grants(PermissionType.READ));
        assertFalse(PermissionType.READ.grants(PermissionType.WRITE));
        assertEquals(PermissionType.READ, PermissionType.values()[0]);
    }

    private ModificationReferenceInfos buildReference(PermissionType permission) {
        return ModificationReferenceInfos.builder()
            .referenceType(ModificationReferenceInfos.Type.DIRECTORY)
            .referencedId(UUID.randomUUID())
            .permission(permission)
            .build();
    }
}
