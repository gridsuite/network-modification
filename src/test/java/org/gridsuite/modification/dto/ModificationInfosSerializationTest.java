/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class ModificationInfosSerializationTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    void computedMetadataPropertiesAreNotSerialized() throws Exception {
        TabularModificationInfos modificationInfos = TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .build();

        JsonNode jsonNode = objectMapper.readTree(objectMapper.writeValueAsString(modificationInfos));

        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), jsonNode.get("type").asText());
        assertFalse(jsonNode.has("mapMessageValues"));
        assertFalse(jsonNode.has("errorType"));
    }

    @Test
    void computedMetadataPropertiesAreIgnoredOnDeserialization() throws Exception {
        String json = """
                {
                  "type": "TABULAR_MODIFICATION",
                  "modificationType": "GENERATOR_MODIFICATION",
                  "mapMessageValues": {
                    "tabularModificationType": "GENERATOR_MODIFICATION"
                  },
                  "errorType": "TABULAR_MODIFICATION_ERROR"
                }
                """;

        ModificationInfos modificationInfos = objectMapper.readValue(json, ModificationInfos.class);

        assertEquals(ModificationType.TABULAR_MODIFICATION, modificationInfos.getType());
    }
}
