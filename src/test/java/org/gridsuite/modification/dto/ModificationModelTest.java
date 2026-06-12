/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.powsybl.commons.report.ReportNode;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.ModificationModel;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 * Test class for ModificationModel base class methods that should be overridden by subclasses.
 */
class ModificationModelTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    private static final class TestModificationModel extends ModificationModel {
        // Intentionally does not override createSubReportNode() or toModification()
        // to test the UnsupportedOperationException throwing behavior
    }

    @Test
    void testCreateSubReportNodeThrowsUnsupportedOperationException() {
        ModificationModel modificationInfos = new TestModificationModel();
        ReportNode mockReportNode = ReportNode.newRootReportNode().withMessageTemplate("test").build();

        UnsupportedOperationException exception = assertThrows(
            UnsupportedOperationException.class,
            () -> modificationInfos.createSubReportNode(mockReportNode),
            "createSubReportNode should throw UnsupportedOperationException when not implemented"
        );

        String expectedMessage = "Method createSubReportNode must be implemented in subclass TestModificationModel";
        assertEquals(expectedMessage, exception.getMessage(),
            "Exception message should indicate which method and class need implementation");
    }

    @Test
    void testToModificationThrowsUnsupportedOperationException() {
        ModificationModel modificationInfos = new TestModificationModel();

        UnsupportedOperationException exception = assertThrows(
            UnsupportedOperationException.class,
            modificationInfos::toModification,
            "toModification should throw UnsupportedOperationException when not implemented"
        );

        String expectedMessage = "Method toModification must be implemented in subclass TestModificationModel";
        assertEquals(expectedMessage, exception.getMessage(),
            "Exception message should indicate which method and class need implementation");
    }

    @Test
    void testModificationInfosToModelReturnsSameInstance() {
        ModificationInfos modificationInfos = new LineModificationInfos();

        assertSame(modificationInfos, modificationInfos.toModel());
    }

    @Test
    void testModificationInfosPolymorphicDeserialization() throws JsonProcessingException {
        LineCreationInfos modificationInfos = LineCreationInfos.builder()
            .equipmentId("line")
            .build();

        ObjectNode json = objectMapper.valueToTree(modificationInfos);
        // These values are not read during deserialization
        json.put("messageType", "customMessage");
        json.put("messageValues", "customValues");

        ModificationInfos deserialized = objectMapper.treeToValue(json, ModificationInfos.class);

        LineCreationInfos lineCreationInfos = assertInstanceOf(LineCreationInfos.class, deserialized);
        assertEquals("LINE_CREATION", lineCreationInfos.getMessageType());
        assertEquals("{\"equipmentId\":\"line\"}", lineCreationInfos.getMessageValues());
    }

    @Test
    void testNestedModificationInfosPolymorphicDeserialization() throws JsonProcessingException {
        CompositeModificationInfos modificationInfos = CompositeModificationInfos.builder()
            .name("composite")
            .modificationsInfos(List.of(LineCreationInfos.builder().equipmentId("line").build()))
            .build();

        ModificationInfos deserialized = objectMapper.readValue(
            objectMapper.writeValueAsString(modificationInfos),
            ModificationInfos.class
        );

        CompositeModificationInfos composite = assertInstanceOf(CompositeModificationInfos.class, deserialized);
        assertInstanceOf(LineCreationInfos.class, composite.getModificationsInfos().getFirst());
    }

    @Test
    void testUnknownModificationTypeIsRejected() {
        assertThrows(JsonProcessingException.class, () -> objectMapper.readValue(
            "{\"type\":\"UNKNOWN_MODIFICATION\"}",
            ModificationInfos.class
        ));
    }

    @Test
    void testEveryModificationTypeHasJsonSubtype() throws Exception {
        JsonSubTypes jsonSubTypes = ModificationInfos.class.getAnnotation(JsonSubTypes.class);
        Set<String> subtypeNames = Arrays.stream(jsonSubTypes.value())
            .map(JsonSubTypes.Type::value)
            .map(this::newInstance)
            .map(ModificationInfos::getType)
            .map(Enum::name)
            .collect(Collectors.toSet());
        Set<String> modificationTypeNames = Arrays.stream(ModificationType.values())
            .map(Enum::name)
            .collect(Collectors.toSet());

        assertEquals(modificationTypeNames, subtypeNames);

        for (JsonSubTypes.Type subtype : jsonSubTypes.value()) {
            ModificationInfos modificationInfos = newInstance(subtype.value());
            ModificationInfos deserialized = objectMapper.treeToValue(
                objectMapper.createObjectNode().put("type", modificationInfos.getType().name()),
                ModificationInfos.class
            );
            assertInstanceOf(subtype.value(), deserialized);
        }
    }

    private ModificationInfos newInstance(Class<?> type) {
        try {
            return (ModificationInfos) type.getConstructor().newInstance();
        } catch (ReflectiveOperationException e) {
            throw new AssertionError("Could not instantiate " + type.getName(), e);
        }
    }
}
