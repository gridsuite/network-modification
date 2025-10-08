/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.powsybl.commons.report.ReportNode;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 * Test class for ModificationInfos base class methods that should be overridden by subclasses.
 */
class ModificationInfosTest {

    private static class TestModificationInfos extends ModificationInfos {
        // Intentionally does not override createSubReportNode() or toModification()
        // to test the UnsupportedOperationException throwing behavior
    }

    @Test
    void testCreateSubReportNodeThrowsUnsupportedOperationException() {
        ModificationInfos modificationInfos = new TestModificationInfos();
        ReportNode mockReportNode = ReportNode.newRootReportNode().withMessageTemplate("test").build();

        UnsupportedOperationException exception = assertThrows(
                UnsupportedOperationException.class,
                () -> modificationInfos.createSubReportNode(mockReportNode),
                "createSubReportNode should throw UnsupportedOperationException when not implemented"
        );

        String expectedMessage = "Method createSubReportNode must be implemented in subclass TestModificationInfos";
        assertEquals(expectedMessage, exception.getMessage(),
                "Exception message should indicate which method and class need implementation");
    }

    @Test
    void testToModificationThrowsUnsupportedOperationException() {
        ModificationInfos modificationInfos = new TestModificationInfos();

        UnsupportedOperationException exception = assertThrows(
                UnsupportedOperationException.class,
                modificationInfos::toModification,
                "toModification should throw UnsupportedOperationException when not implemented"
        );

        String expectedMessage = "Method toModification must be implemented in subclass TestModificationInfos";
        assertEquals(expectedMessage, exception.getMessage(),
                "Exception message should indicate which method and class need implementation");
    }
}
