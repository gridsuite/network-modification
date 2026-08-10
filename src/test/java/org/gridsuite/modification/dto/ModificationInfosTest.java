/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.powsybl.commons.report.ReportNode;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 * Test class for ModificationInfos base class methods that should be overridden by subclasses.
 */
class ModificationInfosTest {

    private static final String TAG = "PH1";
    private static final String OTHER_TAG = "PH2";

    private static final class TestModificationInfos extends ModificationInfos {
        // Intentionally does not override createSubReportNode() or toModification()
        // to test the UnsupportedOperationException throwing behavior
    }

    private static ModificationInfos modificationInfos(Boolean activated, Map<String, Boolean> applicabilityByRootNetworkTag) {
        ModificationInfos modificationInfos = new TestModificationInfos();
        modificationInfos.setActivated(activated);
        modificationInfos.setApplicabilityByRootNetworkTag(applicabilityByRootNetworkTag);
        return modificationInfos;
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

    @Test
    void testNotActivatedWhenModificationIsDeactivated() {
        assertFalse(modificationInfos(false, Map.of(TAG, true)).isActivatedOn(TAG),
                "A deactivated modification is activated on no root network, whatever its applicabilities");
        assertFalse(modificationInfos(null, Map.of(TAG, true)).isActivatedOn(TAG),
                "An undefined activation flag is not an activation");
    }

    @Test
    void testActivatedWhenNoRootNetworkTagGiven() {
        assertTrue(modificationInfos(true, Map.of(TAG, false)).isActivatedOn(null),
                "A null tag means no root network context: the per root network applicabilities are then ignored");
    }

    @Test
    void testActivatedWhenNoApplicabilityDefined() {
        assertTrue(modificationInfos(true, null).isActivatedOn(TAG),
                "A modification without any applicability is activated on every root network");
    }

    @Test
    void testActivatedWhenTagHasNoEntry() {
        assertTrue(modificationInfos(true, Map.of()).isActivatedOn(TAG),
                "A tag without an entry is applicable");
        assertTrue(modificationInfos(true, Map.of(OTHER_TAG, false)).isActivatedOn(TAG),
                "An entry deactivating another tag leaves this one applicable");
    }

    @Test
    void testActivationFollowsTagEntry() {
        assertTrue(modificationInfos(true, Map.of(TAG, true)).isActivatedOn(TAG));
        assertFalse(modificationInfos(true, Map.of(TAG, false)).isActivatedOn(TAG));
    }

    @Test
    void testTagsAreIndependent() {
        ModificationInfos modificationInfos = modificationInfos(true, Map.of(TAG, false, OTHER_TAG, true));
        assertFalse(modificationInfos.isActivatedOn(TAG));
        assertTrue(modificationInfos.isActivatedOn(OTHER_TAG),
                "Deactivating a tag must not affect the other ones");
    }
}
