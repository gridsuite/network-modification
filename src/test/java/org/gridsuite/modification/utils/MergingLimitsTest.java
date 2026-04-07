/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.powsybl.commons.report.PowsyblCoreReportResourceBundle;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;

import java.io.IOException;
import java.util.Optional;

import static org.gridsuite.modification.utils.TestUtils.testReportNode;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Etienne LESOT <etienne.lesot at rte-france.com>
 */
public final class MergingLimitsTest {

    private MergingLimitsTest() {

    }

    public static void testModificationMergedLimits(Network network, ModificationInfos modificationInfos, String replacementLineId, String expectedReportNodePath) throws IOException {
        Line line1 = network.getLine("l1");
        line1.newOperationalLimitsGroup1("groupNotMergedSide1");
        assertTrue(line1.getOperationalLimitsGroup1("group1").isPresent());
        assertNotNull(line1.getOperationalLimitsGroup1("group1").get().getProperty("property1"));
        assertEquals("value1", line1.getOperationalLimitsGroup1("group1").get().getProperty("property1"));
        line1.getOperationalLimitsGroup1("group1").get().setProperty("property2", "value2");
        line1.getOperationalLimitsGroup1("group1").get().setProperty("property3", "value3");
        line1.getOperationalLimitsGroup1("group1").get().newCurrentLimits()
                .setPermanentLimit(10)
                .beginTemporaryLimit()
                .setName("limit1")
                .setValue(16)
                .setAcceptableDuration(10)
                .endTemporaryLimit()
                .beginTemporaryLimit()
                .setName("limitNotMerged")
                .setValue(20)
                .setAcceptableDuration(100)
                .endTemporaryLimit()
                .add();

        Line line2 = network.getLine("l2");
        line2.newOperationalLimitsGroup2("groupNotMergedSide2");
        assertTrue(line2.getOperationalLimitsGroup1("group1").isPresent());
        assertNotNull(line2.getOperationalLimitsGroup1("group1").get().getProperty("property1"));
        assertEquals("value1", line2.getOperationalLimitsGroup1("group1").get().getProperty("property1"));
        line2.getOperationalLimitsGroup1("group1").get().setProperty("property2", "differentValue");
        line2.getOperationalLimitsGroup1("group1").get().setProperty("property4", "value4");
        line2.getOperationalLimitsGroup1("group1").get().newCurrentLimits()
                .setPermanentLimit(12)
                .beginTemporaryLimit()
                .setName("limit1")
                .setValue(13)
                .setAcceptableDuration(10)
                .endTemporaryLimit()
                .add();
        ReportNode report = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME, PowsyblCoreReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        modificationInfos.toModification().apply(network, report);

        Line line = network.getLine(replacementLineId);
        assertNotNull(line);
        assertNotNull(line.getOperationalLimitsGroups1());
        assertEquals(3, line.getOperationalLimitsGroups1().size());
        Optional<OperationalLimitsGroup> group1 = line.getOperationalLimitsGroup1("group1");
        assertTrue(group1.isPresent());
        assertTrue(group1.get().getCurrentLimits().isPresent());
        CurrentLimits currentLimits = group1.get().getCurrentLimits().get();
        assertEquals(10, currentLimits.getPermanentLimit());
        assertEquals(1, currentLimits.getTemporaryLimits().size());
        LoadingLimits.TemporaryLimit temporaryLimit = currentLimits.getTemporaryLimit(10);
        assertNotNull(temporaryLimit);
        assertEquals(13, temporaryLimit.getValue());
        assertEquals("limit1", temporaryLimit.getName());

        // properties
        assertEquals(1, group1.get().getPropertyNames().size());
        assertNotNull(group1.get().getProperty("property1"));
        assertEquals("value1", group1.get().getProperty("property1"));
        assertNull(group1.get().getProperty("property2"));
        assertNull(group1.get().getProperty("property3"));

        // side 2
        assertNotNull(line.getOperationalLimitsGroups2());
        assertEquals(4, line.getOperationalLimitsGroups2().size());
        testReportNode(report, expectedReportNodePath);
    }
}
