/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularmodifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.CurrentLimits;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
public class LimitSetModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1").operationalLimitsGroup1(List.of(OperationalLimitsGroupModificationInfos.builder()
                                        .id("DEFAULT")
                                        .side("ONE")
                                        .modificationType(OperationalLimitsGroupModificationType.MODIFIED)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.REPLACED)
                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                .temporaryLimits(List.of(
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.REPLACED)
                                                                .name("test1")
                                                                .acceptableDuration(2)
                                                                .value(10.)
                                                                .build()
                                                )).build())
                                        .build()))
                                .build(),
                        LineModificationInfos.builder().equipmentId("line1").operationalLimitsGroup1(List.of(OperationalLimitsGroupModificationInfos.builder()
                                        .id("DEFAULT")
                                        .side("ONE")
                                        .modificationType(OperationalLimitsGroupModificationType.MODIFIED)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.ADDED)
                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                .temporaryLimits(List.of(
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.ADDED)
                                                                .name("test2")
                                                                .acceptableDuration(1)
                                                                .value(10.)
                                                                .build()
                                                )).build())
                                        .build()))
                                .build()))
                .build();
    }

    @Test
    @Override
    public void testApply() {
        ModificationInfos modificationInfos = buildModification();
        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);
        assertAfterNetworkModificationApplication(reportNode);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        CurrentLimits line1CurrentLimits = getNetwork().getLine("line1").getOperationalLimitsGroup1("DEFAULT").orElse(null).getCurrentLimits().orElse(null);
        assertEquals(2, line1CurrentLimits.getTemporaryLimits().size());
        assertEquals("test2", line1CurrentLimits.getTemporaryLimit(1).getName());
        assertEquals("test1", line1CurrentLimits.getTemporaryLimit(2).getName());

    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogMessageWithoutRank("Previous temporary limits were removed", "network.modification.temporaryLimitsReplaced", reportNode);
    }

    @Override
    protected void checkModification() {
    }
}
