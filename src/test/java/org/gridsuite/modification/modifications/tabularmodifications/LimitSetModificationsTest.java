/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularmodifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.CurrentLimits;
import com.powsybl.iidm.network.Line;
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
                        LineModificationInfos.builder().equipmentId("line1").operationalLimitsGroups(
                                List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                        .id("DEFAULT")
                                        .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                        .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.REPLACED)
                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                .temporaryLimits(List.of(
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.REPLACE)
                                                                .name("test1")
                                                                .acceptableDuration(2)
                                                                .value(10.)
                                                                .build()
                                                )).build())
                                        .build(),
                                OperationalLimitsGroupModificationInfos.builder()
                                        .id("DEFAULT")
                                        .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                        .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.ADDED)
                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                .temporaryLimits(List.of(
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.ADD)
                                                                .name("test2")
                                                                .acceptableDuration(1)
                                                                .value(10.)
                                                                .build()
                                                )).build())
                                        .build()))
                                .build(),
                        LineModificationInfos.builder().equipmentId("line2").operationalLimitsGroup1(List.of(OperationalLimitsGroupModificationInfos.builder()
                                        .id("DEFAULT")
                                        .side("ONE")
                                        .selectedOperationalLimitsGroupId("DEFAULT")
                                        .modificationType(OperationalLimitsGroupModificationType.ADD)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                .permanentLimit(99.)
                                                .temporaryLimits(List.of(
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.ADD)
                                                                .name("test1")
                                                                .acceptableDuration(1)
                                                                .value(10.)
                                                                .build()
                                                )).build())
                                        .build()))
                                .build(),
                        // Should generate an warning because there's no match for this temporary limit modification
                        LineModificationInfos.builder().equipmentId("line1").operationalLimitsGroup1(List.of(OperationalLimitsGroupModificationInfos.builder()
                                        .id("DEFAULT")
                                        .side("ONE")
                                        .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                .temporaryLimits(List.of(
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.MODIFY)
                                                                .name("test1")
                                                                .acceptableDuration(3)
                                                                .value(10.)
                                                                .build()
                                                )).build())
                                        .build()))
                                .build(),
                        //Should fail since provided operational limit group already exists on this side
                        LineModificationInfos.builder().equipmentId("line2").operationalLimitsGroup1(List.of(OperationalLimitsGroupModificationInfos.builder()
                                        .id("DEFAULT")
                                        .side("ONE")
                                        .selectedOperationalLimitsGroupId("DEFAULT")
                                        .modificationType(OperationalLimitsGroupModificationType.ADD)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                        .build()))
                                .build(),
                        //group0 already exists in network for this equipment
                        LineModificationInfos.builder().equipmentId("line2").operationalLimitsGroup2(List.of(OperationalLimitsGroupModificationInfos.builder()
                                        .id("group0")
                                        .side("TWO")
                                        .selectedOperationalLimitsGroupId("group0")
                                        .modificationType(OperationalLimitsGroupModificationType.REPLACE)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                .permanentLimit(99.)
                                                .temporaryLimits(List.of(
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.ADD)
                                                                .name("test1")
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

        Line line2 = getNetwork().getLine("line2");
        CurrentLimits line2CurrentLimitsSide1 = line2.getOperationalLimitsGroup1("DEFAULT").orElse(null).getCurrentLimits().orElse(null);
        assertEquals(1, line2CurrentLimitsSide1.getTemporaryLimits().size());
        assertEquals("test1", line2CurrentLimitsSide1.getTemporaryLimit(1).getName());
        assertEquals("DEFAULT", line2.getSelectedOperationalLimitsGroupId1().orElse(null));

        CurrentLimits line2CurrentLimitsSide2 = line2.getOperationalLimitsGroup2("group0").orElse(null).getCurrentLimits().orElse(null);
        assertEquals(1, line2CurrentLimitsSide2.getTemporaryLimits().size());
        assertEquals("test1", line2CurrentLimitsSide2.getTemporaryLimit(1).getName());
        assertEquals("group0", line2.getSelectedOperationalLimitsGroupId2().orElse(null));
    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogMessageWithoutRank("Operational limits group named DEFAULT has been modified on side ONE", "network.modification.operationalLimitsGroupModified", reportNode);
        assertLogMessageWithoutRank("Previous temporary limits were removed", "network.modification.temporaryLimitsReplaced", reportNode);
        assertLogMessageWithoutRank("Cannot add DEFAULT operational limit group, one with the given name already exists", "network.modification.tabular.modification.exception", reportNode);
        assertLogMessageWithoutRank("No existing temporary limit found with acceptableDuration = 3 matching is based on acceptableDuration if that helps", "network.modification.temporaryLimitsNoMatch", reportNode);
        assertLogMessageWithoutRank("Operational limits group named group0 is now active on side TWO", "network.modification.newSelectedOperationalLimitsGroup", reportNode);
        assertLogMessageWithoutRank("Operational limits group named group0 has been replaced on side TWO", "network.modification.operationalLimitsGroupReplaced", reportNode);
        assertLogMessageWithoutRank("New operational limits group added named DEFAULT on side ONE", "network.modification.operationalLimitsGroupAdded", reportNode);

    }

    @Override
    protected void checkModification() {
    }
}
