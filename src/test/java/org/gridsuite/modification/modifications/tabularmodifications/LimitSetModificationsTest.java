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
import org.gridsuite.modification.dto.tabular.LimitSetsTabularModificationInfos;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import static org.gridsuite.modification.dto.AttributeModification.toAttributeModification;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
class LimitSetModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .selectedOperationalLimitsGroupId1(new AttributeModification<>("", OperationType.UNSET))
                                .operationalLimitsGroups(
                                        List.of(
                                                OperationalLimitsGroupModificationInfos.builder()
                                                        .id("DEFAULT")
                                                        .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                        .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.REPLACE)
                                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                                .temporaryLimits(List.of(
                                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                                .modificationType(TemporaryLimitModificationType.REPLACE)
                                                                                .name(toAttributeModification("test1", OperationType.SET))
                                                                                .acceptableDuration(toAttributeModification(2, OperationType.SET))
                                                                                .value(toAttributeModification(10., OperationType.SET))
                                                                                .build()
                                                                )).build())
                                                        .build(),
                                                OperationalLimitsGroupModificationInfos.builder()
                                                        .id("DEFAULT")
                                                        .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                        .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                                .temporaryLimits(List.of(
                                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                                .modificationType(TemporaryLimitModificationType.ADD)
                                                                                .name(toAttributeModification("test2", OperationType.SET))
                                                                                .acceptableDuration(toAttributeModification(1, OperationType.SET))
                                                                                .value(toAttributeModification(10., OperationType.SET))
                                                                                .build()
                                                                )).build())
                                                        .build()))
                                .build(),
                        LineModificationInfos.builder()
                                .equipmentId("line2")
                                .selectedOperationalLimitsGroupId1(new AttributeModification<>("DEFAULT", OperationType.SET))
                                .selectedOperationalLimitsGroupId2(new AttributeModification<>("", OperationType.UNSET))
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.ADD)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .permanentLimit(99.)
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.ADD)
                                                                        .name(toAttributeModification("test1", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(1, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()))
                                .build(),
                        // Should generate an warning because there's no match for this temporary limit modification
                        LineModificationInfos.builder().equipmentId("line1").operationalLimitsGroups(List.of(OperationalLimitsGroupModificationInfos.builder()
                                        .id("DEFAULT")
                                        .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                        .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                                        .currentLimits(CurrentLimitsModificationInfos.builder()
                                                .temporaryLimits(List.of(
                                                        // throws a warning
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.MODIFY)
                                                                .name(toAttributeModification("test1", OperationType.SET))
                                                                .acceptableDuration(toAttributeModification(3, OperationType.SET))
                                                                .value(toAttributeModification(10., OperationType.SET))
                                                                .build(),
                                                        // valid modification
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.MODIFY)
                                                                .name(toAttributeModification("test1", OperationType.SET))
                                                                .acceptableDuration(toAttributeModification(2, OperationType.SET))
                                                                .value(toAttributeModification(50., OperationType.SET))
                                                                .build(),
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.ADD)
                                                                .name(toAttributeModification("test2_plus", OperationType.SET))
                                                                .acceptableDuration(toAttributeModification(1, OperationType.SET))
                                                                .value(toAttributeModification(25., OperationType.SET))
                                                                .build(),
                                                        CurrentTemporaryLimitModificationInfos.builder()
                                                                .modificationType(TemporaryLimitModificationType.DELETE)
                                                                .name(toAttributeModification("test2", OperationType.SET))
                                                                .acceptableDuration(toAttributeModification(1, OperationType.SET))
                                                                .value(toAttributeModification(15., OperationType.SET))
                                                                .build()
                                                )).build())
                                        .build()))
                                .build(),
                        //Should fail since provided operational limit group already exists on this side
                        LineModificationInfos.builder()
                                .equipmentId("line2")
                                .selectedOperationalLimitsGroupId1(new AttributeModification<>("DEFAULT", OperationType.SET))
                                .selectedOperationalLimitsGroupId2(new AttributeModification<>("DEFAULT", OperationType.SET))
                                .operationalLimitsGroups(List.of(OperationalLimitsGroupModificationInfos.builder()
                                        .id("DEFAULT")
                                        .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                        .modificationType(OperationalLimitsGroupModificationType.ADD)
                                        .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                        .build()))
                                .build(),
                        //group0 already exists in network for this equipment
                        LineModificationInfos.builder()
                                .equipmentId("line2")
                                .selectedOperationalLimitsGroupId2(new AttributeModification<>("group0", OperationType.SET))
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("group0")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE2)
                                                .modificationType(OperationalLimitsGroupModificationType.REPLACE)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .permanentLimit(99.)
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.ADD)
                                                                        .name(toAttributeModification("test1", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(1, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()))
                                .build(),
                        //group0 already exists in network for this equipment, so MODIFY_OR_ADD will be a modification
                        LineModificationInfos.builder()
                                .equipmentId("line2")
                                .selectedOperationalLimitsGroupId2(new AttributeModification<>("group0", OperationType.SET))
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("group0")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE2)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY_OR_ADD)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .permanentLimit(100.)
                                                        .build())
                                                .build()))
                                .build(),
                        LineModificationInfos.builder()
                                .equipmentId("line2")
                                .selectedOperationalLimitsGroupId2(new AttributeModification<>("group0", OperationType.SET))
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("UNKNOWN")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE2)
                                                .build()))
                                .build())
                )
                .build();
    }

    @Test
    @Override
    public void testApply() {
        ModificationInfos modificationInfos = buildModification();
        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);
        assertAfterNetworkModificationApplication(reportNode);
    }

    /**
     * Test modifying only the name of a temporary limit (value unchanged)
     */
    @Test
    void testModifyTemporaryLimitNameOnly() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.MODIFY)
                                                                        .name(toAttributeModification("renamed_limit", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(null) // No value modification
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        assertEquals("renamed_limit", limits.getTemporaryLimit(32).getName());
        assertEquals(15.0, limits.getTemporaryLimit(32).getValue(), 0.01);
    }

    /**
     * Test modifying only the value of a temporary limit (name unchanged)
     */
    @Test
    void testModifyTemporaryLimitValueOnly() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.MODIFY)
                                                                        .name(toAttributeModification("name32", OperationType.SET)) // No name modification
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(999., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        assertEquals(999., limits.getTemporaryLimit(32).getValue(), 0.01);
        assertNotNull(limits.getTemporaryLimit(32).getName());

    }

    /**
     * Test DELETE modification type with isThisLimitDeleted check
     */
    @Test
    void testDeleteTemporaryLimit() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.DELETE)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.DELETE)
                                                                        .name(toAttributeModification("test1", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        assertNull(limits.getTemporaryLimit(32));
    }

    /**
     * Test MODIFY_OR_ADD with non-existent limit (should add)
     */
    @Test
    void testModifyOrAddNonExistentLimit() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                                                        .name(toAttributeModification("new_limit", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(5, OperationType.SET))
                                                                        .value(toAttributeModification(150., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        assertNotNull(limits.getTemporaryLimit(5));
        assertEquals("new_limit", limits.getTemporaryLimit(5).getName());
        assertEquals(150., limits.getTemporaryLimit(5).getValue(), 0.01);
    }

    /**
     * Test modification with no actual changes (name and value same as existing)
     */
    @Test
    void testModifyWithNoActualChanges() {
        // First get current values
        CurrentLimits existingLimits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(existingLimits);

        String existingName = existingLimits.getTemporaryLimit(32).getName();
        double existingValue = existingLimits.getTemporaryLimit(32).getValue();

        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.MODIFY)
                                                                        .name(null) // Keep existing
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(null) // Keep existing
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        assertEquals(existingName, limits.getTemporaryLimit(32).getName());
        assertEquals(existingValue, limits.getTemporaryLimit(32).getValue(), 0.01);
    }

    /**
     * Test ADD with a missing name: the limit must not be added and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testAddTemporaryLimitWithMissingName() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                            .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                    .modificationType(TemporaryLimitModificationType.ADD)
                                                                        .name(toAttributeModification(null, OperationType.SET))
                                                                    .acceptableDuration(toAttributeModification(99, OperationType.SET))
                                                                    .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // the limit was not added
        assertNull(limits.getTemporaryLimit(99));
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to ADD: ignored",
            "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test REPLACE with a missing name: the limit must not be added and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testReplaceTemporaryLimitWithMissingName() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                            .temporaryLimitsModificationType(TemporaryLimitModificationType.REPLACE)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                    .modificationType(TemporaryLimitModificationType.REPLACE)
                                                                    .name(toAttributeModification(null, OperationType.SET))
                                                                    .acceptableDuration(toAttributeModification(99, OperationType.SET))
                                                                    .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // the limit was not added
        assertNull(limits.getTemporaryLimit(99));
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to REPLACE: ignored",
            "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test DELETE with a missing name: the limit must not be deleted and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testDeleteTemporaryLimitWithMissingName() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.DELETE)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.DELETE)
                                                                        .name(null)
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // the limit was not deleted
        assertNotNull(limits.getTemporaryLimit(32));
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to DELETE: ignored",
                "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test DELETE with a missing acceptableDuration: the limit must not be deleted and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testDeleteTemporaryLimitWithMissingDuration() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.DELETE)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.DELETE)
                                                                        .name(toAttributeModification("name32", OperationType.SET))
                                                                        .acceptableDuration(null)
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // the limit was not deleted
        assertNotNull(limits.getTemporaryLimit(32));
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to DELETE: ignored",
                "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test MODIFY with a missing name: the limit must not be modified and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testModifyTemporaryLimitWithMissingName() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
            .modificationType(ModificationType.LINE_MODIFICATION)
            .modifications(List.of(
                LineModificationInfos.builder().equipmentId("line1")
                    .operationalLimitsGroups(List.of(
                        OperationalLimitsGroupModificationInfos.builder()
                            .id("DEFAULT")
                            .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                            .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                            .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                            .currentLimits(CurrentLimitsModificationInfos.builder()
                                .temporaryLimits(List.of(
                                    CurrentTemporaryLimitModificationInfos.builder()
                                        .modificationType(TemporaryLimitModificationType.MODIFY)
                                        .name(toAttributeModification(null, OperationType.SET))
                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                        .value(toAttributeModification(999., OperationType.SET))
                                        .build()
                                )).build())
                            .build()
                    )).build()
            ))
            .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                .getOperationalLimitsGroup1("DEFAULT").orElse(null))
            .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // value is unchanged
        assertEquals(15.0, limits.getTemporaryLimit(32).getValue(), 0.01);
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to MODIFY: ignored",
            "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test MODIFY with a missing acceptableDuration: the limit must not be modified and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testModifyTemporaryLimitWithMissingDuration() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
            .modificationType(ModificationType.LINE_MODIFICATION)
            .modifications(List.of(
                LineModificationInfos.builder().equipmentId("line1")
                    .operationalLimitsGroups(List.of(
                        OperationalLimitsGroupModificationInfos.builder()
                            .id("DEFAULT")
                            .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                            .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                            .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                            .currentLimits(CurrentLimitsModificationInfos.builder()
                                .temporaryLimits(List.of(
                                    CurrentTemporaryLimitModificationInfos.builder()
                                        .modificationType(TemporaryLimitModificationType.MODIFY)
                                        .name(toAttributeModification("name32", OperationType.SET))
                                        .acceptableDuration(toAttributeModification(null, OperationType.SET))
                                        .value(toAttributeModification(999., OperationType.SET))
                                        .build()
                                )).build())
                            .build()
                    )).build()
            ))
            .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                .getOperationalLimitsGroup1("DEFAULT").orElse(null))
            .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // value is unchanged
        assertEquals(15.0, limits.getTemporaryLimit(32).getValue(), 0.01);
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to MODIFY: ignored",
            "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test MODIFY_OR_ADD with a missing name: the limit must not be modified and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testModifyOrAddTemporaryLimitWithMissingName() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                            .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                    .modificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                                                        .name(toAttributeModification(null, OperationType.SET))
                                                                    .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                    .value(toAttributeModification(999., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // value is unchanged
        assertEquals(15.0, limits.getTemporaryLimit(32).getValue(), 0.01);
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to MODIFY_OR_ADD: ignored",
            "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test MODIFY_OR_ADD with a missing acceptableDuration: the limit must not be modified and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testModifyOrAddTemporaryLimitWithMissingDuration() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
            .modificationType(ModificationType.LINE_MODIFICATION)
            .modifications(List.of(
                LineModificationInfos.builder().equipmentId("line1")
                    .operationalLimitsGroups(List.of(
                        OperationalLimitsGroupModificationInfos.builder()
                            .id("DEFAULT")
                            .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                            .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                            .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                            .currentLimits(CurrentLimitsModificationInfos.builder()
                                .temporaryLimits(List.of(
                                    CurrentTemporaryLimitModificationInfos.builder()
                                        .modificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                        .name(toAttributeModification("name32", OperationType.SET))
                                        .acceptableDuration(toAttributeModification(null, OperationType.SET))
                                        .value(toAttributeModification(999., OperationType.SET))
                                        .build()
                                )).build())
                            .build()
                    )).build()
            ))
            .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                .getOperationalLimitsGroup1("DEFAULT").orElse(null))
            .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // value is unchanged
        assertEquals(15.0, limits.getTemporaryLimit(32).getValue(), 0.01);
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to MODIFY_OR_ADD: ignored",
            "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test that when the OLG modification type is ADD and the temporary limit modification
     * type is not ADD (e.g. MODIFY), a temporaryLimitsWrongModification log is emitted and
     * the temporary limit modification type is forced to ADD so the limit is created.
     */
    @Test
    void testTemporaryLimitWrongModificationTypeIsForcedToAddOnAdd() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line2")
                                .selectedOperationalLimitsGroupId1(new AttributeModification<>("DEFAULT", OperationType.SET))
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.ADD)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .permanentLimit(99.)
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.MODIFY)
                                                                        .name(toAttributeModification("test1", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(1, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line2")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // the limit was added (modification type forced to ADD)
        assertNotNull(limits.getTemporaryLimit(1));
        assertEquals("test1", limits.getTemporaryLimit(1).getName());
        assertEquals(10.0, limits.getTemporaryLimit(1).getValue(), 0.01);
        assertLogMessageWithoutRank(
                "Temporary limit modification type can only be ADD when limit group modification type is not MODIFY: switched from MODIFY to ADD",
                "network.modification.temporaryLimitsWrongModification", reportNode);
    }

    /**
     * Test that when the OLG modification type is REPLACE and the temporary limit modification
     * type is not ADD (e.g. MODIFY), a temporaryLimitsWrongModification log is emitted and
     * the temporary limit modification type is forced to ADD so the limit is created.
     */
    @Test
    void testTemporaryLimitWrongModificationTypeIsForcedToAddOnReplace() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.REPLACE)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .permanentLimit(99.)
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.MODIFY)
                                                                        .name(toAttributeModification("test1", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(7, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // the limit was added (modification type forced to ADD)
        assertNotNull(limits.getTemporaryLimit(7));
        assertEquals("test1", limits.getTemporaryLimit(7).getName());
        assertEquals(10.0, limits.getTemporaryLimit(7).getValue(), 0.01);
        assertLogMessageWithoutRank(
                "Temporary limit modification type can only be ADD when limit group modification type is not MODIFY: switched from MODIFY to ADD",
                "network.modification.temporaryLimitsWrongModification", reportNode);
    }

    /**
     * Test that an ADD with a duration already used by another limit on the same side
     * is skipped with a WARN log.
     */
    @Test
    void testAddTemporaryLimitWithSameDurationIsSkipped() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.ADD)
                                                                        .name(toAttributeModification("different_name", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        assertLogMessageWithoutRank("Duplicate name or duration for temporary limit different_name (duration: 32) to ADD: ignored",
                "network.modification.temporaryLimitsDuplicate", reportNode);
    }

    /**
     * Test that an ADD with a name already used by another limit on the same side
     * is skipped with a WARN log.
     */
    @Test
    void testAddTemporaryLimitWithSameNameIsSkipped() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.ADD)
                                                                        .name(toAttributeModification("name32", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(99, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        assertLogMessageWithoutRank("Duplicate name or duration for temporary limit name32 (duration: 99) to ADD: ignored",
                "network.modification.temporaryLimitsDuplicate", reportNode);
    }

    /**
     * Test ADD with a missing acceptableDuration: the limit must not be added and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testAddTemporaryLimitWithMissingDuration() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.ADD)
                                                                        .name(toAttributeModification("new_limit", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(null, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // only the two pre-existing limits remain (no new one added)
        assertEquals(2, limits.getTemporaryLimits().size());
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to ADD: ignored",
                "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test REPLACE with a missing acceptableDuration: the limit must not be replaced and a temporaryLimitsMissingInfo log must be emitted.
     */
    @Test
    void testReplaceTemporaryLimitWithMissingDuration() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.REPLACE)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.REPLACE)
                                                                        .name(toAttributeModification("name32", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(null, OperationType.SET))
                                                                        .value(toAttributeModification(10., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // existing limit unchanged
        assertEquals(15.0, limits.getTemporaryLimit(32).getValue(), 0.01);
        assertLogMessageWithoutRank("Missing info (name or duration) to find temporary limit to REPLACE: ignored",
                "network.modification.temporaryLimitsMissingInfo", reportNode);
    }

    /**
     * Test REPLACE on an existing temporary limit: name and value must be replaced by the
     * provided ones, no carry-over of previous fields.
     */
    @Test
    void testReplaceTemporaryLimitOnExistingReplacesNameAndValue() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.REPLACE)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.REPLACE)
                                                                        .name(toAttributeModification("renamed", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(99., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        assertEquals("renamed", limits.getTemporaryLimit(32).getName());
        assertEquals(99., limits.getTemporaryLimit(32).getValue(), 0.01);
    }

    /**
     * Test REPLACE on an existing temporary limit with no value provided: the value
     * must NOT be carried over from the previous limit (REPLACE semantics).
     */
    @Test
    void testReplaceTemporaryLimitWithEmptyValueDoesNotPreservePrevious() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.REPLACE)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.REPLACE)
                                                                        .name(toAttributeModification("renamed", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(null)
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        assertEquals("renamed", limits.getTemporaryLimit(32).getName());
        // previous value (15.0) must not be preserved by REPLACE
        assertNotEquals(15.0, limits.getTemporaryLimit(32).getValue());
    }

    /**
     * Test that an ADD on a duration that is being deleted in the same batch is allowed
     * (no duplicate detected) since the conflicting limit is being removed.
     */
    @Test
    void testAddTemporaryLimitConflictingWithDeletedInSameBatchSucceeds() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                // delete the existing limit at duration 32
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.DELETE)
                                                                        .name(toAttributeModification("name32", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(15., OperationType.SET))
                                                                        .build(),
                                                                // add a new limit reusing the freed duration
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.ADD)
                                                                        .name(toAttributeModification("brand_new", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(42., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // the new limit replaced the deleted one at duration 32
        assertEquals("brand_new", limits.getTemporaryLimit(32).getName());
        assertEquals(42., limits.getTemporaryLimit(32).getValue(), 0.01);
    }

    /**
     * Test MODIFY where the new name collides with another existing limit's name on the same side:
     * must be skipped with a temporaryLimitsDuplicate WARN.
     */
    @Test
    void testModifyTemporaryLimitWithDuplicateName() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.MODIFY)
                                                                        // name33 already used by another existing limit (duration 33)
                                                                        .name(toAttributeModification("name33", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(99., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // limit at duration 32 unchanged
        assertEquals("name32", limits.getTemporaryLimit(32).getName());
        assertEquals(15.0, limits.getTemporaryLimit(32).getValue(), 0.01);
        assertLogMessageWithoutRank("Duplicate name or duration for temporary limit name33 (duration: 32) to MODIFY: ignored",
                "network.modification.temporaryLimitsDuplicate", reportNode);
    }

    /**
     * Test MODIFY_OR_ADD on an existing limit: behaves as a MODIFY (value updated, no add).
     */
    @Test
    void testModifyOrAddOnExistingLimit() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                                                        .name(toAttributeModification("name32", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(77., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // still 2 limits, value updated
        assertEquals(2, limits.getTemporaryLimits().size());
        assertEquals("name32", limits.getTemporaryLimit(32).getName());
        assertEquals(77., limits.getTemporaryLimit(32).getValue(), 0.01);
    }

    /**
     * Test MODIFY_OR_ADD where the provided name collides with another existing limit's name on
     * a different duration: must be skipped with a temporaryLimitsDuplicate WARN.
     */
    @Test
    void testModifyOrAddTemporaryLimitWithDuplicateName() {
        ModificationInfos modificationInfos = LimitSetsTabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(List.of(
                        LineModificationInfos.builder().equipmentId("line1")
                                .operationalLimitsGroups(List.of(
                                        OperationalLimitsGroupModificationInfos.builder()
                                                .id("DEFAULT")
                                                .applicability(OperationalLimitsGroupInfos.Applicability.SIDE1)
                                                .modificationType(OperationalLimitsGroupModificationType.MODIFY)
                                                .temporaryLimitsModificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                                .currentLimits(CurrentLimitsModificationInfos.builder()
                                                        .temporaryLimits(List.of(
                                                                CurrentTemporaryLimitModificationInfos.builder()
                                                                        .modificationType(TemporaryLimitModificationType.MODIFY_OR_ADD)
                                                                        // name33 already used by an existing limit (duration 33)
                                                                        .name(toAttributeModification("name33", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(50, OperationType.SET))
                                                                        .value(toAttributeModification(99., OperationType.SET))
                                                                        .build()
                                                        )).build())
                                                .build()
                                )).build()
                ))
                .build();

        ReportNode reportNode = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationInfos.toModification().apply(getNetwork(), reportNode);

        CurrentLimits limits = Objects.requireNonNull(getNetwork().getLine("line1")
                        .getOperationalLimitsGroup1("DEFAULT").orElse(null))
                .getCurrentLimits().orElse(null);
        assertNotNull(limits);
        // no new limit added at duration 50
        assertNull(limits.getTemporaryLimit(50));
        // existing name33 unchanged
        assertEquals(15.0, limits.getTemporaryLimit(33).getValue(), 0.01);
        assertLogMessageWithoutRank("Duplicate name or duration for temporary limit name33 (duration: 50) to MODIFY_OR_ADD: ignored",
                "network.modification.temporaryLimitsDuplicate", reportNode);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Line line1 = getNetwork().getLine("line1");
        CurrentLimits line1CurrentLimits = Objects.requireNonNull(getNetwork().getLine("line1").getOperationalLimitsGroup1("DEFAULT").orElse(null)).getCurrentLimits().orElse(null);
        Assertions.assertNotNull(line1CurrentLimits);
        assertEquals(2, line1CurrentLimits.getTemporaryLimits().size());
        assertEquals("test2_plus", line1CurrentLimits.getTemporaryLimit(1).getName());
        assertEquals("test1", line1CurrentLimits.getTemporaryLimit(2).getName());
        assertEquals(50, line1CurrentLimits.getTemporaryLimit(2).getValue());
        assertNull(line1.getSelectedOperationalLimitsGroupId1().orElse(null));

        Line line2 = getNetwork().getLine("line2");
        CurrentLimits line2DefaultCurrentLimitsSide1 = Objects.requireNonNull(line2.getOperationalLimitsGroup1("DEFAULT").orElse(null)).getCurrentLimits().orElse(null);
        Assertions.assertNotNull(line2DefaultCurrentLimitsSide1);
        assertEquals(1, line2DefaultCurrentLimitsSide1.getTemporaryLimits().size());
        assertEquals("test1", line2DefaultCurrentLimitsSide1.getTemporaryLimit(1).getName());
        assertEquals("DEFAULT", line2.getSelectedOperationalLimitsGroupId1().orElse(null));
        assertEquals("group0", line2.getSelectedOperationalLimitsGroupId2().orElse(null));
        CurrentLimits line2Group0CurrentLimitsSide2 = Objects.requireNonNull(line2.getOperationalLimitsGroup2("group0").orElse(null)).getCurrentLimits().orElse(null);
        Assertions.assertNotNull(line2Group0CurrentLimitsSide2);
        assertEquals(100, line2Group0CurrentLimitsSide2.getPermanentLimit());

        CurrentLimits line2CurrentLimitsSide2 = Objects.requireNonNull(line2.getOperationalLimitsGroup2("group0").orElse(null)).getCurrentLimits().orElse(null);
        Assertions.assertNotNull(line2CurrentLimitsSide2);
        assertEquals(1, line2CurrentLimitsSide2.getTemporaryLimits().size());
        assertEquals("test1", line2CurrentLimitsSide2.getTemporaryLimit(1).getName());
        assertEquals("group0", line2.getSelectedOperationalLimitsGroupId2().orElse(null));
    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogMessageWithoutRank("Limit set DEFAULT has been modified on side 1", "network.modification.operationalLimitsGroupModified", reportNode);
        assertLogMessageWithoutRank("Previous temporary limits were removed", "network.modification.temporaryLimitsReplaced", reportNode);
        assertLogMessageWithoutRank("Cannot add DEFAULT operational limit group, one with the given name already exists", "network.modification.tabular.modification.exception", reportNode);
        assertLogMessageWithoutRank("No existing temporary limit found with acceptableDuration = 3 matching is based on acceptableDuration if that helps", "network.modification.temporaryLimitsNoMatch", reportNode);
        assertLogMessageWithoutRank("limit set selected on side 2 : group0", "network.modification.limitSetSelectedOnSide2", reportNode);
        assertLogMessageWithoutRank("Limit set group0 has replaced the existing limit sets on side 2", "network.modification.operationalLimitsGroupReplaced", reportNode);
        assertLogMessageWithoutRank("Limit set DEFAULT added on side 1", "network.modification.operationalLimitsGroupAdded", reportNode);
    }

    @Override
    protected void checkModification() {
        // abstract method that has to be implemented even if there is nothing to check
    }
}
