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
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

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
                                .selectedOperationalLimitsGroup1(new AttributeModification<>("", OperationType.UNSET))
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
                                .selectedOperationalLimitsGroup1(new AttributeModification<>("DEFAULT", OperationType.SET))
                                .selectedOperationalLimitsGroup2(new AttributeModification<>("", OperationType.UNSET))
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
                                .selectedOperationalLimitsGroup1(new AttributeModification<>("DEFAULT", OperationType.SET))
                                .selectedOperationalLimitsGroup2(new AttributeModification<>("DEFAULT", OperationType.SET))
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
                                .selectedOperationalLimitsGroup2(new AttributeModification<>("group0", OperationType.SET))
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
                                .selectedOperationalLimitsGroup2(new AttributeModification<>("group0", OperationType.SET))
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
                                .selectedOperationalLimitsGroup2(new AttributeModification<>("group0", OperationType.SET))
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
    protected void testModifyTemporaryLimitNameOnly() {
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
    protected void testModifyTemporaryLimitValueOnly() {
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
                                                                        .name(null) // No name modification
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
     * Test modifying temporary limit with null value (MAX_VALUE scenario)
     */
    @Test
    protected void testModifyTemporaryLimitWithNullValue() {
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
                                                                        .name(toAttributeModification("limit_no_value", OperationType.SET))
                                                                        .acceptableDuration(null) // Will be Integer.MAX_VALUE
                                                                        .value(null) // Will be Double.MAX_VALUE
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
        assertNotNull(limits.getTemporaryLimit(Integer.MAX_VALUE));
    }

    /**
     * Test DELETE modification type with isThisLimitDeleted check
     */
    @Test
    protected void testDeleteTemporaryLimit() {
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
    protected void testModifyOrAddNonExistentLimit() {
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
    protected void testModifyWithNoActualChanges() {
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
        assertLogMessageWithoutRank("Limit set group0 has been replaced on side 2", "network.modification.operationalLimitsGroupReplaced", reportNode);
        assertLogMessageWithoutRank("Limit set DEFAULT added on side 1", "network.modification.operationalLimitsGroupAdded", reportNode);
    }

    /**
     * Test creating temporary limit with createTemporaryLimit method
     */
    @Test
    protected void testCreateTemporaryLimit() {
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
                                                                        .name(toAttributeModification("created_limit", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(10, OperationType.SET))
                                                                        .value(toAttributeModification(500., OperationType.SET))
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
        assertNotNull(limits.getTemporaryLimit(10));
        assertEquals("created_limit", limits.getTemporaryLimit(10).getName());
        assertEquals(500., limits.getTemporaryLimit(10).getValue(), 0.01);
    }

    /**
     * Test modifying both name and value simultaneously
     */
    @Test
    protected void testModifyBothNameAndValue() {
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
                                                                        .name(toAttributeModification("both_changed", OperationType.SET))
                                                                        .acceptableDuration(toAttributeModification(32, OperationType.SET))
                                                                        .value(toAttributeModification(777., OperationType.SET))
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
        assertEquals("both_changed", limits.getTemporaryLimit(32).getName());
        assertEquals(777., limits.getTemporaryLimit(32).getValue(), 0.01);
    }

    @Override
    protected void checkModification() {
        // abstract method that has to be implemented even if there is nothing to check
    }
}
