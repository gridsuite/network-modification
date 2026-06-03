/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.OperationalLimitsGroup;
import com.powsybl.iidm.network.ValidationException;

import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability.SIDE1;
import static org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability.SIDE2;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class LineCreationInBusBreakerTest extends AbstractNetworkModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final String PROP1_NAME = "prop1";
    private static final String PROP2_NAME = "prop2";
    private static final String PROP1_VALUE = "value1";
    private static final String PROP2_VALUE = "value2";

    @Override
    protected void checkModification() {
        LineCreationModel lineCreationModel = (LineCreationModel) buildModification();
        lineCreationModel.setBusOrBusbarSectionId2("notFoundBus");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> lineCreationModel.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                exception.getMessage());
    }

    @Test
    void testCreateLineOptionalParameters5() throws Exception {
        LineCreationModel lineCreationModelPermanentLimitNOK = LineCreationModel.builder()
                .stashed(false)
                .equipmentId("idLine2")
                .equipmentName("nameLine2")
                .r(100.0)
                .x(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .operationalLimitsGroups(
                    List.of(
                        OperationalLimitsGroupModel.builder()
                            .id("limiSet1")
                            .currentLimits(
                                CurrentLimitsModel.builder().permanentLimit(-1.0).build()
                        ).applicability(SIDE1).build()
                    )
                )
                .selectedOperationalLimitsGroupId1("limiSet1")
                .build();
        ValidationException exception = assertThrows(ValidationException.class, () -> lineCreationModelPermanentLimitNOK.toModification().apply(getNetwork()));
        assertEquals("AC line 'idLine2': permanent limit must be >= 0", exception.getMessage());
    }

    @Test
    void testApplySelectedLimitsGroupsNotExist() {
        ModificationModel modification = buildModificationWithInvalidSelectedLimitGroups();
        modification.toModification().apply(getNetwork());
        assertEquals("", getNetwork().getLine("idLine1").getSelectedOperationalLimitsGroupId1().orElse(""));
        assertEquals("", getNetwork().getLine("idLine1").getSelectedOperationalLimitsGroupId2().orElse(""));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationModel buildModification() {
        return buildModification("limitSet1", "limitSet2");
    }

    private ModificationModel buildModification(String selectedLimitGroups1, String selectedLimitGroups2) {
        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "bus1" and
        //         voltage level "v2" and busbar section "bus2"
        return LineCreationModel.builder()
            .stashed(false)
            .equipmentId("idLine1")
            .equipmentName("nameLine1")
            .r(100.0)
            .x(100.0)
            .g1(10.0)
            .b1(10.0)
            .g2(20.0)
            .b2(20.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("bus1")
            .operationalLimitsGroups(
                List.of(
                    OperationalLimitsGroupModel.builder()
                        .id("limitSet1")
                        .currentLimits(
                            CurrentLimitsModel.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build()
                        ).applicability(SIDE1).build(),
                    OperationalLimitsGroupModel.builder()
                        .id("limitSet2")
                        .currentLimits(
                            CurrentLimitsModel.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build()
                        ).applicability(SIDE2).build()
                )
            )
            .selectedOperationalLimitsGroupId1(selectedLimitGroups1)
            .selectedOperationalLimitsGroupId2(selectedLimitGroups2)
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .lineSegments(List.of(new LineSegmentModel(UUID.randomUUID().toString(), 1, "1", "50", null),
                new LineSegmentModel(UUID.randomUUID().toString(), 1, "1", null, 0.95)))
            .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Test
    void testCreateLimitsProperties() {
        LineCreationModel modificationModel = (LineCreationModel) buildModification();
        modificationModel.setOperationalLimitsGroups(List.of(
            OperationalLimitsGroupModel.builder()
                .id("newLimit")
                .applicability(SIDE1)
                .limitsProperties(List.of(new LimitsPropertyModel(PROP1_NAME, PROP1_VALUE),
                    new LimitsPropertyModel(PROP2_NAME, PROP2_VALUE)))
                .currentLimits(CurrentLimitsModel.builder().permanentLimit(10.0)
                    .build())
                .build()));

        modificationModel.toModification().apply(getNetwork());
        Line line = getNetwork().getLine("idLine1");
        assertNotNull(line);
        Optional<OperationalLimitsGroup> limitSet = line.getOperationalLimitsGroup1("newLimit");
        assertTrue(limitSet.isPresent());
        Set<String> propertiesName = limitSet.get().getPropertyNames();
        assertEquals(2, propertiesName.size());
        assertTrue(propertiesName.contains(PROP1_NAME));
        assertTrue(propertiesName.contains(PROP2_NAME));
        assertEquals(PROP1_VALUE, limitSet.get().getProperty(PROP1_NAME));
        assertEquals(PROP2_VALUE, limitSet.get().getProperty(PROP2_NAME));
    }

    @Test
    void testCreateLimitsPropertiesWithDuplicates() {
        LineCreationModel modificationModel = (LineCreationModel) buildModification();
        modificationModel.setOperationalLimitsGroups(List.of(
            OperationalLimitsGroupModel.builder()
                .id("newLimit")
                .applicability(SIDE1)
                .limitsProperties(List.of(new LimitsPropertyModel(PROP1_NAME, PROP1_VALUE),
                    new LimitsPropertyModel(PROP1_NAME, PROP2_VALUE)))
                .currentLimits(CurrentLimitsModel.builder().permanentLimit(10.0)
                    .build())
                .build()));
        modificationModel.toModification().apply(getNetwork());
        Line line = getNetwork().getLine("idLine1");
        assertNotNull(line);
        Optional<OperationalLimitsGroup> limitSet = line.getOperationalLimitsGroup1("newLimit");
        assertTrue(limitSet.isPresent());

        // If there are duplicates properties are not created
        Set<String> propertiesName = limitSet.get().getPropertyNames();
        assertEquals(0, propertiesName.size());
    }

    private ModificationModel buildModificationWithInvalidSelectedLimitGroups() {
        return buildModification("invalid1", "invalid2");
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getLine("idLine1"));
        assertEquals(PROPERTY_VALUE, getNetwork().getLine("idLine1").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("LINE_CREATION", modificationModel.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationModel.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLine1", createdValues.get("equipmentId"));
    }
}
