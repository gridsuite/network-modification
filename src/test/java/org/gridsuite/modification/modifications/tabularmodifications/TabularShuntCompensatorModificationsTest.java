/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.tabularmodifications;

import com.powsybl.commons.report.ReportConstants;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.model.constants.OperationType;
import org.gridsuite.modification.model.constants.ShuntCompensatorType;
import org.gridsuite.modification.model.tabular.TabularModificationModel;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.modifications.tabular.TabularModification;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.gridsuite.modification.utils.TestUtils.assertLogNthMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

/**
 * @author SARTORI David <david.sartori_externe@rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TabularShuntCompensatorModificationsTest extends AbstractNetworkModificationTest {

    @Mock
    private Network network;

    @Mock
    private ShuntCompensator shuntCompensator;

    @BeforeEach
    void specificSetUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationModel buildModification() {
        List<ModificationModel> modifications = List.of(
                ShuntCompensatorModificationModel.builder().equipmentId("v2shunt").maximumSectionCount(new AttributeModification<>(100, OperationType.SET)).sectionCount(new AttributeModification<>(10, OperationType.SET)).build(),
                ShuntCompensatorModificationModel.builder().equipmentId("v5shunt").maximumSectionCount(new AttributeModification<>(200, OperationType.SET)).sectionCount(new AttributeModification<>(20, OperationType.SET)).build()
        );
        return TabularModificationModel.builder()
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
                .modifications(modifications)
                .build();
    }

    @Test
    @Override
    public void testApply() {
        ModificationModel modificationModel = buildModification();
        ReportNode reportNode = modificationModel.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test").build());
        modificationModel.toModification().apply(getNetwork(), reportNode);
        assertAfterNetworkModificationApplication(reportNode);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(100, getNetwork().getShuntCompensator("v2shunt").getMaximumSectionCount());
        assertEquals(10, getNetwork().getShuntCompensator("v2shunt").getSectionCount());
        assertEquals(200, getNetwork().getShuntCompensator("v5shunt").getMaximumSectionCount());
        assertEquals(20, getNetwork().getShuntCompensator("v5shunt").getSectionCount());
    }

    private void assertAfterNetworkModificationApplication(ReportNode reportNode) {
        assertAfterNetworkModificationApplication();
        assertLogNthMessage("Modification of v2shunt", "network.modification.tabular.modification.equipmentId", reportNode, 1);
        assertLogNthMessage("Modification of v5shunt", "network.modification.tabular.modification.equipmentId", reportNode, 2);
        assertLogMessageWithoutRank("Tabular modification: 2 shunt compensators have been modified", "network.modification.tabular.modification", reportNode);
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationModel.getType().toString());
        Map<String, String> createdValues = modificationModel.getMapMessageValues();
        assertEquals(ModificationType.SHUNT_COMPENSATOR_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Test
    void testCheckModificationConflict() {
        var shuntModification = ShuntCompensatorModificationModel
                .builder()
                .equipmentId("id")
                .maxQAtNominalV(AttributeModification.toAttributeModification(1.0, OperationType.SET))
                .maxSusceptance(AttributeModification.toAttributeModification(10.0, OperationType.SET))
                .build();

        var tabularModificationModel = TabularModificationModel
                .builder()
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
                .modifications(Collections.singletonList(shuntModification))
                .build();

        var tabularModification = (TabularModification) tabularModificationModel.toModification();

        when(network.getShuntCompensator("id")).thenReturn(shuntCompensator);
        when(shuntCompensator.getModelType()).thenReturn(ShuntCompensatorModelType.LINEAR);
        when(shuntCompensator.getId()).thenReturn("id");
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build();

        tabularModification.specificCheck(shuntModification, network, reportNode);

        shuntModification.setShuntCompensatorType(AttributeModification.toAttributeModification(ShuntCompensatorType.CAPACITOR, OperationType.SET));
        tabularModification.specificCheck(shuntModification, network, reportNode);

        shuntModification.setMaxQAtNominalV(null);
        tabularModification.specificCheck(shuntModification, network, reportNode);

        assertEquals(TypedValue.WARN_SEVERITY, reportNode.getChildren().get(0).getValues().get(ReportConstants.SEVERITY_KEY));
    }

    @Test
    void testCheckModificationNonLinear() {
        var shuntModification = ShuntCompensatorModificationModel
                .builder()
                .equipmentId("id")
                .maxQAtNominalV(AttributeModification.toAttributeModification(1.0, OperationType.SET))
                .build();

        var tabularModificationModel = TabularModificationModel
                .builder()
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
                .modifications(Collections.singletonList(shuntModification))
                .build();

        var tabularModification = (TabularModification) tabularModificationModel.toModification();

        when(network.getShuntCompensator("id")).thenReturn(shuntCompensator);
        when(shuntCompensator.getModelType()).thenReturn(ShuntCompensatorModelType.NON_LINEAR);
        when(shuntCompensator.getId()).thenReturn("id");

        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build();
        tabularModification.specificCheck(shuntModification, network, reportNode);

        assertEquals(TypedValue.ERROR_SEVERITY, reportNode.getChildren().get(0).getValues().get(ReportConstants.SEVERITY_KEY));

    }

    @Test
    void testCheckModificationOK() {
        var shuntModification = ShuntCompensatorModificationModel
                .builder()
                .equipmentId("id")
                .maxQAtNominalV(AttributeModification.toAttributeModification(1.0, OperationType.SET))
                .build();

        var tabularModificationModel = TabularModificationModel
                .builder()
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
                .modifications(Collections.singletonList(shuntModification))
                .build();

        var tabularModification = (TabularModification) tabularModificationModel.toModification();

        when(network.getShuntCompensator("id")).thenReturn(shuntCompensator);
        when(shuntCompensator.getModelType()).thenReturn(ShuntCompensatorModelType.LINEAR);
        when(shuntCompensator.getId()).thenReturn("id");
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build();

        tabularModification.specificCheck(shuntModification, network, reportNode);
        assertEquals(0, reportNode.getChildren().size());
    }

    @Override
    protected void checkModification() {
    }
}
