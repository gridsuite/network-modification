/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;

import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.model.CompositeModificationModel;
import org.gridsuite.modification.model.GeneratorCreationModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationCreation;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class CompositeModificationsTest extends AbstractNetworkModificationTest {

    @Override
    public void checkModification() {
        // nothing to check here
    }

    @Test
    void checkCompositeExecutionDepth() {
        Network network = getNetwork();
        CompositeModificationModel compositeModificationModel = (CompositeModificationModel) buildModification();

        // checks that the sub sub sub netmod is executed at the right depth
        ReportNode report = compositeModificationModel.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        CompositeModification netmod = (CompositeModification) compositeModificationModel.toModification();
        assertDoesNotThrow(() -> netmod.apply(network, report));
        assertLogMessageAtDepth(
                "Generator with id=idGenerator modified :",
                "network.modification.generatorModification",
                report,
                4
        );
        assertLogMessageAtDepth(
                "Composite modification : 'sub sub composite'",
                "network.modification.composite.apply",
                report,
                2
        );
    }

    @Test
    void checkCompositeExecutionErrorHandling() {
        Network network = getNetwork();
        CompositeModificationModel compositeModificationModel = (CompositeModificationModel) buildModification();

        ReportNode report = compositeModificationModel.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        // regular throwing exception netmod
        GeneratorCreation throwingExceptionNetMod = (GeneratorCreation) buildThrowingModification().toModification();
        assertThrows(PowsyblException.class, () -> throwingExceptionNetMod.apply(network));
        // but doesn't throw once inside a composite modification
        compositeModificationModel.setModificationsInfos(List.of(buildThrowingModification()));
        CompositeModification netmodContainingError = (CompositeModification) compositeModificationModel.toModification();
        assertDoesNotThrow(() -> netmodContainingError.apply(network, report));
        // but the thrown message is inside the report :
        assertLogMessageWithoutRank(
                "Cannot execute GeneratorCreation : GENERATOR_ALREADY_EXISTS : idGenerator",
                "network.modification.composite.exception.report",
                report
        );

    }

    @Test
    void checkCompositeFiltersDeactivatedAndStashedModifications() {
        Network network = getNetwork();
        ModificationModel renameModif = ModificationCreation.getModificationGenerator("idGenerator", "baseline name");
        renameModif.setActivated(true);
        renameModif.setStashed(false);

        ModificationModel deactivatedRenameModif = ModificationCreation.getModificationGenerator("idGenerator", "deactivated name");
        deactivatedRenameModif.setActivated(false);
        deactivatedRenameModif.setStashed(false);

        ModificationModel stashedRenameModif = ModificationCreation.getModificationGenerator("idGenerator", "stashed name");
        stashedRenameModif.setActivated(true);
        stashedRenameModif.setStashed(true);

        ModificationModel invalidModif = ModificationCreation.getModificationGenerator("idGenerator", "null activated name");
        invalidModif.setActivated(null);
        invalidModif.setStashed(null);

        CompositeModificationModel composite = CompositeModificationModel.builder()
                .name("filter test composite")
                .modificationsInfos(List.of(renameModif, deactivatedRenameModif, stashedRenameModif, invalidModif))
                .stashed(false)
                .build();

        ReportNode report = composite.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());

        CompositeModification netmod = (CompositeModification) composite.toModification();
        assertDoesNotThrow(() -> netmod.apply(network, report));

        // Only the baseline rename (activated=true, stashed=false) should have been applied;
        // the deactivated, stashed, and null-activated renames must all have been skipped.
        Generator gen = network.getGenerator("idGenerator");
        assertNotNull(gen);
        assertEquals("baseline name", gen.getOptionalName().orElseThrow());
    }

    private GeneratorCreationModel buildThrowingModification() {
        return ModificationCreation.getCreationGenerator(
                "v1", "idGenerator", "nameGenerator", "1B", "v2load", "LOAD", "v1"
        );
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, false);
    }

    @Override
    protected ModificationModel buildModification() {
        List<ModificationModel> modifications = List.of(
                CompositeModificationModel.builder()
                        .activated(true)
                        .name("sub composite 1")
                        .modificationsInfos(
                                List.of(
                                        ModificationCreation.getModificationGenerator("idGenerator", "other idGenerator name"),
                                        // this should throw an error but not stop the execution of the composite modification and all the other content
                                        buildThrowingModification()
                                )
                        ).build(),
                ModificationCreation.getModificationGenerator("idGenerator", "new idGenerator name"),
                ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED),
                ModificationCreation.getCreationBattery("v1", "idBattery", "nameBattery", "1.1"),
                // test of a composite modification inside a composite modification inside a composite modification
                CompositeModificationModel.builder()
                        .activated(true)
                        .name("sub composite 2")
                        .modificationsInfos(
                                List.of(
                                        CompositeModificationModel.builder()
                                                .activated(true)
                                                .name("sub sub composite")
                                                .modificationsInfos(
                                                        List.of(ModificationCreation.getModificationGenerator("idGenerator", "other idGenerator name again"))
                                                ).build(),
                                        ModificationCreation.getModificationGenerator("idGenerator", "even newer idGenerator name")
                                )
                        ).build()
        );
        return CompositeModificationModel.builder()
                .name("main composite")
                .modificationsInfos(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        Generator gen = getNetwork().getGenerator("idGenerator");
        assertNotNull(gen);
        assertEquals("even newer idGenerator name", gen.getOptionalName().orElseThrow());
        assertNotNull(getNetwork().getLoad("idLoad"));
        assertNotNull(getNetwork().getBattery("idBattery"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationModel.getMessageType());
    }
}
