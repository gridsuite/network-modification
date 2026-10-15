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
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationCreation;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.error.NetworkModificationExceptionType.GENERATOR_ALREADY_EXISTS;
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
        CompositeModificationInfos compositeModificationInfos = (CompositeModificationInfos) buildModification();

        // checks that the sub sub sub netmod is executed at the right depth
        ReportNode report = compositeModificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        CompositeModification netmod = (CompositeModification) compositeModificationInfos.toModification(null);
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
        CompositeModificationInfos compositeModificationInfos = (CompositeModificationInfos) buildModification();

        ReportNode report = compositeModificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        // regular throwing exception netmod
        GeneratorCreation throwingExceptionNetMod = (GeneratorCreation) buildThrowingModification().toModification(null);
        assertThrows(PowsyblException.class, () -> throwingExceptionNetMod.apply(network));
        // but doesn't throw once inside a composite modification
        compositeModificationInfos.setModificationsInfos(List.of(buildThrowingModification()));
        CompositeModification netmodContainingError = (CompositeModification) compositeModificationInfos.toModification(null);
        assertDoesNotThrow(() -> netmodContainingError.apply(network, report));
        // but the thrown message is inside the report :
        assertLogMessageWithoutRank(
                "Cannot execute GENERATOR_CREATION : " + GENERATOR_ALREADY_EXISTS.getMessage() + " : idGenerator",
                "network.modification.composite.exception.report",
                report
        );

    }

    @Test
    void checkCompositeExecutionReportsErrorAndContinues() {
        Network network = getNetwork();
        // Balances Adjustment uses a context to resolve its dependencies (lf params), which may fail during toModification(ctx) call.
        // In this test, we create such a modification without context, so that its dependencies cannot be resolved (throw expected in apply, but not in composite apply flow)
        BalancesAdjustmentModificationInfos balancesAdjustmentWithoutContext = BalancesAdjustmentModificationInfos.builder()
                .areas(List.of())
                .withLoadFlow(true)
                .loadFlowParametersId(UUID.randomUUID())
                .activated(true)
                .build();
        ModificationInfos generatorRename = ModificationCreation.getModificationGenerator("idGenerator", "successfully rename");
        generatorRename.setActivated(true);

        CompositeModificationInfos composite = CompositeModificationInfos.builder()
                .name("Composite including a failure")
                // contains 2 modifications: first will fail, second must succeed
                .modificationsInfos(List.of(balancesAdjustmentWithoutContext, generatorRename))
                .build();

        // execution
        ReportNode report = composite.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        CompositeModification netmod = (CompositeModification) composite.toModification(ModificationContext.empty());
        assertDoesNotThrow(() -> netmod.apply(network, report));

        assertLogMessageWithoutRank(
                "Cannot execute BALANCES_ADJUSTMENT_MODIFICATION : This modification requires a load flow parameters loader, none was provided in the modification context",
                "network.modification.composite.exception.report",
                report
        );
        assertLogMessageWithoutRank(
                "Generator with id=idGenerator modified :",
                "network.modification.generatorModification",
                report
        );
        assertEquals("successfully rename", network.getGenerator("idGenerator").getOptionalName().orElseThrow());
    }

    @Test
    void checkCompositeFiltersDeactivatedAndStashedModifications() {
        Network network = getNetwork();
        ModificationInfos renameModif = ModificationCreation.getModificationGenerator("idGenerator", "baseline name");
        renameModif.setActivated(true);
        renameModif.setStashed(false);

        ModificationInfos deactivatedRenameModif = ModificationCreation.getModificationGenerator("idGenerator", "deactivated name");
        deactivatedRenameModif.setActivated(false);
        deactivatedRenameModif.setStashed(false);

        ModificationInfos stashedRenameModif = ModificationCreation.getModificationGenerator("idGenerator", "stashed name");
        stashedRenameModif.setActivated(true);
        stashedRenameModif.setStashed(true);

        ModificationInfos invalidModif = ModificationCreation.getModificationGenerator("idGenerator", "null activated name");
        invalidModif.setActivated(null);
        invalidModif.setStashed(null);

        CompositeModificationInfos composite = CompositeModificationInfos.builder()
                .name("filter test composite")
                .modificationsInfos(List.of(renameModif, deactivatedRenameModif, stashedRenameModif, invalidModif))
                .stashed(false)
                .build();

        ReportNode report = composite.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());

        CompositeModification netmod = (CompositeModification) composite.toModification(null);
        assertDoesNotThrow(() -> netmod.apply(network, report));

        // Only the baseline rename (activated=true, stashed=false) should have been applied;
        // the deactivated, stashed, and null-activated renames must all have been skipped.
        Generator gen = network.getGenerator("idGenerator");
        assertNotNull(gen);
        assertEquals("baseline name", gen.getOptionalName().orElseThrow());
    }

    private GeneratorCreationInfos buildThrowingModification() {
        return ModificationCreation.getCreationGenerator(
                "v1", "idGenerator", "nameGenerator", "1B", "v2load", "LOAD", "v1"
        );
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, false);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                CompositeModificationInfos.builder()
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
                CompositeModificationInfos.builder()
                        .activated(true)
                        .name("sub composite 2")
                        .modificationsInfos(
                                List.of(
                                        CompositeModificationInfos.builder()
                                                .activated(true)
                                                .name("sub sub composite")
                                                .modificationsInfos(
                                                        List.of(ModificationCreation.getModificationGenerator("idGenerator", "other idGenerator name again"))
                                                ).build(),
                                        ModificationCreation.getModificationGenerator("idGenerator", "even newer idGenerator name")
                                )
                        ).build()
        );
        return CompositeModificationInfos.builder()
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
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }
}
