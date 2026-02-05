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
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.ModificationCreation;
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.utils.TestUtils.assertLogMessageWithoutRank;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class CompositeModificationsTest extends AbstractNetworkModificationTest {

    @Override
    public void checkModification() {
        Network network = getNetwork();
        CompositeModificationInfos compositeModificationInfos = (CompositeModificationInfos) buildModification();

        // regular throwing exception netmod
        GeneratorCreation throwingExceptionNetMod = (GeneratorCreation) buildThrowingModification().toModification();
        assertThrows(PowsyblException.class, () -> throwingExceptionNetMod.apply(network));
        // but doesn't throw once inside a composite modification
        ReportNode report = compositeModificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        compositeModificationInfos.setModifications(List.of(buildThrowingModification()));
        CompositeModification netmod = (CompositeModification) compositeModificationInfos.toModification();
        assertDoesNotThrow(() -> netmod.apply(network, report));
        // but the thrown message is inside the report :
        assertLogMessageWithoutRank(
                "Cannot execute GeneratorCreation : The network " + getNetwork().getId() + " already contains an object 'GeneratorImpl' with the id 'idGenerator'",
                "network.modification.compositeReportException",
                report
        );
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
                        .modifications(
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
                        .modifications(
                                List.of(
                                        CompositeModificationInfos.builder()
                                                .modifications(
                                                        List.of(ModificationCreation.getModificationGenerator("idGenerator", "other idGenerator name again"))
                                                ).build(),
                                        ModificationCreation.getModificationGenerator("idGenerator", "even newer idGenerator name")
                                )
                        ).build()
        );
        return CompositeModificationInfos.builder()
                .modifications(modifications)
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

    // TODO : tester les messages, en tout cas celui de la composite avec son nom
}
