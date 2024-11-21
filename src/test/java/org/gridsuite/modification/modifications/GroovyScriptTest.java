/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.GroovyScriptInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.NetworkCreation;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.GROOVY_SCRIPT_EMPTY;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class GroovyScriptTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return GroovyScriptInfos.builder()
                .stashed(false)
                .script("network.getGenerator('idGenerator').targetP=12\n")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(12, getNetwork().getGenerator("idGenerator").getTargetP(), 0);
    }

    @Override
    protected void checkModification() {
        GroovyScriptInfos groovyScriptInfos = (GroovyScriptInfos) buildModification();
        groovyScriptInfos.setScript("");
        // apply empty groovy script
        Exception exception = assertThrows(NetworkModificationException.class, () -> groovyScriptInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage(),
                exception.getMessage());

        groovyScriptInfos.setScript("      ");
        // apply blank groovy script
        exception = assertThrows(NetworkModificationException.class, () -> groovyScriptInfos.toModification().check(getNetwork()));
        assertEquals(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage(),
                exception.getMessage());

        groovyScriptInfos.setScript("network.getGenerator('there is no generator').targetP=12\n");
        // apply groovy script with unknown generator
        exception = assertThrows(Exception.class, () -> groovyScriptInfos.toModification().apply(getNetwork()));
        assertEquals("Cannot set property 'targetP' on null object",
                exception.getMessage());
    }
}
