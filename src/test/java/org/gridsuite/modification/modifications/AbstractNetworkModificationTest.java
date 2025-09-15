/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkImpl;

import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.utils.DummyNamingStrategy;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.UUID;

/**
 * Class to extend if you want to test a network modification.<ul>
 * <li>Each modification should have its own class and implements the abstract methods.</li>
 * <li>It will automatically run the tests present in this class with the implemented methods.</li>
 * <li>If you want to add a test that can be applied to every modification, add it here.</li>
 * <li>If you want to add a test specific to a modification, add it in its own class.</li>
 * </ul>
 */
/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public abstract class AbstractNetworkModificationTest {

    private static final UUID TEST_NETWORK_ID = UUID.randomUUID();

    private Network network;

    private NamingStrategy namingStrategy = new DummyNamingStrategy();

    private ReportNode rootReportNode = ReportNode.newRootReportNode().withMessageTemplate("apply").build();

    @Autowired
    protected ObjectMapper mapper;

    @BeforeEach
    public void setUp() {
        network = createNetwork(TEST_NETWORK_ID);
    }

    @Test
    public void testApply() throws Exception {
        buildModification().toModification().apply(network, namingStrategy, rootReportNode);
        assertAfterNetworkModificationApplication();
    }

    @Test
    public void testCheck() {
        checkModification();
    }

    protected Network getNetwork() {
        return network;
    }

    protected void setNetwork(Network network) {
        this.network = network;
    }

    protected UUID getNetworkId() {
        return TEST_NETWORK_ID;
    }

    protected UUID getNetworkUuid() {
        return ((NetworkImpl) network).getUuid();
    }

    protected abstract Network createNetwork(UUID networkUuid);

    protected abstract ModificationInfos buildModification();

    protected abstract void assertAfterNetworkModificationApplication();

    protected abstract void checkModification();

    @SuppressWarnings("java:S1130") // Exceptions are throws by overrides
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("{}", modificationInfos.getMessageValues());
    }
}
