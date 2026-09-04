/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.powsybl.commons.report.PowsyblCoreReportResourceBundle;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.report.FilterReportResourceBundle;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.FilterLoader;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.utils.TestUtils.testReportNode;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

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

    private String reportFilePath;

    protected ObjectMapper mapper = new ObjectMapper().registerModule(new JavaTimeModule());

    @BeforeEach
    public void setUp() {
        network = createNetwork(TEST_NETWORK_ID);
        reportFilePath = getReportFilePath();
    }

    @Test
    public void testApply() throws Exception {
        ModificationInfos modificationInfos = buildModification();
        ReportNode report = modificationInfos.createSubReportNode(ReportNode.newRootReportNode()
                .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME, FilterReportResourceBundle.BASE_NAME, PowsyblCoreReportResourceBundle.BASE_NAME)
                .withMessageTemplate("test")
                .build());
        AbstractModification modification = modificationInfos.toModification();
        modification.check(network);
        initApplicationContext(modification);
        modification.apply(network, report);
        assertAfterNetworkModificationApplication();
        if (reportFilePath != null) {
            testReportNode(report, reportFilePath);
        }
    }

    protected void initApplicationContext(AbstractModification modification) {
        // Nothing to init by default
    }

    public final List<Filter> loadFilters(List<UUID> filterUuids) {
        if (getFilterLoader() != null) {
            return getFilterLoader().load(filterUuids);
        }
        return List.of();
    }

    public FilterLoader getFilterLoader() {
        return null;
    }

    @Test
    public void testCheck() {
        checkModification();
    }

    @Test
    public void testRoundTripSerializationDeserialization() throws JsonProcessingException {
        ILoadFlowService loadFlowServiceMock = mock(ILoadFlowService.class);
        IFilterService filterServiceMock = mock(IFilterService.class);
        AbstractModification expectedModification = buildModification().toModification(this::loadFilters);
        expectedModification.initApplicationContext(filterServiceMock, loadFlowServiceMock, null);

        String serializedModification = mapper.writeValueAsString(expectedModification);
        AbstractModification deserializedModification = mapper.readValue(serializedModification, AbstractModification.class);

        assertThat(deserializedModification).isInstanceOf(expectedModification.getClass())
                .isEqualTo(expectedModification);
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

    protected abstract Network createNetwork(UUID networkUuid);

    protected abstract ModificationInfos buildModification();

    protected abstract void assertAfterNetworkModificationApplication();

    protected abstract void checkModification();

    @SuppressWarnings("java:S1130") // Exceptions are throws by overrides
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("{}", modificationInfos.getMessageValues());
    }

    protected String getReportFilePath() {
        return null;
    }
}
