/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchRuntimeException;
import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;
import static org.gridsuite.modification.utils.NetworkUtil.*;

/**
 * @author David SARTORI <david.sartori_externe at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class ModificationUtilsTest {

    @Test
    void testCheckMaxQGreaterThanMinQ() {
        var point1 = ReactiveCapabilityCurvePointsInfos.builder().minQ(10.0).maxQ(20.0).build();
        ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point1),
                MODIFY_GENERATOR_ERROR,
                "old KO, new OK: No exception should be thrown");

        var point2 = ReactiveCapabilityCurvePointsInfos.builder().minQ(20.0).maxQ(10.0).build();
        var exception = (NetworkModificationException) catchRuntimeException(() -> ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point2),
                MODIFY_GENERATOR_ERROR,
                "old OK, new KO: ")
        );
        assertThat(exception.getType()).isEqualTo(MODIFY_GENERATOR_ERROR);
        assertThat(exception)
                .hasMessageEndingWith("old OK, new KO: maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0");

        var point3 = ReactiveCapabilityCurvePointsInfos.builder().minQ(20.0).maxQ(10.0).build();
        exception = (NetworkModificationException) catchRuntimeException(() -> ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point3),
                MODIFY_GENERATOR_ERROR,
                "old null, new KO: ")
        );
        assertThat(exception.getType()).isEqualTo(MODIFY_GENERATOR_ERROR);
        assertThat(exception)
                .hasMessageEndingWith("old null, new KO: maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0");

        var point4 = ReactiveCapabilityCurvePointsInfos.builder().minQ(10.0).maxQ(20.0).build();
        ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point4),
                MODIFY_GENERATOR_ERROR,
                "old null, new OK: No exception should be thrown");

        var point5 = ReactiveCapabilityCurvePointsInfos.builder().minQ(10.0).maxQ(20.0).build();
        ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point5),
                MODIFY_GENERATOR_ERROR,
                "old OK, new null: No exception should be thrown");

        var point6 = ReactiveCapabilityCurvePointsInfos.builder().minQ(20.0).maxQ(10.0).build();
        exception = (NetworkModificationException) catchRuntimeException(() -> ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point6),
                MODIFY_GENERATOR_ERROR,
                "old KO, new null: ")
        );
        assertThat(exception.getType()).isEqualTo(MODIFY_GENERATOR_ERROR);
        assertThat(exception)
                .hasMessageEndingWith("old KO, new null: maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0");
    }

    @Test
    void testGetPosition() {
        Network network = NetworkCreation.create(UUID.randomUUID(), false);
        Substation s1 = network.getSubstation("s1");
        VoltageLevel vl = createVoltageLevel(s1, "VL1", "VL1", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(vl, "VL1.1", "VL1.1", 0);
        // first free position in a new voltage level must be 10
        Assertions.assertEquals(10, ModificationUtils.getInstance().getPosition("VL1.1", network, vl));

        // create load with first position (10)
        createSwitch(vl, "VL1load", "VL1load", SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createLoad(vl, "LOAD_VL1", null, 1, 0.0, 0.0, "feederName", 10, ConnectablePosition.Direction.UNDEFINED);

        // assert new first free position is 20
        Assertions.assertEquals(20, ModificationUtils.getInstance().getPosition("VL1.1", network, vl));

        // test in an existing voltage level
        // max position already used is 5 for trf6, first available +10 is 15
        Assertions.assertEquals(15, ModificationUtils.getInstance().getPosition("1.1", network, vl));
    }
}
