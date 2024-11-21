/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Injection;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.InjectionModificationInfos;
import org.gridsuite.modification.dto.OperationType;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
abstract class AbstractInjectionModificationTest extends AbstractNetworkModificationTest {
    protected void assertChangeConnectionState(Injection<?> existingEquipment, boolean expectedState) throws Exception {
        assertChangeConnectionState(existingEquipment, (InjectionModificationInfos) buildModification(), expectedState);
    }

    protected void assertChangeConnectionState(Injection<?> existingEquipment, InjectionModificationInfos modificationInfos, boolean expectedState) throws Exception {
        modificationInfos.setTerminalConnected(new AttributeModification<>(expectedState, OperationType.SET));

        if (expectedState) {
            if (existingEquipment.getTerminal().isConnected()) {
                existingEquipment.getTerminal().disconnect();
            }
        } else {
            if (!existingEquipment.getTerminal().isConnected()) {
                existingEquipment.getTerminal().connect();
            }
        }
        assertThat(existingEquipment.getTerminal().isConnected()).isNotEqualTo(expectedState);

        modificationInfos.toModification().apply(getNetwork());
        // connection state has changed as expected
        assertThat(existingEquipment.getTerminal().isConnected()).isEqualTo(expectedState);

        // try to modify again => no change on connection state
        modificationInfos.toModification().apply(getNetwork());
        assertThat(existingEquipment.getTerminal().isConnected()).isEqualTo(expectedState);
    }
}
