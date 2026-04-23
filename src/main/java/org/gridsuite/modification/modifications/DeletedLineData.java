/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.OperationalLimitsGroup;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * @author Etienne LESOT <etienne.lesot at rte-france.com>
 */
public class DeletedLineData {
    @Getter
    private final Collection<OperationalLimitsGroup> limitsOnSide1;
    @Getter
    private final Collection<OperationalLimitsGroup> limitsOnSide2;
    private final String selectedLimitGroupOnSide1;
    private final String selectedLimitGroupOnSide2;
    @Getter
    private final String lineId;

    public DeletedLineData(Line line) {
        limitsOnSide1 = List.copyOf(line.getOperationalLimitsGroups1());
        limitsOnSide2 = List.copyOf(line.getOperationalLimitsGroups2());
        selectedLimitGroupOnSide1 = line.getSelectedOperationalLimitsGroupId1().orElse(null);
        selectedLimitGroupOnSide2 = line.getSelectedOperationalLimitsGroupId2().orElse(null);
        lineId = line.getId();
    }

    public Optional<String> getSelectedLimitGroupOnSide1() {
        return Optional.ofNullable(selectedLimitGroupOnSide1);
    }

    public Optional<String> getSelectedLimitGroupOnSide2() {
        return Optional.ofNullable(selectedLimitGroupOnSide2);
    }
}
