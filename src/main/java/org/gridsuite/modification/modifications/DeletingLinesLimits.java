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
import java.util.Optional;

/**
 * @author Etienne LESOT <etienne.lesot at rte-france.com>
 */
public class DeletingLinesLimits {
    @Getter
    private final Collection<OperationalLimitsGroup> line1Side1Limits;
    @Getter
    private final Collection<OperationalLimitsGroup> line1Side2Limits;
    @Getter
    private final Collection<OperationalLimitsGroup> line2Side1Limits;
    @Getter
    private final Collection<OperationalLimitsGroup> line2Side2Limits;
    private final String selectedLimitGroupLine1Side1;
    private final String selectedLimitGroupLine1Side2;
    private final String selectedLimitGroupLine2Side1;
    private final String selectedLimitGroupLine2Side2;
    @Getter
    private final String line1Id;
    @Getter
    private final String line2Id;

    public DeletingLinesLimits(Line line1, Line line2) {
        line1Side1Limits = line1.getOperationalLimitsGroups1();
        line1Side2Limits = line1.getOperationalLimitsGroups2();
        line2Side1Limits = line2.getOperationalLimitsGroups1();
        line2Side2Limits = line2.getOperationalLimitsGroups2();
        selectedLimitGroupLine1Side1 = line1.getSelectedOperationalLimitsGroupId1().orElse(null);
        selectedLimitGroupLine1Side2 = line1.getSelectedOperationalLimitsGroupId2().orElse(null);
        selectedLimitGroupLine2Side1 = line2.getSelectedOperationalLimitsGroupId1().orElse(null);
        selectedLimitGroupLine2Side2 = line2.getSelectedOperationalLimitsGroupId2().orElse(null);
        line1Id = line1.getId();
        line2Id = line2.getId();
    }

    public Optional<String> getSelectedLimitGroupLine1Side1() {
        return Optional.ofNullable(selectedLimitGroupLine1Side1);
    }

    public Optional<String> getSelectedLimitGroupLine1Side2() {
        return Optional.ofNullable(selectedLimitGroupLine1Side2);
    }

    public Optional<String> getSelectedLimitGroupLine2Side1() {
        return Optional.ofNullable(selectedLimitGroupLine2Side1);
    }

    public Optional<String> getSelectedLimitGroupLine2Side2() {
        return Optional.ofNullable(selectedLimitGroupLine2Side2);
    }
}
