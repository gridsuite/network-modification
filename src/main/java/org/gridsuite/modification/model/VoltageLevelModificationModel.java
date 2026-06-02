/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.VoltageLevelModification;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@JsonTypeName("VOLTAGE_LEVEL_MODIFICATION")
@ModificationErrorTypeName("MODIFY_VOLTAGE_LEVEL_ERROR")
public class VoltageLevelModificationModel extends BasicEquipmentModificationModel {
    private AttributeModification<Double> nominalV;

    private AttributeModification<Double> lowVoltageLimit;

    private AttributeModification<Double> highVoltageLimit;

    private AttributeModification<Double> ipMin;

    private AttributeModification<Double> ipMax;

    @Override
    public AbstractModification toModification() {
        return new VoltageLevelModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevel.modification")
                .withUntypedValue("voltageLevelId", getEquipmentId())
                .add();
    }

}
