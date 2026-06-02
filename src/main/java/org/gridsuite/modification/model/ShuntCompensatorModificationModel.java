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
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.ShuntCompensatorModification;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("SHUNT_COMPENSATOR_MODIFICATION")
@ModificationErrorTypeName("MODIFY_SHUNT_COMPENSATOR_ERROR")
public class ShuntCompensatorModificationModel extends InjectionModificationModel {

    private AttributeModification<Integer> maximumSectionCount;

    private AttributeModification<Integer> sectionCount;

    private AttributeModification<Double> maxSusceptance;

    private AttributeModification<Double> maxQAtNominalV;

    private AttributeModification<ShuntCompensatorType> shuntCompensatorType;

    @Override
    public AbstractModification toModification() {
        return new ShuntCompensatorModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.shuntCompensatorModification.modification")
                .withUntypedValue("shuntCompensatorId", this.getEquipmentId())
                .add();
    }
}
