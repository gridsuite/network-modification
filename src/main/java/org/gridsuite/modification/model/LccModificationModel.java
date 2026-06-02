/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.HvdcLine;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.LccModification;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("LCC_MODIFICATION")
@ModificationErrorTypeName("MODIFY_LCC_ERROR")
public class LccModificationModel extends BasicEquipmentModificationModel {
    private AttributeModification<Double> nominalV;

    private AttributeModification<Double> r;

    private AttributeModification<Double> maxP;

    private AttributeModification<HvdcLine.ConvertersMode> convertersMode;

    private AttributeModification<Double> activePowerSetpoint;

    private LccConverterStationModificationModel converterStation1;

    private LccConverterStationModificationModel converterStation2;

    @Override
    public AbstractModification toModification() {
        return new LccModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
            .withMessageTemplate("network.modification.lcc.modification")
            .withUntypedValue("lccId", getEquipmentId())
            .add();
    }
}
