/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.modifications.VscCreation;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("VSC_CREATION")
@ModificationErrorTypeName("CREATE_VSC_ERROR")
public class VscCreationModel extends EquipmentCreationModel {
    private Double nominalV;

    private Double r;

    private Double maxP;

    private Float operatorActivePowerLimitFromSide1ToSide2;

    private Float operatorActivePowerLimitFromSide2ToSide1;

    private HvdcLine.ConvertersMode convertersMode;

    private Double activePowerSetpoint;

    private Boolean angleDroopActivePowerControl;

    private Float p0;

    private Float droop;

    private ConverterStationCreationModel converterStation1;

    private ConverterStationCreationModel converterStation2;

    @Override
    public AbstractModification toModification() {
        return new VscCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.vsc.creation")
                .withUntypedValue("vscId", getEquipmentId())
                .add();
    }
}
