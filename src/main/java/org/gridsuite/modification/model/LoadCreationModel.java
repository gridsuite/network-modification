/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.LoadType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.LoadCreation;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Load creation")
@ModificationErrorTypeName("CREATE_LOAD_ERROR")
public class LoadCreationModel extends InjectionCreationModel implements ModificationModel {
    @Schema(description = "Load type")
    private LoadType loadType;

    @Schema(description = "Active power")
    private double p0;

    @Schema(description = "Reactive power")
    private double q0;

    @Override
    public AbstractModification toModification() {
        return new LoadCreation(this);
    }

    @Override
    public ModificationType getType() {
        return ModificationType.LOAD_CREATION;
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.load.creation")
                .withUntypedValue("loadId", getEquipmentId())
                .add();
    }
}
