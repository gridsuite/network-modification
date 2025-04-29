/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.VoltageInitModification;

import java.util.List;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Voltage init modification infos")
@JsonTypeName("VOLTAGE_INIT_MODIFICATION")
@ModificationErrorTypeName("VOLTAGE_INIT_MODIFICATION_ERROR")
public class VoltageInitModificationInfos extends ModificationInfos {
    @Schema(description = "generators modifications")
    private List<VoltageInitGeneratorModificationInfos> generators;

    @Schema(description = "transformers modifications")
    private List<VoltageInitTransformerModificationInfos> transformers;

    @Schema(description = "static var compensator modifications")
    private List<VoltageInitStaticVarCompensatorModificationInfos> staticVarCompensators;

    @Schema(description = "vsc converter station modifications")
    private List<VoltageInitVscConverterStationModificationInfos> vscConverterStations;

    @Schema(description = "shunt compensator modifications")
    private List<VoltageInitShuntCompensatorModificationInfos> shuntCompensators;

    @Schema(description = "buses modifications")
    private List<VoltageInitBusModificationInfos> buses;

    @Override
    public AbstractModification toModification() {
        return new VoltageInitModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.voltageInitModification").add();
    }
}
