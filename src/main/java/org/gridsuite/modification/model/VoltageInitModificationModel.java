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
import org.gridsuite.modification.modifications.VoltageInitModification;

import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@JsonTypeName("VOLTAGE_INIT_MODIFICATION")
@ModificationErrorTypeName("VOLTAGE_INIT_MODIFICATION_ERROR")
public class VoltageInitModificationModel extends ModificationModel {
    private List<VoltageInitGeneratorModificationModel> generators;

    private List<VoltageInitTransformerModificationModel> transformers;

    private List<VoltageInitStaticVarCompensatorModificationModel> staticVarCompensators;

    private List<VoltageInitVscConverterStationModificationModel> vscConverterStations;

    private List<VoltageInitShuntCompensatorModificationModel> shuntCompensators;

    private List<VoltageInitBusModificationModel> buses;

    private String rootNetworkName;

    private String nodeName;

    private Instant computationDate;

    @Override
    public AbstractModification toModification() {
        return new VoltageInitModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.voltageInitModification").add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("rootNetworkName", getRootNetworkName());
        mapMessageValues.put("nodeName", getNodeName());
        mapMessageValues.put("computationDate", getComputationDate() != null ? getComputationDate().toString() : null);
        return mapMessageValues;
    }
}
