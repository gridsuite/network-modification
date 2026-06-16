/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.ConnectVoltageLevelOnLine;
import com.powsybl.iidm.modification.topology.ConnectVoltageLevelOnLineBuilder;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.Network;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.lang.NonNull;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@AllArgsConstructor
@Builder
public class LineSplitWithVoltageLevel extends AbstractModification {

    private String lineToSplitId;
    private double percent;
    private VoltageLevelCreationInfos mayNewVoltageLevelInfos;
    private String existingVoltageLevelId;
    private String bbsOrBusId;
    private String newLine1Id;
    private String newLine1Name;
    private String newLine2Id;
    private String newLine2Name;

    @Override
    public void check(@NonNull Network network) throws NetworkModificationException {
        if (network.getLine(lineToSplitId) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineToSplitId);
        }
        ModificationUtils.getInstance().controlNewOrExistingVoltageLevel(mayNewVoltageLevelInfos,
                existingVoltageLevelId, bbsOrBusId, network);
        // check future lines don't exist
        if (network.getLine(newLine1Id) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, newLine1Id);
        }
        if (network.getLine(newLine2Id) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, newLine2Id);
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        VoltageLevelCreationInfos mayNewVL = mayNewVoltageLevelInfos;
        if (mayNewVL != null) {
            ModificationUtils.getInstance().createVoltageLevel(mayNewVL.getEquipmentId(), mayNewVL.getProperties(), mayNewVL.getEquipmentName(), mayNewVL.getSubstationId(),
                    mayNewVL.getNominalV(), mayNewVL.getLowVoltageLimit(), mayNewVL.getHighVoltageLimit(), mayNewVL.getIpMin(), mayNewVL.getIpMax(),
                    mayNewVL.getBusbarCount(), mayNewVL.getSectionCount(), mayNewVL.getSwitchKinds(), mayNewVL.getCouplingDevices(), mayNewVL.getSubstationCreation(),
                    subReportNode, network, namingStrategy);
        }
        ConnectVoltageLevelOnLine algo = new ConnectVoltageLevelOnLineBuilder()
                .withPositionPercent(percent)
                .withBusbarSectionOrBusId(bbsOrBusId)
                .withLine1Id(newLine1Id)
                .withLine1Name(newLine1Name)
                .withLine2Id(newLine2Id)
                .withLine2Name(newLine2Name)
                .withLine(network.getLine(lineToSplitId))
                .build();

        algo.apply(network, true, subReportNode);
    }

    @Override
    public String getName() {
        return "LineSplitWithVoltageLevel";
    }
}
