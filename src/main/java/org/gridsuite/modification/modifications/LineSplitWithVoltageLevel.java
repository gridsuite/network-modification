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
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.lang.NonNull;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LineSplitWithVoltageLevel extends AbstractModification {

    private final LineSplitWithVoltageLevelInfos modificationInfos;

    public LineSplitWithVoltageLevel(LineSplitWithVoltageLevelInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(@NonNull Network network) throws NetworkModificationException {
        if (network.getLine(modificationInfos.getLineToSplitId()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToSplitId());
        }
        ModificationUtils.getInstance().controlNewOrExistingVoltageLevel(modificationInfos.getMayNewVoltageLevelInfos(),
                modificationInfos.getExistingVoltageLevelId(), modificationInfos.getBbsOrBusId(), network);
        // check future lines don't exist
        if (network.getLine(modificationInfos.getNewLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getNewLine1Id());
        }
        if (network.getLine(modificationInfos.getNewLine2Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getNewLine2Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevelCreationInfos mayNewVL = modificationInfos.getMayNewVoltageLevelInfos();
        ModificationUtils.getInstance().createVoltageLevelWithProperties(mayNewVL, network, subReportNode);

        ConnectVoltageLevelOnLine algo = new ConnectVoltageLevelOnLineBuilder()
                .withPositionPercent(modificationInfos.getPercent())
                .withBusbarSectionOrBusId(modificationInfos.getBbsOrBusId())
                .withLine1Id(modificationInfos.getNewLine1Id())
                .withLine1Name(modificationInfos.getNewLine1Name())
                .withLine2Id(modificationInfos.getNewLine2Id())
                .withLine2Name(modificationInfos.getNewLine2Name())
                .withLine(network.getLine(modificationInfos.getLineToSplitId()))
                .build();

        algo.apply(network, true, subReportNode);
    }

    @Override
    public String getName() {
        return "LineSplitWithVoltageLevel";
    }
}
