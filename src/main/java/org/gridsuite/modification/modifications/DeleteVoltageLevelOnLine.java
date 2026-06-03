/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.RevertConnectVoltageLevelOnLine;
import com.powsybl.iidm.modification.topology.RevertConnectVoltageLevelOnLineBuilder;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.DeleteVoltageLevelOnLineModel;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.utils.ModificationLimitsUtils.applyRevertModificationWithMergingOfLimits;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class DeleteVoltageLevelOnLine extends AbstractModification {

    private final DeleteVoltageLevelOnLineModel modificationModel;

    public DeleteVoltageLevelOnLine(DeleteVoltageLevelOnLineModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        // check existing lines
        if (network.getLine(modificationModel.getLineToAttachTo1Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationModel.getLineToAttachTo1Id());
        }
        if (network.getLine(modificationModel.getLineToAttachTo2Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationModel.getLineToAttachTo2Id());
        }
        // check future line does not exist
        if (network.getLine(modificationModel.getReplacingLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationModel.getReplacingLine1Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        RevertConnectVoltageLevelOnLineBuilder builder = new RevertConnectVoltageLevelOnLineBuilder();
        RevertConnectVoltageLevelOnLine algo = builder.withLine1Id(modificationModel.getLineToAttachTo1Id())
                .withLine2Id(modificationModel.getLineToAttachTo2Id())
                .withLineId(modificationModel.getReplacingLine1Id())
                .withLineName(modificationModel.getReplacingLine1Name())
                .build();

        applyRevertModificationWithMergingOfLimits(network,
                modificationModel.getLineToAttachTo1Id(),
                modificationModel.getLineToAttachTo2Id(),
                modificationModel.getReplacingLine1Id(),
                algo,
                subReportNode);
    }

    @Override
    public String getName() {
        return "DeleteVoltageLevelOnLine";
    }
}
