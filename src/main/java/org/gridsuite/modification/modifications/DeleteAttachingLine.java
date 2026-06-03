/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.RevertCreateLineOnLine;
import com.powsybl.iidm.modification.topology.RevertCreateLineOnLineBuilder;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.DeleteAttachingLineModel;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.utils.ModificationLimitsUtils.applyRevertModificationWithMergingOfLimits;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class DeleteAttachingLine extends AbstractModification {

    private final DeleteAttachingLineModel modificationModel;

    public DeleteAttachingLine(DeleteAttachingLineModel modificationModel) {
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
        if (network.getLine(modificationModel.getAttachedLineId()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationModel.getAttachedLineId());
        }
        // check future line does not exist
        if (network.getLine(modificationModel.getReplacingLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationModel.getReplacingLine1Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        RevertCreateLineOnLineBuilder builder = new RevertCreateLineOnLineBuilder();
        RevertCreateLineOnLine algo = builder.withLineToBeMerged1Id(modificationModel.getLineToAttachTo1Id())
                .withLineToBeMerged2Id(modificationModel.getLineToAttachTo2Id())
                .withLineToBeDeletedId(modificationModel.getAttachedLineId())
                .withMergedLineId(modificationModel.getReplacingLine1Id())
                .withMergedLineName(modificationModel.getReplacingLine1Name())
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
        return "DeleteAttachingLine";
    }
}
