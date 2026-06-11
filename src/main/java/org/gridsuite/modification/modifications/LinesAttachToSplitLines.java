/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.ReplaceTeePointByVoltageLevelOnLine;
import com.powsybl.iidm.modification.topology.ReplaceTeePointByVoltageLevelOnLineBuilder;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.LinesAttachToSplitLinesModel;
import org.gridsuite.modification.utils.ModificationUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class LinesAttachToSplitLines extends AbstractModification {

    private final LinesAttachToSplitLinesModel modificationModel;

    public LinesAttachToSplitLines(LinesAttachToSplitLinesModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        // check existing lines, vl and busbar
        if (network.getLine(modificationModel.getLineToAttachTo1Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationModel.getLineToAttachTo1Id());
        }
        if (network.getLine(modificationModel.getLineToAttachTo2Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationModel.getLineToAttachTo2Id());
        }
        if (network.getLine(modificationModel.getAttachedLineId()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationModel.getAttachedLineId());
        }
        VoltageLevel vl = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getVoltageLevelId());
        ModificationUtils.getInstance().controlBus(vl, modificationModel.getBbsBusId());
        // check future lines don't exist
        if (network.getLine(modificationModel.getReplacingLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationModel.getReplacingLine1Id());
        }
        if (network.getLine(modificationModel.getReplacingLine2Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationModel.getReplacingLine2Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ReplaceTeePointByVoltageLevelOnLine algo = new ReplaceTeePointByVoltageLevelOnLineBuilder()
            .withTeePointLine1(modificationModel.getLineToAttachTo1Id())
            .withTeePointLine2(modificationModel.getLineToAttachTo2Id())
            .withTeePointLineToRemove(modificationModel.getAttachedLineId())
            .withBbsOrBusId(modificationModel.getBbsBusId())
            .withNewLine1Id(modificationModel.getReplacingLine1Id())
            .withNewLine1Name(modificationModel.getReplacingLine1Name())
            .withNewLine2Id(modificationModel.getReplacingLine2Id())
            .withNewLine2Name(modificationModel.getReplacingLine2Name())
            .build();
        algo.apply(network, true, subReportNode);
    }

    @Override
    public String getName() {
        return "LinesAttachToSplitLines";
    }
}
