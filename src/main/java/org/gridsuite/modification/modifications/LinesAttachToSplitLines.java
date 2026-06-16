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
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.utils.ModificationUtils;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@NoArgsConstructor
@Getter
@AllArgsConstructor
@Builder
public class LinesAttachToSplitLines extends AbstractModification {

    private String lineToAttachTo1Id;
    private String lineToAttachTo2Id;
    private String attachedLineId;
    private String voltageLevelId;
    private String bbsBusId;
    private String replacingLine1Id;
    private String replacingLine1Name;
    private String replacingLine2Id;
    private String replacingLine2Name;

    @Override
    public void check(Network network) throws NetworkModificationException {
        // check existing lines, vl and busbar
        if (network.getLine(lineToAttachTo1Id) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineToAttachTo1Id);
        }
        if (network.getLine(lineToAttachTo2Id) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineToAttachTo2Id);
        }
        if (network.getLine(attachedLineId) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, attachedLineId);
        }
        VoltageLevel vl = ModificationUtils.getInstance().getVoltageLevel(network, voltageLevelId);
        ModificationUtils.getInstance().controlBus(vl, bbsBusId);
        // check future lines don't exist
        if (network.getLine(replacingLine1Id) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, replacingLine1Id);
        }
        if (network.getLine(replacingLine2Id) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, replacingLine2Id);
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ReplaceTeePointByVoltageLevelOnLine algo = new ReplaceTeePointByVoltageLevelOnLineBuilder()
                .withTeePointLine1(lineToAttachTo1Id)
                .withTeePointLine2(lineToAttachTo2Id)
                .withTeePointLineToRemove(attachedLineId)
                .withBbsOrBusId(bbsBusId)
                .withNewLine1Id(replacingLine1Id)
                .withNewLine1Name(replacingLine1Name)
                .withNewLine2Id(replacingLine2Id)
                .withNewLine2Name(replacingLine2Name)
                .build();
        algo.apply(network, true, subReportNode);
    }

    @Override
    public String getName() {
        return "LinesAttachToSplitLines";
    }
}
