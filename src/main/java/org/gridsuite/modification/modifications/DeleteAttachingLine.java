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
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.utils.ModificationLimitsUtils.applyRevertModificationWithMergingOfLimits;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Getter
@Setter
@AllArgsConstructor
@Builder
public class DeleteAttachingLine extends AbstractModification {

    private String lineToAttachTo1Id;
    private String lineToAttachTo2Id;
    private String attachedLineId;
    private String replacingLine1Id;
    private String replacingLine1Name;

    @Override
    public void check(Network network) throws NetworkModificationException {
        // check existing lines
        if (network.getLine(lineToAttachTo1Id) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineToAttachTo1Id);
        }
        if (network.getLine(lineToAttachTo2Id) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineToAttachTo2Id);
        }
        if (network.getLine(attachedLineId) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, attachedLineId);
        }
        // check future line does not exist
        if (network.getLine(replacingLine1Id) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, replacingLine1Id);
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        RevertCreateLineOnLineBuilder builder = new RevertCreateLineOnLineBuilder();
        RevertCreateLineOnLine algo = builder.withLineToBeMerged1Id(lineToAttachTo1Id)
                .withLineToBeMerged2Id(lineToAttachTo2Id)
                .withLineToBeDeletedId(attachedLineId)
                .withMergedLineId(replacingLine1Id)
                .withMergedLineName(replacingLine1Name)
                .build();

        applyRevertModificationWithMergingOfLimits(network,
                lineToAttachTo1Id,
                lineToAttachTo2Id,
                replacingLine1Id,
                algo,
                subReportNode);
    }

    @Override
    public String getName() {
        return "DeleteAttachingLine";
    }
}
