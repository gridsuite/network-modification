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
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.OperationalLimitsGroup;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.DeleteAttachingLineInfos;

import java.util.Collection;
import java.util.Optional;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.utils.ModificationLimitsUtils.setMergedOperationalLimitsGroups;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class DeleteAttachingLine extends AbstractModification {

    private final DeleteAttachingLineInfos modificationInfos;

    public DeleteAttachingLine(DeleteAttachingLineInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        // check existing lines
        if (network.getLine(modificationInfos.getLineToAttachTo1Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToAttachTo1Id());
        }
        if (network.getLine(modificationInfos.getLineToAttachTo2Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToAttachTo2Id());
        }
        if (network.getLine(modificationInfos.getAttachedLineId()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getAttachedLineId());
        }
        // check future line does not exist
        if (network.getLine(modificationInfos.getReplacingLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getReplacingLine1Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {

        // to be removed if powsybl integrate it
        Line line1 = network.getLine(modificationInfos.getLineToAttachTo1Id());
        String line1Id = line1.getId();
        Optional<String> selectedGroupLine1Side1 = line1.getSelectedOperationalLimitsGroupId1();
        Optional<String> selectedGroupLine1Side2 = line1.getSelectedOperationalLimitsGroupId2();
        Collection<OperationalLimitsGroup> groupsLine1Side1 = line1.getOperationalLimitsGroups1();
        Collection<OperationalLimitsGroup> groupsLine1Side2 = line1.getOperationalLimitsGroups2();
        Line line2 = network.getLine(modificationInfos.getLineToAttachTo2Id());
        String line2Id = line2.getId();
        Optional<String> selectedGroupLine2Side1 = line2.getSelectedOperationalLimitsGroupId1();
        Optional<String> selectedGroupLine2Side2 = line2.getSelectedOperationalLimitsGroupId2();
        Collection<OperationalLimitsGroup> groupsLine2Side1 = line2.getOperationalLimitsGroups1();
        Collection<OperationalLimitsGroup> groupsLine2Side2 = line2.getOperationalLimitsGroups2();

        RevertCreateLineOnLineBuilder builder = new RevertCreateLineOnLineBuilder();
        RevertCreateLineOnLine algo = builder.withLineToBeMerged1Id(modificationInfos.getLineToAttachTo1Id())
                .withLineToBeMerged2Id(modificationInfos.getLineToAttachTo2Id())
                .withLineToBeDeletedId(modificationInfos.getAttachedLineId())
                .withMergedLineId(modificationInfos.getReplacingLine1Id())
                .withMergedLineName(modificationInfos.getReplacingLine1Name())
                .build();
        algo.apply(network, true, subReportNode);

        // to be removed if powsybl integrate it
        setMergedOperationalLimitsGroups(network.getLine(modificationInfos.getReplacingLine1Id()),
                groupsLine1Side1,
                groupsLine1Side2,
                groupsLine2Side1,
                groupsLine2Side2,
                selectedGroupLine1Side1.orElse(null),
                selectedGroupLine1Side2.orElse(null),
                selectedGroupLine2Side1.orElse(null),
                selectedGroupLine2Side2.orElse(null),
                line1Id,
                line2Id,
                subReportNode);
    }

    @Override
    public String getName() {
        return "DeleteAttachingLine";
    }
}
