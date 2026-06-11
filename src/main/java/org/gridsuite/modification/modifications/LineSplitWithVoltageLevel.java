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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.LineSplitWithVoltageLevelModel;
import org.gridsuite.modification.model.VoltageLevelCreationModel;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.lang.NonNull;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LineSplitWithVoltageLevel extends AbstractModification {

    private final LineSplitWithVoltageLevelModel modificationModel;

    public LineSplitWithVoltageLevel(LineSplitWithVoltageLevelModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void check(@NonNull Network network) throws NetworkModificationException {
        if (network.getLine(modificationModel.getLineToSplitId()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationModel.getLineToSplitId());
        }
        ModificationUtils.getInstance().controlNewOrExistingVoltageLevel(modificationModel.getMayNewVoltageLevelInfos(),
            modificationModel.getExistingVoltageLevelId(), modificationModel.getBbsOrBusId(), network);
        // check future lines don't exist
        if (network.getLine(modificationModel.getNewLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationModel.getNewLine1Id());
        }
        if (network.getLine(modificationModel.getNewLine2Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationModel.getNewLine2Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        VoltageLevelCreationModel mayNewVL = modificationModel.getMayNewVoltageLevelInfos();
        if (mayNewVL != null) {
            ModificationUtils.getInstance().createVoltageLevel(mayNewVL, subReportNode, network, namingStrategy);
        }
        ConnectVoltageLevelOnLine algo = new ConnectVoltageLevelOnLineBuilder()
            .withPositionPercent(modificationModel.getPercent())
            .withBusbarSectionOrBusId(modificationModel.getBbsOrBusId())
            .withLine1Id(modificationModel.getNewLine1Id())
            .withLine1Name(modificationModel.getNewLine1Name())
            .withLine2Id(modificationModel.getNewLine2Id())
            .withLine2Name(modificationModel.getNewLine2Name())
            .withLine(network.getLine(modificationModel.getLineToSplitId()))
            .build();

        algo.apply(network, true, subReportNode);
    }

    @Override
    public String getName() {
        return "LineSplitWithVoltageLevel";
    }
}
