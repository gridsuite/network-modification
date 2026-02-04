/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.dto.CompositeModificationInfos;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class CompositeModification extends AbstractModification {

    private final CompositeModificationInfos compositeModificationInfos;

    public CompositeModification(CompositeModificationInfos compositeModificationInfos) {
        this.compositeModificationInfos = compositeModificationInfos;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // TODO : ajouer un log parent INFO structurant
        compositeModificationInfos.getModifications().forEach(
                modif ->
                    // TODO : en cas d'erreur ne pas interrompre l'exécution de la composite en soit
                    modif.toModification().apply(network, subReportNode)
        );
    }

    @Override
    public String getName() {
        return "CompositeModification";
    }
}
