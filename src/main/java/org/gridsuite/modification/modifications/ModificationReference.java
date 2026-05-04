/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.dto.ModificationReferenceInfos;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class ModificationReference extends AbstractModification {

    private final ModificationReferenceInfos modificationReferenceInfos;

    protected IFilterService filterService;
    protected ILoadFlowService loadFlowService;

    public ModificationReference(ModificationReferenceInfos modificationReferenceInfos) {
        this.modificationReferenceInfos = modificationReferenceInfos;
    }

    @Override
    public void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService) {
        this.filterService = filterService;
        this.loadFlowService = loadFlowService;
    }

    @Override
    public void check(Network network) {
        super.check(network);
        modificationReferenceInfos.check();
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        AbstractModification modification = modificationReferenceInfos.getReferenceInfos().toModification();
        modification.check(network);
        modification.initApplicationContext(filterService, loadFlowService);
        modification.apply(network, namingStrategy, modificationReferenceInfos.getReferenceInfos().createSubReportNode(subReportNode));
    }

    @Override
    public String getName() {
        return "ModificationReference";
    }
}
