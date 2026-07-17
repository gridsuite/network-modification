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
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;

import java.util.Objects;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Getter
@Setter
public class ModificationReference extends AbstractModification {

    private UUID referenceId;
    private ModificationReferenceInfos.Type referenceType;
    private ModificationInfos referenceInfos;

    protected IFilterService filterService;
    protected ILoadFlowService loadFlowService;

    @Builder
    public ModificationReference(UUID referenceId,
                                 ModificationReferenceInfos.Type referenceType,
                                 ModificationInfos referenceInfos) {
        this.referenceId = referenceId;
        this.referenceType = referenceType;
        this.referenceInfos = referenceInfos;
    }

    @Override
    public void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService) {
        this.filterService = filterService;
        this.loadFlowService = loadFlowService;
    }

    @Override
    public void check(Network network) {
        super.check(network);
        Objects.requireNonNull(referenceId, "referenceId is required");
        Objects.requireNonNull(referenceType, "referenceType is required");
        Objects.requireNonNull(referenceInfos, "referenceInfos is required");
        referenceInfos.check();
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        AbstractModification modification = referenceInfos.toModification();
        modification.check(network);
        modification.initApplicationContext(filterService, loadFlowService);
        modification.apply(network, namingStrategy, referenceInfos.createSubReportNode(subReportNode));
    }

    @Override
    public String getName() {
        return ModificationType.MODIFICATION_REFERENCE.name();
    }
}
