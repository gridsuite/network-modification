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
import lombok.*;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationReferenceInfos;

import java.util.Objects;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ModificationReference extends AbstractModification {

    private ModificationReferenceInfos.Type referenceType;
    private AbstractModification referenceModification;

    @Builder
    public ModificationReference(ModificationReferenceInfos.Type referenceType,
                                 AbstractModification referenceModification) {
        this.referenceType = referenceType;
        this.referenceModification = referenceModification;
    }

    @Override
    protected void initServices(IFilterService filterService, ILoadFlowService loadFlowService) {
        referenceModification.initServices(filterService, loadFlowService);
    }

    @Override
    public void check(Network network) {
        super.check(network);
        Objects.requireNonNull(referenceType, "referenceType is required");
        Objects.requireNonNull(referenceModification, "referenceInfos is required");
        referenceModification.check(network);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        referenceModification.apply(network, namingStrategy, referenceModification.createSubReportNode(subReportNode));
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        String messageTemplate = referenceType == ModificationReferenceInfos.Type.BASIC
                ? "network.modification.basic.reference.apply"
                : "network.modification.directory.reference.apply";
        return reportNode.newReportNode().withMessageTemplate(messageTemplate).add();
    }

    @Override
    public String getName() {
        return ModificationType.MODIFICATION_REFERENCE.name();
    }
}
