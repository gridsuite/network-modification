/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.Network;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;

import java.util.List;

import static org.gridsuite.modification.modifications.byfilter.AbstractModificationByAssignment.VALUE_KEY_ERROR_MESSAGE;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
public class CompositeModification extends AbstractModification {

    private List<ModificationInfos> modificationsInfos;
    private Integer maxDepth;

    protected IFilterService filterService;
    protected ILoadFlowService loadFlowService;

    @Builder
    public CompositeModification(List<ModificationInfos> modificationsInfos,
                                 Integer maxDepth) {
        this.modificationsInfos = modificationsInfos;
        this.maxDepth = maxDepth;
    }

    @Override
    public void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService) {
        this.filterService = filterService;
        this.loadFlowService = loadFlowService;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        modificationsInfos.stream()
                .filter(modificationInfos -> Boolean.TRUE.equals(modificationInfos.getActivated())
                        && Boolean.FALSE.equals(modificationInfos.getStashed()))
                .forEach(
                        modif -> {
                            ReportNode modifNode = modif.createSubReportNode(subReportNode);
                            AbstractModification modification = modif.toModification();
                            try {
                                modification.check(network);
                                modification.initApplicationContext(filterService, loadFlowService);
                                modification.apply(network, namingStrategy, modifNode);
                            } catch (Exception e) {
                                // in case of error in a network modification, the composite modification doesn't interrupt its execution :
                                // the following modifications will be carried out
                                modifNode.newReportNode()
                                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                                        .withMessageTemplate("network.modification.composite.exception.report")
                                        .withUntypedValue("modificationName", modif.toModification().getName())
                                        .withUntypedValue(VALUE_KEY_ERROR_MESSAGE, e.getMessage())
                                        .withSeverity(TypedValue.ERROR_SEVERITY)
                                        .add();
                            }
                        }
            );
    }

    @Override
    public String getName() {
        return "CompositeModification";
    }
}
