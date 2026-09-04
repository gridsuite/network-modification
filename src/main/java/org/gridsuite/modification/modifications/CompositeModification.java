/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.Network;
import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;

import static org.gridsuite.modification.modifications.byfilter.AbstractModificationByAssignment.VALUE_KEY_ERROR_MESSAGE;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@Getter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CompositeModification extends AbstractModification {

    private CompositeModificationInfos compositeModificationInfos;

    @JsonIgnore
    @EqualsAndHashCode.Exclude
    protected IFilterService filterService;

    @JsonIgnore
    @EqualsAndHashCode.Exclude
    protected ILoadFlowService loadFlowService;

    public CompositeModification(CompositeModificationInfos compositeModificationInfos) {
        this.compositeModificationInfos = compositeModificationInfos;
    }

    @Override
    protected void initServices(IFilterService filterService, ILoadFlowService loadFlowService) {
        this.filterService = filterService;
        this.loadFlowService = loadFlowService;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        compositeModificationInfos.getModificationsInfos().stream()
                .filter(modificationInfos -> modificationInfos.isActivatedOn(getRootNetworkTag()))
                .forEach(
                        modif -> {
                            AbstractModification modification = modif.toModification();
                            ReportNode modifNode = modification.createSubReportNode(subReportNode);
                            try {
                                modification.check(network);
                                modification.initApplicationContext(filterService, loadFlowService, getRootNetworkTag());
                                modification.apply(network, namingStrategy, modifNode);
                            } catch (Exception e) {
                                // in case of error in a network modification, the composite modification doesn't interrupt its execution :
                                // the following modifications will be carried out
                                modifNode.newReportNode()
                                        .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                                        .withMessageTemplate("network.modification.composite.exception.report")
                                        .withUntypedValue("modificationName", modification.getName())
                                        .withUntypedValue(VALUE_KEY_ERROR_MESSAGE, e.getMessage())
                                        .withSeverity(TypedValue.ERROR_SEVERITY)
                                        .add();
                            }
                        }
        );
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.composite.apply")
                .withUntypedValue("modificationName", compositeModificationInfos.getName())
                .add();
    }

    @Override
    public String getName() {
        return ModificationType.COMPOSITE_MODIFICATION.name();
    }
}
