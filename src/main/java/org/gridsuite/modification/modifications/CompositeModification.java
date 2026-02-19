/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;

import static org.gridsuite.modification.modifications.byfilter.AbstractModificationByAssignment.VALUE_KEY_ERROR_MESSAGE;

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
        compositeModificationInfos.getModifications().forEach(
            modif -> {
                ReportNode modifNode = modif.createSubReportNode(subReportNode);
                try {
                    AbstractModification modification = modif.toModification();
                    modification.check(network);
                    modification.apply(network, modifNode);
                } catch (Exception e) {
                    // in case of error in a network modification, the composite modification doesn't interrupt its execution :
                    // the following modifications will be carried out
                    modifNode.newReportNode()
                            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                            .withMessageTemplate("network.modification.composite.reportException")
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
