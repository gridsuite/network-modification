/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import lombok.*;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.gridsuite.modification.NetworkModificationException;

import static org.gridsuite.modification.NetworkModificationException.Type.GROOVY_SCRIPT_EMPTY;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
@Getter
@Setter
@AllArgsConstructor
@Builder
public class GroovyScript extends AbstractModification {

    private String script;

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (StringUtils.isBlank(script)) {
            throw new NetworkModificationException(GROOVY_SCRIPT_EMPTY);
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        var conf = new CompilerConfiguration();
        var binding = new Binding();
        binding.setProperty("network", network);
        var shell = new GroovyShell(binding, conf);
        shell.evaluate(script);

        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.groovyScriptApplied")
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
    }

    @Override
    public String getName() {
        return "GroovyScript";
    }
}
