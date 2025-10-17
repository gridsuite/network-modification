/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto.tabular;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import lombok.*;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.tabular.TabularModification;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.experimental.SuperBuilder;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@EqualsAndHashCode(callSuper = true)
@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Tabular modification")
@JsonTypeName("TABULAR_MODIFICATION")
@ModificationErrorTypeName("TABULAR_MODIFICATION_ERROR")
public class TabularModificationInfos extends TabularBaseInfos {
    @Override
    public AbstractModification toModification() {
        return new TabularModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.tabularModification")
                .withUntypedValue("modificationType", formatEquipmentTypeName())
                .add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("tabularModificationType", getModificationType().name());
        return mapMessageValues;
    }
}
