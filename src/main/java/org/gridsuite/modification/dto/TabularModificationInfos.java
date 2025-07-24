/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import lombok.*;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.TabularModification;
import org.springframework.lang.NonNull;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.experimental.SuperBuilder;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Tabular modification")
@JsonTypeName("TABULAR_MODIFICATION")
@ModificationErrorTypeName("TABULAR_MODIFICATION_ERROR")
public class TabularModificationInfos extends TabularBaseInfos {

    @Schema(description = "Modification type")
    @NonNull
    private ModificationType modificationType;

    @Schema(description = "modifications")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ModificationInfos> modifications;

    @Override
    public AbstractModification toModification() {
        return new TabularModification(this);
    }

    public String formatEquipmentTypeName() {
        return switch (getModificationType()) {
            case GENERATOR_MODIFICATION -> getModifications().size() > 1 ? "generators" : "generator";
            case LOAD_MODIFICATION -> getModifications().size() > 1 ? "loads" : "load";
            case TWO_WINDINGS_TRANSFORMER_MODIFICATION -> getModifications().size() > 1 ? "two windings transformers" : "two windings transformer";
            case BATTERY_MODIFICATION -> getModifications().size() > 1 ? "batteries" : "battery";
            case VOLTAGE_LEVEL_MODIFICATION -> getModifications().size() > 1 ? "voltage levels" : "voltage level";
            case SHUNT_COMPENSATOR_MODIFICATION -> getModifications().size() > 1 ? "shunt compensators" : "shunt compensator";
            case LINE_MODIFICATION -> getModifications().size() > 1 ? "lines" : "line";
            case SUBSTATION_MODIFICATION -> getModifications().size() > 1 ? "substations" : "substation";
            default -> "equipments of unknown type";
        };
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
