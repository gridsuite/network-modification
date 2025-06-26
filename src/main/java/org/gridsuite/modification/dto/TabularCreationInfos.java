/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.TabularCreation;
import org.springframework.lang.NonNull;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@EqualsAndHashCode(callSuper = true)
@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Tabular creation")
@JsonTypeName("TABULAR_CREATION")
@ModificationErrorTypeName("TABULAR_CREATION_ERROR")
public class TabularCreationInfos extends ModificationInfos {

    @Schema(description = "Creation type")
    @NonNull
    private ModificationType creationType;

    @Schema(description = "Creations")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ModificationInfos> creations;

    @Override
    public AbstractModification toModification() {
        return new TabularCreation(this);
    }

    public String getDefaultMessage() {
        return switch (getCreationType()) {
            case GENERATOR_CREATION -> getCreations().size() > 1 ? "generators" : "generator";
            case LOAD_CREATION -> getCreations().size() > 1 ? "loads" : "load";
            case SHUNT_COMPENSATOR_CREATION ->
                    getCreations().size() > 1 ? "shunt compensators" : "shunt compensator";
            case BATTERY_CREATION -> getCreations().size() > 1 ? "batteries" : "battery";
            default -> "equipments of unknown type";
        };
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.tabularCreation")
                .withUntypedValue("creationType", getDefaultMessage())
                .add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("tabularCreationType", getCreationType().name());
        return mapMessageValues;
    }
}
