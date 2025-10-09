/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto.tabular;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationInfos;
import org.springframework.lang.NonNull;

import java.util.List;

/**
 * @author David Braquart <david.braquart_externe at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Data
@Schema(description = "Tabular abstract modification")
public class TabularBaseInfos extends ModificationInfos {

    @Schema(description = "additional properties")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<TabularPropertyInfos> properties;

    @Schema(description = "csv file name")
    private String csvFilename;

    @Schema(description = "Modification type")
    @NonNull
    private ModificationType modificationType;

    @Schema(description = "modifications")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ModificationInfos> modifications;

    public String formatEquipmentTypeName() {
        return switch (getModificationType()) {
            case GENERATOR_CREATION, GENERATOR_MODIFICATION -> getModifications().size() > 1 ? "generators" : "generator";
            case LOAD_CREATION, LOAD_MODIFICATION -> getModifications().size() > 1 ? "loads" : "load";
            case SHUNT_COMPENSATOR_CREATION, SHUNT_COMPENSATOR_MODIFICATION -> getModifications().size() > 1 ? "shunt compensators" : "shunt compensator";
            case BATTERY_CREATION, BATTERY_MODIFICATION -> getModifications().size() > 1 ? "batteries" : "battery";
            case TWO_WINDINGS_TRANSFORMER_MODIFICATION -> getModifications().size() > 1 ? "two windings transformers" : "two windings transformer";
            case VOLTAGE_LEVEL_MODIFICATION -> getModifications().size() > 1 ? "voltage levels" : "voltage level";
            case LINE_MODIFICATION -> getModifications().size() > 1 ? "lines" : "line";
            case SUBSTATION_MODIFICATION -> getModifications().size() > 1 ? "substations" : "substation";
            default -> "equipments of unknown type";
        };
    }
}
