/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.ModificationReference;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Modification reference")
@JsonTypeName("MODIFICATION_REFERENCE")
@ModificationErrorTypeName("MODIFICATION_REFERENCE_ERROR")
public class ModificationReferenceInfos extends ModificationInfos {
    public enum Type {
        SAMPLE,
        DIRECTORY,
    }

    @Schema(description = "modification reference id")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private UUID referenceId;

    @Schema(description = "modification reference type")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Type referenceType;

    @Schema(description = "modification reference info")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private ModificationInfos referenceInfos;

    @Override
    public AbstractModification toModification() {
        return new ModificationReference(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate(referenceType == Type.SAMPLE ? "network.modification.sample.reference.apply" : "network.modification.directory.reference.apply").add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        return new HashMap<>();
    }

    @Override
    public void check() {
        super.check();
        Objects.requireNonNull(referenceId, "referenceId is required");
        Objects.requireNonNull(referenceType, "referenceType is required");
        Objects.requireNonNull(referenceInfos, "referenceInfos is required");
    }
}
