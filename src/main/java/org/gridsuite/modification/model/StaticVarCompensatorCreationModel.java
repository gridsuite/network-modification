/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.StaticVarCompensator;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.StaticVarCompensatorCreation;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("STATIC_VAR_COMPENSATOR_CREATION")
@ModificationErrorTypeName("CREATE_STATIC_VAR_COMPENSATOR_ERROR")
public class StaticVarCompensatorCreationModel extends InjectionCreationModel {
    private Double maxSusceptance;

    private Double minSusceptance;

    private Double maxQAtNominalV;

    private Double minQAtNominalV;

    private StaticVarCompensator.RegulationMode regulationMode;

    private Double voltageSetpoint;

    private Double reactivePowerSetpoint;

    private VoltageRegulationType voltageRegulationType;

    private String regulatingTerminalId;

    private String regulatingTerminalType;

    private String regulatingTerminalVlId;

    @JsonProperty("isRegulating")
    public boolean regulating;

    private boolean standbyAutomatonOn;

    private boolean standby;

    private Double b0;

    private Double q0;

    private Double lowVoltageSetpoint;

    private Double highVoltageSetpoint;

    private Double lowVoltageThreshold;

    private Double highVoltageThreshold;

    @Override
    public AbstractModification toModification() {
        return new StaticVarCompensatorCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.staticVarCompensatorCreation").withUntypedValue("id", this.getEquipmentId()).add();
    }
}
