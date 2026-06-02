/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.SwitchKind;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.VoltageLevelCreation;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("VOLTAGE_LEVEL_CREATION")
@ModificationErrorTypeName("CREATE_VOLTAGE_LEVEL_ERROR")
public class VoltageLevelCreationModel extends EquipmentCreationModel {

    private String substationId;

    private double nominalV;

    private Double lowVoltageLimit;

    private Double highVoltageLimit;

    private Double ipMin;

    private Double ipMax;

    private int busbarCount;

    private int sectionCount;

    private List<SwitchKind> switchKinds;

    private List<CouplingDeviceModel> couplingDevices;

    private SubstationCreationModel substationCreation;

    @Override
    public AbstractModification toModification() {
        return new VoltageLevelCreation(this);
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        if (getSubstationCreation() != null) {
            Map<String, String> mapMessageValues = new HashMap<>();
            mapMessageValues.put("voltageLevelEquipmentId", getEquipmentId());
            mapMessageValues.put("substationEquipmentId", getSubstationCreation().getEquipmentId());
            return mapMessageValues;
        }
        return Map.of("equipmentId", getEquipmentId());
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevel.creation")
                .withUntypedValue("voltageLevelId", getEquipmentId())
                .add();
    }
}
