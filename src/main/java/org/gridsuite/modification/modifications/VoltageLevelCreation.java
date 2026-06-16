/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_VOLTAGE_LEVEL_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 */
@NoArgsConstructor
@Getter
@AllArgsConstructor
@Builder
public class VoltageLevelCreation extends AbstractModification {

    private String equipmentId;
    private List<FreePropertyInfos> properties;
    private String equipmentName;
    private String substationId;
    private double nominalV;
    private Double lowVoltageLimit;
    private Double highVoltageLimit;
    private Double ipMin;
    private Double ipMax;
    private int busbarCount;
    private int sectionCount;
    private List<SwitchKind> switchKinds;
    private List<CouplingDeviceInfos> couplingDevices;
    private SubstationCreationInfos substationCreation;

    @Override
    public void check(Network network) throws NetworkModificationException {
        String errorMessage = "Voltage level '" + equipmentId + "' : ";
        ModificationUtils.getInstance().controlVoltageLevelCreation(equipmentId, couplingDevices, ipMin, ipMax, network);
        checkIsNotNegativeValue(errorMessage, nominalV, CREATE_VOLTAGE_LEVEL_ERROR, "Nominal Voltage");
        checkIsNotNegativeValue(errorMessage, lowVoltageLimit, CREATE_VOLTAGE_LEVEL_ERROR, "Low voltage limit");
        checkIsNotNegativeValue(errorMessage, highVoltageLimit, CREATE_VOLTAGE_LEVEL_ERROR, "High voltage limit");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        apply(network, new DefaultNamingStrategy(), subReportNode);
    }

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, ReportNode subReportNode) {
        ModificationUtils.getInstance().createVoltageLevel(equipmentId, properties, equipmentName, substationId, nominalV, lowVoltageLimit, highVoltageLimit,
                ipMin, ipMax, busbarCount, sectionCount, switchKinds, couplingDevices, substationCreation, subReportNode, network, namingStrategy);
    }

    @Override
    public String getName() {
        return "VoltageLevelCreation";
    }

}
