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
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.CREATE_VOLTAGE_LEVEL_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 */
@Getter
@Setter
public class VoltageLevelCreation extends AbstractEquipmentCreation {

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
    private SubstationCreation substationCreation;

    @Builder
    public VoltageLevelCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName,
                                String substationId, double nominalV, Double lowVoltageLimit, Double highVoltageLimit,
                                Double ipMin, Double ipMax, int busbarCount, int sectionCount,
                                List<SwitchKind> switchKinds,
                                List<CouplingDeviceInfos> couplingDevices, SubstationCreation substationCreation) {
        super(equipmentId, properties, equipmentName);
        this.substationId = substationId;
        this.nominalV = nominalV;
        this.lowVoltageLimit = lowVoltageLimit;
        this.highVoltageLimit = highVoltageLimit;
        this.ipMin = ipMin;
        this.ipMax = ipMax;
        this.busbarCount = busbarCount;
        this.sectionCount = sectionCount;
        this.switchKinds = switchKinds;
        this.couplingDevices = couplingDevices;
        this.substationCreation = substationCreation;
    }

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
        ModificationUtils.getInstance().createVoltageLevel(this, subReportNode, network, namingStrategy);
    }

    @Override
    public String getName() {
        return "VoltageLevelCreation";
    }

}
