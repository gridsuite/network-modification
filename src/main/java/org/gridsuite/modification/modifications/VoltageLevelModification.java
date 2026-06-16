/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import com.powsybl.iidm.network.extensions.MeasurementsAdder;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.BusbarSectionVMeasurementInfos;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_VOLTAGE_LEVEL_ERROR;
import static org.gridsuite.modification.utils.MeasurementUtils.upsertMeasurement;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@Getter
@AllArgsConstructor
@Builder
public class VoltageLevelModification extends AbstractModification {

    public static final String ERROR_MESSAGE = "Voltage level '%s' : ";
    private String equipmentId;
    private List<FreePropertyInfos> properties;
    private AttributeModification<String> equipmentName;
    private AttributeModification<Double> nominalV;
    private AttributeModification<Double> lowVoltageLimit;
    private AttributeModification<Double> highVoltageLimit;
    private AttributeModification<Double> ipMin;
    private AttributeModification<Double> ipMax;
    private List<BusbarSectionVMeasurementInfos> busbarSectionVMeasurements;

    @Override
    public void check(Network network) throws NetworkModificationException {
        boolean ipMinSet = false;
        boolean ipMaxSet = false;
        String errorMessage = String.format(ERROR_MESSAGE, equipmentId);
        if (Objects.nonNull(ipMin)) {
            ipMinSet = true;
            if (ipMin.getValue() < 0) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMin must be positive");
            }
        }
        if (Objects.nonNull(ipMax)) {
            ipMaxSet = true;
            if (ipMax.getValue() < 0) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMax must be positive");
            }
        }
        if (ipMinSet && ipMaxSet) {
            if (ipMin.getValue() > ipMax.getValue()) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMin cannot be greater than IpMax");
            }
        } else if (ipMinSet || ipMaxSet) {
            // only one Icc set: check with existing VL attributes
            checkIccValuesAgainstEquipmentInNetwork(network, ipMinSet, ipMaxSet);
        }
        if (nominalV != null) {
            checkIsNotNegativeValue(errorMessage, nominalV.getValue(), MODIFY_VOLTAGE_LEVEL_ERROR, "Nominal Voltage");
        }
        if (lowVoltageLimit != null) {
            checkIsNotNegativeValue(errorMessage, lowVoltageLimit.getValue(), MODIFY_VOLTAGE_LEVEL_ERROR, "Low voltage limit");
        }
        if (highVoltageLimit != null) {
            checkIsNotNegativeValue(errorMessage, highVoltageLimit.getValue(), MODIFY_VOLTAGE_LEVEL_ERROR, "High voltage limit");
        }
    }

    private void checkIccValuesAgainstEquipmentInNetwork(Network network, boolean ipMinSet, boolean ipMaxSet) {
        VoltageLevel existingVoltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, equipmentId);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = existingVoltageLevel.getExtension(IdentifiableShortCircuit.class);
        if (Objects.isNull(identifiableShortCircuit)) {
            if (ipMinSet) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMax is required");
            }
        } else {
            if (ipMinSet && ipMin.getValue() > identifiableShortCircuit.getIpMax() ||
                    ipMaxSet && identifiableShortCircuit.getIpMin() > ipMax.getValue()) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMin cannot be greater than IpMax");
            }
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, equipmentId);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.voltageLevelModification")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        ModificationUtils.getInstance().applyElementaryModifications(voltageLevel::setName, () -> voltageLevel.getOptionalName().orElse("No value"), equipmentName,
                subReportNode, "Name");
        modifyNominalV(voltageLevel, nominalV, subReportNode);
        modifLowVoltageLimit(voltageLevel, lowVoltageLimit, subReportNode);
        modifyHighVoltageLimit(voltageLevel, highVoltageLimit, subReportNode);

        modifyVoltageLevelShortCircuit(ipMin, ipMax, subReportNode, voltageLevel);
        PropertiesUtils.applyProperties(voltageLevel, subReportNode, properties, "network.modification.VlProperties");
        // busbar section voltage measurements
        if (!CollectionUtils.isEmpty(busbarSectionVMeasurements)) {
            ReportNode bbsReportNode = subReportNode.newReportNode()
                    .withMessageTemplate("network.modification.stateEstimationData")
                    .add();
            busbarSectionVMeasurements.forEach(vMeas -> {
                BusbarSection bbs = network.getBusbarSection(vMeas.getBusbarSectionId());
                if (bbs != null) {
                    List<ReportNode> measurementReports = new ArrayList<>();
                    applyBusbarSectionVoltageMeasurement(bbs, vMeas, measurementReports);
                    if (!measurementReports.isEmpty()) {
                        ModificationUtils.getInstance().reportModifications(bbsReportNode, measurementReports,
                                "network.modification.voltageLevel.busbarSection.measurements", Map.of("id", bbs.getId()));
                    }
                }
            });
        }
    }

    private void applyBusbarSectionVoltageMeasurement(BusbarSection bbs, BusbarSectionVMeasurementInfos vMeasInfo, List<ReportNode> reports) {
        Double vValue = vMeasInfo.getVMeasurementValue() != null ? vMeasInfo.getVMeasurementValue().getValue() : null;
        Boolean vValidity = vMeasInfo.getVMeasurementValidity() != null ? vMeasInfo.getVMeasurementValidity().getValue() : null;
        if (vValue == null && vValidity == null) {
            return;
        }
        Measurements<?> measurements = (Measurements<?>) bbs.getExtension(Measurements.class);
        if (measurements == null) {
            measurements = (Measurements<?>) bbs.newExtension(MeasurementsAdder.class).add();
        }
        upsertMeasurement(measurements, Measurement.Type.VOLTAGE, vValue, vValidity, reports);
    }

    @Override
    public String getName() {
        return "VoltageLevelModification";
    }

    public static void modifyHighVoltageLimit(VoltageLevel voltageLevel, AttributeModification<Double> highVoltageLimit, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(voltageLevel::setHighVoltageLimit, voltageLevel::getHighVoltageLimit, highVoltageLimit, subReportNode, "High voltage limit");
    }

    public static void modifLowVoltageLimit(VoltageLevel voltageLevel, AttributeModification<Double> lowVoltageLimit, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(voltageLevel::setLowVoltageLimit, voltageLevel::getLowVoltageLimit, lowVoltageLimit, subReportNode, "Low voltage limit");
    }

    public static void modifyNominalV(VoltageLevel voltageLevel, AttributeModification<Double> modifNominalV, ReportNode subReportNode) {
        ModificationUtils.getInstance().applyElementaryModifications(voltageLevel::setNominalV, voltageLevel::getNominalV, modifNominalV, subReportNode, "Nominal voltage");
    }

    public static void modifyVoltageLevelShortCircuit(AttributeModification<Double> ipMin,
                                                      AttributeModification<Double> ipMax,
                                                      ReportNode subReportNode,
                                                      VoltageLevel voltageLevel) {
        if (ipMin == null && ipMax == null) {
            return;
        }

        List<ReportNode> reports = new ArrayList<>();
        IdentifiableShortCircuitAdder<VoltageLevel> identifiableShortCircuitAdder = voltageLevel.newExtension(IdentifiableShortCircuitAdder.class);
        Double oldIpMin = null;
        Double oldIpMax = null;
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        if (identifiableShortCircuit != null) {
            oldIpMin = identifiableShortCircuit.getIpMin();
            oldIpMax = identifiableShortCircuit.getIpMax();
        }

        if (ipMin != null) {
            var newIpMin = ipMin.getValue();

            identifiableShortCircuitAdder.withIpMin(newIpMin);

            //convert to kA to report it like the user set it.
            var oldIpMinToReport = convertToKiloAmps(oldIpMin);
            var newIpMinToReport = convertToKiloAmps(newIpMin);
            reports.add(ModificationUtils.getInstance()
                    .buildModificationReport(oldIpMinToReport, newIpMinToReport, "Low short circuit current limit"));
        } else if (oldIpMin != null) {
            identifiableShortCircuitAdder.withIpMin(oldIpMin);
        }

        if (ipMax != null) {
            var newIpMax = ipMax.getValue();
            identifiableShortCircuitAdder.withIpMax(newIpMax);

            //Convert to kA to report it like the user set it.
            var oldIpMaxToReport = convertToKiloAmps(oldIpMax);
            var newIpMaxToReport = convertToKiloAmps(newIpMax);
            reports.add(ModificationUtils.getInstance()
                    .buildModificationReport(oldIpMaxToReport, newIpMaxToReport, "High short circuit current limit"));
        } else if (oldIpMax != null) {
            identifiableShortCircuitAdder.withIpMax(oldIpMax);
        }

        identifiableShortCircuitAdder.add();
        if (subReportNode != null) {
            reports.forEach(report -> insertReportNode(subReportNode, report));
        }
    }

    private static Double convertToKiloAmps(Double value) {
        return (value != null) ? value * 0.001 : null;
    }
}
