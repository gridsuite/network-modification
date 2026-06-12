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
import com.powsybl.iidm.network.extensions.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.AttributeModification;
import org.gridsuite.modification.model.BusbarSectionVMeasurementModel;
import org.gridsuite.modification.model.VoltageLevelModificationModel;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;
import org.springframework.util.CollectionUtils;

import java.util.*;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_VOLTAGE_LEVEL_ERROR;
import static org.gridsuite.modification.utils.ModificationUtils.checkIsNotNegativeValue;
import static org.gridsuite.modification.utils.ModificationUtils.insertReportNode;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class VoltageLevelModification extends AbstractModification {

    public static final String ERROR_MESSAGE = "Voltage level '%s' : ";
    private final VoltageLevelModificationModel modificationModel;

    public VoltageLevelModification(VoltageLevelModificationModel voltageLevelModificationModel) {
        this.modificationModel = voltageLevelModificationModel;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        boolean ipMinSet = false;
        boolean ipMaxSet = false;
        String errorMessage = String.format(ERROR_MESSAGE, modificationModel.getEquipmentId());
        if (Objects.nonNull(modificationModel.getIpMin())) {
            ipMinSet = true;
            if (modificationModel.getIpMin().getValue() < 0) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMin must be positive");
            }
        }
        if (Objects.nonNull(modificationModel.getIpMax())) {
            ipMaxSet = true;
            if (modificationModel.getIpMax().getValue() < 0) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMax must be positive");
            }
        }
        if (ipMinSet && ipMaxSet) {
            if (modificationModel.getIpMin().getValue() > modificationModel.getIpMax().getValue()) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMin cannot be greater than IpMax");
            }
        } else if (ipMinSet || ipMaxSet) {
            // only one Icc set: check with existing VL attributes
            checkIccValuesAgainstEquipmentInNetwork(network, ipMinSet, ipMaxSet);
        }
        if (modificationModel.getNominalV() != null) {
            checkIsNotNegativeValue(errorMessage, modificationModel.getNominalV().getValue(), MODIFY_VOLTAGE_LEVEL_ERROR, "Nominal Voltage");
        }
        if (modificationModel.getLowVoltageLimit() != null) {
            checkIsNotNegativeValue(errorMessage, modificationModel.getLowVoltageLimit().getValue(), MODIFY_VOLTAGE_LEVEL_ERROR, "Low voltage limit");
        }
        if (modificationModel.getHighVoltageLimit() != null) {
            checkIsNotNegativeValue(errorMessage, modificationModel.getHighVoltageLimit().getValue(), MODIFY_VOLTAGE_LEVEL_ERROR, "High voltage limit");
        }
    }

    private void checkIccValuesAgainstEquipmentInNetwork(Network network, boolean ipMinSet, boolean ipMaxSet) {
        VoltageLevel existingVoltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getEquipmentId());
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = existingVoltageLevel.getExtension(IdentifiableShortCircuit.class);
        if (Objects.isNull(identifiableShortCircuit)) {
            if (ipMinSet) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMax is required");
            }
        } else {
            if (ipMinSet && modificationModel.getIpMin().getValue() > identifiableShortCircuit.getIpMax() ||
                ipMaxSet && identifiableShortCircuit.getIpMin() > modificationModel.getIpMax().getValue()) {
                throw new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMin cannot be greater than IpMax");
            }
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationModel.getEquipmentId());

        subReportNode.newReportNode()
            .withMessageTemplate("network.modification.voltageLevelModification")
            .withUntypedValue("id", modificationModel.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        ModificationUtils.getInstance().applyElementaryModifications(voltageLevel::setName,
            () -> voltageLevel.getOptionalName().orElse("No value"), modificationModel.getEquipmentName(), subReportNode, "Name");
        modifyNominalV(voltageLevel, modificationModel.getNominalV(), subReportNode);
        modifLowVoltageLimit(voltageLevel, modificationModel.getLowVoltageLimit(), subReportNode);
        modifyHighVoltageLimit(voltageLevel, modificationModel.getHighVoltageLimit(), subReportNode);

        modifyVoltageLevelShortCircuit(modificationModel.getIpMin(), modificationModel.getIpMax(), subReportNode, voltageLevel);
        PropertiesUtils.applyProperties(voltageLevel, subReportNode, modificationModel.getProperties(), "network.modification.VlProperties");
        // busbar section voltage measurements
        if (!CollectionUtils.isEmpty(modificationModel.getBusbarSectionVMeasurements())) {
            ReportNode bbsReportNode = subReportNode.newReportNode()
                .withMessageTemplate("network.modification.stateEstimationData")
                .add();
            modificationModel.getBusbarSectionVMeasurements().forEach(vMeas -> {
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

    private void applyBusbarSectionVoltageMeasurement(BusbarSection bbs, BusbarSectionVMeasurementModel vMeasInfo, List<ReportNode> reports) {
        Double vValue = vMeasInfo.getVMeasurementValue() != null ? vMeasInfo.getVMeasurementValue().getValue() : null;
        Boolean vValidity = vMeasInfo.getVMeasurementValidity() != null ? vMeasInfo.getVMeasurementValidity().getValue() : null;
        if (vValue == null && vValidity == null) {
            return;
        }
        Measurements<?> measurements = (Measurements<?>) bbs.getExtension(Measurements.class);
        if (measurements == null) {
            measurements = (Measurements<?>) bbs.newExtension(MeasurementsAdder.class).add();
        }
        upsertVoltageMeasurement(measurements, vValue, vValidity, reports);
    }

    private void upsertVoltageMeasurement(Measurements<?> measurements, Double value, Boolean requestedValidity, List<ReportNode> reports) {
        String fieldPrefix = "Voltage measurement ";
        Measurement existing = measurements.getMeasurements(Measurement.Type.VOLTAGE).stream().findFirst().orElse(null);
        if (existing != null) {
            if (value != null) {
                double oldValue = existing.getValue();
                existing.setValue(value);
                reports.add(ModificationUtils.buildModificationReport(oldValue, value, fieldPrefix + "value", TypedValue.INFO_SEVERITY));
            }
            if (requestedValidity != null) {
                boolean oldValidity = existing.isValid();
                ModificationUtils.updateMeasurementValidity(existing, requestedValidity);
                reports.add(ModificationUtils.buildModificationReport(oldValidity, requestedValidity, fieldPrefix + "validity", TypedValue.INFO_SEVERITY));
            }
        } else {
            var adder = measurements.newMeasurement().setId(UUID.randomUUID().toString()).setType(Measurement.Type.VOLTAGE);
            if (value != null) {
                adder.setValue(value);
                reports.add(ModificationUtils.buildModificationReport(null, value, fieldPrefix + "value", TypedValue.INFO_SEVERITY));
            }
            if (requestedValidity != null) {
                adder.setValid(requestedValidity);
                reports.add(ModificationUtils.buildModificationReport(null, requestedValidity, fieldPrefix + "validity", TypedValue.INFO_SEVERITY));
            }
            adder.add();
        }
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
