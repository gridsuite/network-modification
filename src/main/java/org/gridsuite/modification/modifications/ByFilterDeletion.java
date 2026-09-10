/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.RemoveFeederBay;
import com.powsybl.iidm.modification.topology.RemoveHvdcLineBuilder;
import com.powsybl.iidm.modification.topology.RemoveSubstationBuilder;
import com.powsybl.iidm.modification.topology.RemoveVoltageLevel;
import com.powsybl.iidm.network.*;
import lombok.*;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.utils.ModificationUtils;

import java.util.*;

import static org.gridsuite.modification.modifications.byfilter.AbstractModificationByAssignment.VALUE_KEY_EQUIPMENT_COUNT;

/**
 * @author Antoine Bouhours <antoine.bouhours at rte-france.com>
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ByFilterDeletion extends AbstractModification {

    private static final String REPORT_KEY_FILTER_EVALUATION = "network.modification.byFilterDeletion.filterEvaluation";
    private static final String REPORT_KEY_FILTER_EVALUATION_RESULT = "network.modification.byFilterDeletion.filterEvaluationResult";
    private static final String REPORT_KEY_NO_EQUIPMENT_TO_REMOVE = "network.modification.byFilterDeletion.noEquipmentToRemove";
    private static final String REPORT_KEY_EQUIPMENTS_TO_REMOVE = "network.modification.byFilterDeletion.equipmentsToRemove";
    private static final String REPORT_KEY_REMOVE_EQUIPMENTS = "network.modification.byFilterDeletion.removeEquipments";

    private IdentifiableType equipmentType;
    private List<Filter> filters;

    private static final EnumSet<IdentifiableType> CONNECTABLE_TYPES = EnumSet.of(
            IdentifiableType.LINE,
            IdentifiableType.TWO_WINDINGS_TRANSFORMER,
            IdentifiableType.THREE_WINDINGS_TRANSFORMER,
            IdentifiableType.GENERATOR,
            IdentifiableType.BATTERY,
            IdentifiableType.LOAD,
            IdentifiableType.SHUNT_COMPENSATOR,
            IdentifiableType.BOUNDARY_LINE,
            IdentifiableType.STATIC_VAR_COMPENSATOR
            );

    @Builder
    public ByFilterDeletion(IdentifiableType equipmentType, List<Filter> filters) {
        this.equipmentType = equipmentType;
        this.filters = filters;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Set<Identifiable<?>> equipments = new HashSet<>();
        for (int i = 0; i < filters.size(); i++) {
            ReportNode filterReportNode = subReportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION)
                    .withUntypedValue("filterCount", i)
                    .add();

            equipments.addAll(filters.get(i).evaluate(network, filterReportNode));
        }
        subReportNode.newReportNode()
                .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION_RESULT)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .withUntypedValue(VALUE_KEY_EQUIPMENT_COUNT, equipments.size())
                .add();
        if (CollectionUtils.isEmpty(equipments)) {
            subReportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_NO_EQUIPMENT_TO_REMOVE)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .add();
        } else {
            subReportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_EQUIPMENTS_TO_REMOVE)
                    .withUntypedValue("nbEquipments", (long) equipments.size())
                    .withUntypedValue("type", equipmentType.name())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            ReportNode removeEquipmentsNode = subReportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_REMOVE_EQUIPMENTS)
                    .add();
            // Report node is pushed to network instance to allow deletion logs from other libraries to be added
            network.getReportNodeContext().pushReportNode(removeEquipmentsNode);
            applyFilterDeletion(network, removeEquipmentsNode, equipments);
        }
    }

    @Override
    public String getName() {
        return ModificationType.BY_FILTER_DELETION.name();
    }

    private void applyFilterDeletion(Network network, ReportNode subReportNode, Set<Identifiable<?>> equipments) {
        IdentifiableType identifiableType = equipmentType;
        if (CONNECTABLE_TYPES.contains(identifiableType)) {
            equipments.forEach(identifiableAttribute -> new RemoveFeederBay(identifiableAttribute.getId()).apply(network, true, subReportNode));
        } else if (identifiableType == IdentifiableType.VOLTAGE_LEVEL) {
            equipments.forEach(identifiableAttribute -> new RemoveVoltageLevel(identifiableAttribute.getId()).apply(network, true, subReportNode));
        } else if (identifiableType == IdentifiableType.SUBSTATION) {
            equipments.forEach(identifiableAttribute -> new RemoveSubstationBuilder().withSubstationId(identifiableAttribute.getId()).build().apply(network, true, subReportNode));
        } else if (identifiableType == IdentifiableType.HVDC_LINE) {
            equipments.forEach(identifiableAttribute -> removeHvdcLine(network, subReportNode, identifiableAttribute));
        } else {
            throw NetworkModificationException.createEquipmentTypeUnknown(identifiableType.name());
        }
    }

    private void removeHvdcLine(Network network, ReportNode subReportNode, Identifiable<?> equipment) {
        HvdcLine hvdcLine = (HvdcLine) ModificationUtils.getInstance().getEquipmentByIdentifiableType(network, equipmentType, equipment.getId());
        if (hvdcLine != null) {
            HvdcConverterStation<?> converterStation1 = hvdcLine.getConverterStation1();
            HvdcConverterStation<?> converterStation2 = hvdcLine.getConverterStation2();
            if (converterStation1.getHvdcType() == HvdcConverterStation.HvdcType.LCC || converterStation2.getHvdcType() == HvdcConverterStation.HvdcType.LCC) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.SCNotRemoved")
                        .withUntypedValue("id", equipment.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            }
        }
        new RemoveHvdcLineBuilder().withHvdcLineId(equipment.getId()).build().apply(network, true, subReportNode);
    }
}
