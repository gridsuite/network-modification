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
import com.powsybl.iidm.network.HvdcConverterStation;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;

import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.utils.ModificationUtils;
import org.springframework.util.CollectionUtils;

import static org.gridsuite.modification.utils.ModificationUtils.createReport;
import static org.gridsuite.modification.utils.ModificationUtils.distinctByKey;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Antoine Bouhours <antoine.bouhours at rte-france.com>
 */
public class ByFilterDeletion extends AbstractModification {

    private final ByFilterDeletionInfos modificationInfos;

    protected IFilterService filterService;

    private static final EnumSet<IdentifiableType> CONNECTABLE_TYPES = EnumSet.of(
            IdentifiableType.LINE,
            IdentifiableType.TWO_WINDINGS_TRANSFORMER,
            IdentifiableType.THREE_WINDINGS_TRANSFORMER,
            IdentifiableType.GENERATOR,
            IdentifiableType.BATTERY,
            IdentifiableType.LOAD,
            IdentifiableType.SHUNT_COMPENSATOR,
            IdentifiableType.DANGLING_LINE,
            IdentifiableType.STATIC_VAR_COMPENSATOR
            );

    public ByFilterDeletion(ByFilterDeletionInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService) {
        this.filterService = filterService;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        var filters = modificationInfos.getFilters().stream()
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        Map<UUID, FilterEquipments> exportFilters = ModificationUtils.getUuidFilterEquipmentsMap(filterService, network, subReportNode, filters, modificationInfos.getErrorType());
        if (exportFilters != null) {
            ModificationUtils.logWrongEquipmentsIdsFilters(subReportNode, exportFilters, filters);
            Set<IdentifiableAttributes> identifiableAttributes = ModificationUtils.getIdentifiableAttributes(exportFilters, modificationInfos.getFilters(), subReportNode);

            if (CollectionUtils.isEmpty(identifiableAttributes)) {
                String filterNames = modificationInfos.getFilters().stream().map(FilterInfos::getName).collect(Collectors.joining(", "));
                createReport(subReportNode, "network.modification.allFiltersWrong", Map.of("filterNames", filterNames), TypedValue.WARN_SEVERITY);
            } else {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.equipmentByFilterDeleted")
                        .withUntypedValue("nbEquipments", (long) identifiableAttributes.size())
                        .withUntypedValue("type", modificationInfos.getEquipmentType().name())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
                // Report node is pushed to network instance to allow deletion logs from other libraries to be added
                network.getReportNodeContext().pushReportNode(subReportNode);
                applyFilterDeletion(network, subReportNode, identifiableAttributes);
            }
        }
    }

    @Override
    public String getName() {
        return "ByFilterDeletion";
    }

    private void applyFilterDeletion(Network network, ReportNode subReportNode, Set<IdentifiableAttributes> identifiableAttributes) {
        IdentifiableType identifiableType = modificationInfos.getEquipmentType();
        if (CONNECTABLE_TYPES.contains(identifiableType)) {
            identifiableAttributes.forEach(identifiableAttribute -> new RemoveFeederBay(identifiableAttribute.getId()).apply(network, true, subReportNode));
        } else if (identifiableType == IdentifiableType.VOLTAGE_LEVEL) {
            identifiableAttributes.forEach(identifiableAttribute -> new RemoveVoltageLevel(identifiableAttribute.getId()).apply(network, true, subReportNode));
        } else if (identifiableType == IdentifiableType.SUBSTATION) {
            identifiableAttributes.forEach(identifiableAttribute -> new RemoveSubstationBuilder().withSubstationId(identifiableAttribute.getId()).build().apply(network, true, subReportNode));
        } else if (identifiableType == IdentifiableType.HVDC_LINE) {
            identifiableAttributes.forEach(identifiableAttribute -> removeHvdcLine(network, subReportNode, identifiableAttribute));
        } else {
            throw NetworkModificationException.createEquipmentTypeUnknown(identifiableType.name());
        }
    }

    private void removeHvdcLine(Network network, ReportNode subReportNode, IdentifiableAttributes identifiableAttribute) {
        HvdcLine hvdcLine = (HvdcLine) ModificationUtils.getInstance().getEquipmentByIdentifiableType(network, modificationInfos.getEquipmentType(), identifiableAttribute.getId());
        if (hvdcLine != null) {
            HvdcConverterStation<?> converterStation1 = hvdcLine.getConverterStation1();
            HvdcConverterStation<?> converterStation2 = hvdcLine.getConverterStation2();
            if (converterStation1.getHvdcType() == HvdcConverterStation.HvdcType.LCC || converterStation2.getHvdcType() == HvdcConverterStation.HvdcType.LCC) {
                subReportNode.newReportNode()
                        .withMessageTemplate("network.modification.SCNotRemoved")
                        .withUntypedValue("id", identifiableAttribute.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            }
        }
        new RemoveHvdcLineBuilder().withHvdcLineId(identifiableAttribute.getId()).build().apply(network, true, subReportNode);
    }
}
