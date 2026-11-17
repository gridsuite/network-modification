/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Injection;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import com.powsybl.iidm.network.ThreeWindingsTransformer;
import com.powsybl.iidm.network.TieLine;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.reducer.IdentifierNetworkPredicate;
import com.powsybl.iidm.reducer.NetworkReducer;
import com.powsybl.iidm.reducer.NetworkReducerObserver;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.filter.Filter;
import org.gridsuite.modification.ModificationType;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;

import static org.gridsuite.modification.modifications.byfilter.AbstractModificationByAssignment.VALUE_KEY_EQUIPMENT_COUNT;

/**
 * The network reduction is relying on a NetworkPredicate instance, to define an area of interest (i.e., a list of equipments to keep in the network after the reduction).
 * The equipments outside this area are removed, and the lines, transformers and HVDC lines connecting voltage levels inside and outside this area will be replaced by
 * injections (loads rather than boundary lines in our case).
 * In this modification, the network predicate is the entire voltage level list minus the ones evaluated by the user-defined filters.
 *
 * @author David Braquart <david.braquart at rte-france.com>
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class NetworkReduction extends AbstractModification {
    private static final String VALUE_KEY_FILTER_IDENTIFIER = "filterIdentifier";
    private static final String REPORT_KEY_FILTER_EVALUATION = "network.modification.filterEvaluation";
    private static final String REPORT_KEY_FILTERS_EVALUATION = "network.modification.filtersEvaluation";
    private static final String REPORT_KEY_FILTER_EVALUATION_RESULT = "network.modification.filterEvaluationResult";
    private static final String REPORT_KEY_NO_EQUIPMENT_TO_REMOVE = "network.modification.byFilterDeletion.noEquipmentToRemove";
    private static final String REPORT_KEY_REDUCTION_APPLIED = "network.modification.networkReduction.applied";

    // the voltage levels to exclude from the network
    private List<Filter> filters;

    @Builder
    public NetworkReduction(List<Filter> filters) {
        this.filters = filters;
    }

    @Override
    public void apply(Network network, ReportNode reportNode) {
        ReportNode filtersContainer = reportNode.newReportNode()
                .withMessageTemplate(REPORT_KEY_FILTERS_EVALUATION)
                .add();

        Collection<Identifiable<?>> excludedVoltageLevels = getEquipmentsFromFilters(network, filtersContainer);

        if (excludedVoltageLevels.isEmpty()) {
            // If filters do not evaluate to any equipment, we just add a warn report
            reportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_NO_EQUIPMENT_TO_REMOVE)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .add();
        } else {
            filtersContainer.newReportNode()
                    .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION_RESULT)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_COUNT, excludedVoltageLevels.size())
                    .add();
            ReportNode reductionReport = reportNode.newReportNode()
                    .withMessageTemplate(REPORT_KEY_REDUCTION_APPLIED)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();

            List<String> retainedVoltageLevelIds = network.getVoltageLevelStream()
                    .filter(voltageLevel -> !excludedVoltageLevels.contains(voltageLevel))
                    .map(VoltageLevel::getId)
                    .toList();

            NetworkReducer reducer = NetworkReducer.builder()
                    .withBoundaryLines(false) // means replacement injections are exclusively fictitious loads
                    .withNetworkPredicate(new IdentifierNetworkPredicate(retainedVoltageLevelIds))
                    .withObservers(new ReductionReportObserver(reductionReport))
                    .build();
            reducer.reduce(network);
        }
    }

    @Override
    public String getName() {
        return ModificationType.NETWORK_REDUCTION.name();
    }

    private Collection<Identifiable<?>> getEquipmentsFromFilters(Network network, ReportNode filtersContainer) {
        Collection<Identifiable<?>> evaluatedEquipments = new LinkedHashSet<>();
        for (int i = 0; i < filters.size(); i++) {
            Filter filter = getFilters().get(i);
            String filterIdentifier = filter.getName() == null ? Integer.toString(i + 1) : filter.getName();
            ReportNode filterContainer = filtersContainer.newReportNode()
                    .withMessageTemplate(REPORT_KEY_FILTER_EVALUATION)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .withUntypedValue(VALUE_KEY_FILTER_IDENTIFIER, filterIdentifier)
                    .add();
            evaluatedEquipments.addAll(filter.evaluate(network, filterContainer));
        }
        return evaluatedEquipments;
    }

    private static final class ReductionReportObserver implements NetworkReducerObserver {
        private static final String VALUE_KEY_SUBSTATION_ID = "substationId";
        private static final String VALUE_KEY_VOLTAGE_LEVEL_ID = "voltageLevelId";
        private static final String VALUE_KEY_LINE_ID = "lineId";
        private static final String VALUE_KEY_TIE_LINE_ID = "tieLineId";
        private static final String VALUE_KEY_TRANSFORMER_ID = "transformerId";
        private static final String VALUE_KEY_HVDC_LINE_ID = "hvdcLineId";

        private final ReportNode reportNode;

        private ReductionReportObserver(ReportNode reportNode) {
            this.reportNode = reportNode;
        }

        @Override
        public void substationRemoved(Substation substation) {
            reportRemoval("substationRemoved", VALUE_KEY_SUBSTATION_ID, substation.getId());
        }

        @Override
        public void voltageLevelRemoved(VoltageLevel voltageLevel) {
            reportRemoval("voltageLevelRemoved", VALUE_KEY_VOLTAGE_LEVEL_ID, voltageLevel.getId());
        }

        @Override
        public void lineReplaced(Line line, Injection injection) {
            reportReplacement("lineReplaced", VALUE_KEY_LINE_ID, line.getId(), injection);
        }

        @Override
        public void lineRemoved(Line line) {
            reportRemoval("lineRemoved", VALUE_KEY_LINE_ID, line.getId());
        }

        @Override
        public void tieLineRemoved(TieLine tieLine) {
            reportRemoval("tieLineRemoved", VALUE_KEY_TIE_LINE_ID, tieLine.getId());
        }

        @Override
        public void transformerReplaced(TwoWindingsTransformer transformer, Injection injection) {
            reportReplacement("twoWindingsTransformerReplaced", VALUE_KEY_TRANSFORMER_ID, transformer.getId(), injection);
        }

        @Override
        public void transformerRemoved(TwoWindingsTransformer transformer) {
            reportRemoval("twoWindingsTransformerRemoved", VALUE_KEY_TRANSFORMER_ID, transformer.getId());
        }

        @Override
        public void transformerReplaced(ThreeWindingsTransformer transformer, Injection injection) {
            reportReplacement("threeWindingsTransformerReplaced", VALUE_KEY_TRANSFORMER_ID, transformer.getId(), injection);
        }

        @Override
        public void transformerRemoved(ThreeWindingsTransformer transformer) {
            reportRemoval("threeWindingsTransformerRemoved", VALUE_KEY_TRANSFORMER_ID, transformer.getId());
        }

        @Override
        public void hvdcLineReplaced(HvdcLine hvdcLine, Injection injection) {
            reportReplacement("hvdcLineReplaced", VALUE_KEY_HVDC_LINE_ID, hvdcLine.getId(), injection);
        }

        @Override
        public void hvdcLineRemoved(HvdcLine hvdcLine) {
            reportRemoval("hvdcLineRemoved", VALUE_KEY_HVDC_LINE_ID, hvdcLine.getId());
        }

        private void reportRemoval(String event, String equipmentKey, String equipmentId) {
            reportNode.newReportNode()
                    .withMessageTemplate("network.modification.networkReduction." + event)
                    .withUntypedValue(equipmentKey, equipmentId)
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .add();
        }

        private void reportReplacement(String event, String equipmentKey, String equipmentId, Injection<?> injection) {
            reportNode.newReportNode()
                    .withMessageTemplate("network.modification.networkReduction." + event)
                    .withUntypedValue(equipmentKey, equipmentId)
                    .withUntypedValue("injectionId", injection.getId())
                    .withUntypedValue("injectionType", injection.getType().name())
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .add();
        }
    }
}
