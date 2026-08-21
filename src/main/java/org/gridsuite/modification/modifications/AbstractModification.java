/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.computation.ComputationManager;
import com.powsybl.iidm.modification.AbstractNetworkModification;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.Network;
import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.modifications.byfilter.ByFormulaModification;
import org.gridsuite.modification.modifications.byfilter.ModificationByAssignment;
import org.gridsuite.modification.modifications.tabular.TabularCreation;
import org.gridsuite.modification.modifications.tabular.TabularModification;

/**
 * Abstract modification.
 *
 * <p>Concrete modifications are JSON-serializable/deserializable: the Jackson type discriminator is
 * {@link #getName()} (see {@code @JsonTypeInfo}) and each concrete subclass must be registered in
 * the {@code @JsonSubTypes} registry of this class.
 *
 * <p>The injected application context ({@code filterService}, {@code loadFlowService}) is not serialized.
 * After deserialization, {@link #initApplicationContext(IFilterService, ILoadFlowService)} must be called
 * again with valid services before the modification can be applied.
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@JsonTypeInfo(
        use = JsonTypeInfo.Id.NAME,
        property = "name",
        include = JsonTypeInfo.As.EXISTING_PROPERTY
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = ByFormulaModification.class, name = "BY_FORMULA_MODIFICATION"),
    @JsonSubTypes.Type(value = ModificationByAssignment.class, name = "MODIFICATION_BY_ASSIGNMENT"),
    @JsonSubTypes.Type(value = TabularCreation.class, name = "TABULAR_CREATION"),
    @JsonSubTypes.Type(value = TabularModification.class, name = "TABULAR_MODIFICATION"),
    @JsonSubTypes.Type(value = BalancesAdjustmentModification.class, name = "BALANCES_ADJUSTMENT_MODIFICATION"),
    @JsonSubTypes.Type(value = BatteryCreation.class, name = "BATTERY_CREATION"),
    @JsonSubTypes.Type(value = BatteryModification.class, name = "BATTERY_MODIFICATION"),
    @JsonSubTypes.Type(value = ByFilterDeletion.class, name = "BY_FILTER_DELETION"),
    @JsonSubTypes.Type(value = CompositeModification.class, name = "COMPOSITE_MODIFICATION"),
    @JsonSubTypes.Type(value = CreateCouplingDevice.class, name = "CREATE_COUPLING_DEVICE"),
    @JsonSubTypes.Type(value = CreateVoltageLevelSection.class, name = "CREATE_VOLTAGE_LEVEL_SECTION"),
    @JsonSubTypes.Type(value = CreateVoltageLevelTopology.class, name = "CREATE_VOLTAGE_LEVEL_TOPOLOGY"),
    @JsonSubTypes.Type(value = DeleteAttachingLine.class, name = "DELETE_ATTACHING_LINE"),
    @JsonSubTypes.Type(value = DeleteVoltageLevelOnLine.class, name = "DELETE_VOLTAGE_LEVEL_ON_LINE"),
    @JsonSubTypes.Type(value = EquipmentAttributeModification.class, name = "EQUIPMENT_ATTRIBUTE_MODIFICATION"),
    @JsonSubTypes.Type(value = EquipmentDeletion.class, name = "EQUIPMENT_DELETION"),
    @JsonSubTypes.Type(value = GenerationDispatch.class, name = "GENERATION_DISPATCH"),
    @JsonSubTypes.Type(value = GeneratorCreation.class, name = "GENERATOR_CREATION"),
    @JsonSubTypes.Type(value = GeneratorModification.class, name = "GENERATOR_MODIFICATION"),
    @JsonSubTypes.Type(value = GeneratorScaling.class, name = "GENERATOR_SCALING"),
    @JsonSubTypes.Type(value = GroovyScript.class, name = "GROOVY_SCRIPT"),
    @JsonSubTypes.Type(value = LccCreation.class, name = "LCC_CREATION"),
    @JsonSubTypes.Type(value = LccModification.class, name = "LCC_MODIFICATION"),
    @JsonSubTypes.Type(value = LineAttachToVoltageLevel.class, name = "LINE_ATTACH_TO_VOLTAGE_LEVEL"),
    @JsonSubTypes.Type(value = LineCreation.class, name = "LINE_CREATION"),
    @JsonSubTypes.Type(value = LineModification.class, name = "LINE_MODIFICATION"),
    @JsonSubTypes.Type(value = LinesAttachToSplitLines.class, name = "LINES_ATTACH_TO_SPLIT_LINES"),
    @JsonSubTypes.Type(value = LineSplitWithVoltageLevel.class, name = "LINE_SPLIT_WITH_VOLTAGE_LEVEL"),
    @JsonSubTypes.Type(value = LoadCreation.class, name = "LOAD_CREATION"),
    @JsonSubTypes.Type(value = LoadModification.class, name = "LOAD_MODIFICATION"),
    @JsonSubTypes.Type(value = LoadScaling.class, name = "LOAD_SCALING"),
    @JsonSubTypes.Type(value = ModificationReference.class, name = "MODIFICATION_REFERENCE"),
    @JsonSubTypes.Type(value = MoveVoltageLevelFeederBays.class, name = "MOVE_VOLTAGE_LEVEL_FEEDER_BAYS"),
    @JsonSubTypes.Type(value = OperatingStatusModification.class, name = "OPERATING_STATUS_MODIFICATION"),
    @JsonSubTypes.Type(value = ShuntCompensatorCreation.class, name = "SHUNT_COMPENSATOR_CREATION"),
    @JsonSubTypes.Type(value = ShuntCompensatorModification.class, name = "SHUNT_COMPENSATOR_MODIFICATION"),
    @JsonSubTypes.Type(value = StaticVarCompensatorCreation.class, name = "STATIC_VAR_COMPENSATOR_CREATION"),
    @JsonSubTypes.Type(value = SubstationCreation.class, name = "SUBSTATION_CREATION"),
    @JsonSubTypes.Type(value = SubstationModification.class, name = "SUBSTATION_MODIFICATION"),
    @JsonSubTypes.Type(value = TwoWindingsTransformerCreation.class, name = "TWO_WINDINGS_TRANSFORMER_CREATION"),
    @JsonSubTypes.Type(value = TwoWindingsTransformerModification.class, name = "TWO_WINDINGS_TRANSFORMER_MODIFICATION"),
    @JsonSubTypes.Type(value = VoltageInitModification.class, name = "VOLTAGE_INIT_MODIFICATION"),
    @JsonSubTypes.Type(value = VoltageLevelCreation.class, name = "VOLTAGE_LEVEL_CREATION"),
    @JsonSubTypes.Type(value = VoltageLevelModification.class, name = "VOLTAGE_LEVEL_MODIFICATION"),
    @JsonSubTypes.Type(value = VoltageLevelTopologyModification.class, name = "VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION"),
    @JsonSubTypes.Type(value = VscCreation.class, name = "VSC_CREATION"),
    @JsonSubTypes.Type(value = VscModification.class, name = "VSC_MODIFICATION")
})
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public abstract class AbstractModification extends AbstractNetworkModification {

    @Override
    public void apply(Network network, NamingStrategy namingStrategy, boolean throwException, ComputationManager computationManager, ReportNode reportNode) {
        apply(network, reportNode);
    }

    public void check(Network network) throws NetworkModificationException {
        // To perform input data check before hypothesis apply. Nothing to check here
    }

    /**
     * Injects the application context services ({@code filterService}, {@code loadFlowService}) required
     * to {@link #apply(Network, ReportNode)}.
     *
     * <p>These services are not serialized with the modification: after deserialization, this method must
     * be called again with valid services before the modification can be applied.
     */
    public void initApplicationContext(IFilterService filterService, ILoadFlowService loadFlowService) {
        // To add some specific information
    }

    @Override
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public abstract String getName();
}
