/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.mapper;

import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.model.ModificationModel;
import org.mapstruct.AfterMapping;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Named;
import org.mapstruct.SubclassExhaustiveStrategy;
import org.mapstruct.SubclassMapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(subclassExhaustiveStrategy = SubclassExhaustiveStrategy.RUNTIME_EXCEPTION)
public interface ModificationModelMapper {
    ModificationModelMapper INSTANCE = Mappers.getMapper(ModificationModelMapper.class);

    @SubclassMapping(source = org.gridsuite.modification.dto.BalancesAdjustmentModificationInfos.class, target = org.gridsuite.modification.model.BalancesAdjustmentModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.BatteryCreationInfos.class, target = org.gridsuite.modification.model.BatteryCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.BatteryModificationInfos.class, target = org.gridsuite.modification.model.BatteryModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.ByFilterDeletionInfos.class, target = org.gridsuite.modification.model.ByFilterDeletionModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.ByFormulaModificationInfos.class, target = org.gridsuite.modification.model.ByFormulaModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.CompositeModificationInfos.class, target = org.gridsuite.modification.model.CompositeModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.ConverterStationCreationInfos.class, target = org.gridsuite.modification.model.ConverterStationCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.ConverterStationModificationInfos.class, target = org.gridsuite.modification.model.ConverterStationModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.CreateCouplingDeviceInfos.class, target = org.gridsuite.modification.model.CreateCouplingDeviceModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.CreateVoltageLevelSectionInfos.class, target = org.gridsuite.modification.model.CreateVoltageLevelSectionModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.CreateVoltageLevelTopologyInfos.class, target = org.gridsuite.modification.model.CreateVoltageLevelTopologyModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.DeleteAttachingLineInfos.class, target = org.gridsuite.modification.model.DeleteAttachingLineModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.DeleteVoltageLevelOnLineInfos.class, target = org.gridsuite.modification.model.DeleteVoltageLevelOnLineModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.EquipmentAttributeModificationInfos.class, target = org.gridsuite.modification.model.EquipmentAttributeModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.EquipmentDeletionInfos.class, target = org.gridsuite.modification.model.EquipmentDeletionModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.GenerationDispatchInfos.class, target = org.gridsuite.modification.model.GenerationDispatchModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.GeneratorCreationInfos.class, target = org.gridsuite.modification.model.GeneratorCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.GeneratorModificationInfos.class, target = org.gridsuite.modification.model.GeneratorModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.GeneratorScalingInfos.class, target = org.gridsuite.modification.model.GeneratorScalingModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.GroovyScriptInfos.class, target = org.gridsuite.modification.model.GroovyScriptModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LccConverterStationCreationInfos.class, target = org.gridsuite.modification.model.LccConverterStationCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LccConverterStationModificationInfos.class, target = org.gridsuite.modification.model.LccConverterStationModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LccCreationInfos.class, target = org.gridsuite.modification.model.LccCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LccModificationInfos.class, target = org.gridsuite.modification.model.LccModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.tabular.LimitSetsTabularModificationInfos.class, target = org.gridsuite.modification.model.tabular.LimitSetsTabularModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LineAttachToVoltageLevelInfos.class, target = org.gridsuite.modification.model.LineAttachToVoltageLevelModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LineCreationInfos.class, target = org.gridsuite.modification.model.LineCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LineModificationInfos.class, target = org.gridsuite.modification.model.LineModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LinesAttachToSplitLinesInfos.class, target = org.gridsuite.modification.model.LinesAttachToSplitLinesModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LineSplitWithVoltageLevelInfos.class, target = org.gridsuite.modification.model.LineSplitWithVoltageLevelModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LoadCreationInfos.class, target = org.gridsuite.modification.model.LoadCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LoadModificationInfos.class, target = org.gridsuite.modification.model.LoadModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LoadScalingInfos.class, target = org.gridsuite.modification.model.LoadScalingModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.ModificationByAssignmentInfos.class, target = org.gridsuite.modification.model.ModificationByAssignmentModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.MoveVoltageLevelFeederBaysInfos.class, target = org.gridsuite.modification.model.MoveVoltageLevelFeederBaysModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.OperatingStatusModificationInfos.class, target = org.gridsuite.modification.model.OperatingStatusModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.ShuntCompensatorCreationInfos.class, target = org.gridsuite.modification.model.ShuntCompensatorCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.ShuntCompensatorModificationInfos.class, target = org.gridsuite.modification.model.ShuntCompensatorModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.StaticVarCompensatorCreationInfos.class, target = org.gridsuite.modification.model.StaticVarCompensatorCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.SubstationCreationInfos.class, target = org.gridsuite.modification.model.SubstationCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.SubstationModificationInfos.class, target = org.gridsuite.modification.model.SubstationModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.tabular.TabularCreationInfos.class, target = org.gridsuite.modification.model.tabular.TabularCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.tabular.TabularModificationInfos.class, target = org.gridsuite.modification.model.tabular.TabularModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.TwoWindingsTransformerCreationInfos.class, target = org.gridsuite.modification.model.TwoWindingsTransformerCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.TwoWindingsTransformerModificationInfos.class, target = org.gridsuite.modification.model.TwoWindingsTransformerModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.VoltageInitModificationInfos.class, target = org.gridsuite.modification.model.VoltageInitModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.VoltageLevelCreationInfos.class, target = org.gridsuite.modification.model.VoltageLevelCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.VoltageLevelModificationInfos.class, target = org.gridsuite.modification.model.VoltageLevelModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.VoltageLevelTopologyModificationInfos.class, target = org.gridsuite.modification.model.VoltageLevelTopologyModificationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.VscCreationInfos.class, target = org.gridsuite.modification.model.VscCreationModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.VscModificationInfos.class, target = org.gridsuite.modification.model.VscModificationModel.class)
    ModificationModel toModel(ModificationInfos infos);

    @SubclassMapping(source = org.gridsuite.modification.dto.HvdcLccDeletionInfos.class, target = org.gridsuite.modification.model.HvdcLccDeletionModel.class)
    org.gridsuite.modification.model.AbstractEquipmentDeletionModel map(org.gridsuite.modification.dto.AbstractEquipmentDeletionInfos infos);

    @SubclassMapping(source = org.gridsuite.modification.dto.byfilter.assignment.BooleanAssignmentInfos.class, target = org.gridsuite.modification.model.byfilter.assignment.BooleanAssignmentModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.byfilter.assignment.EnumAssignmentInfos.class, target = org.gridsuite.modification.model.byfilter.assignment.EnumAssignmentModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos.class, target = org.gridsuite.modification.model.byfilter.assignment.DoubleAssignmentModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.byfilter.assignment.IntegerAssignmentInfos.class, target = org.gridsuite.modification.model.byfilter.assignment.IntegerAssignmentModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.byfilter.assignment.PropertyAssignmentInfos.class, target = org.gridsuite.modification.model.byfilter.assignment.PropertyAssignmentModel.class)
    org.gridsuite.modification.model.byfilter.assignment.AssignmentModel map(org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos infos);

    @SubclassMapping(source = org.gridsuite.modification.dto.LccShuntCompensatorInfos.class, target = org.gridsuite.modification.model.LccShuntCompensatorModel.class)
    @SubclassMapping(source = org.gridsuite.modification.dto.LccShuntCompensatorModificationInfos.class, target = org.gridsuite.modification.model.LccShuntCompensatorModificationModel.class)
    org.gridsuite.modification.model.AbstractLccShuntCompensatorModel map(org.gridsuite.modification.dto.AbstractLccShuntCompensatorInfos infos);

    // A composite only carries the modifications that must be applied: inactive/stashed children are filtered out here.
    @Mapping(target = "modificationsInfos", source = "modificationsInfos", qualifiedByName = "activeModificationModels")
    org.gridsuite.modification.model.CompositeModificationModel toModel(org.gridsuite.modification.dto.CompositeModificationInfos infos);

    // Exposed for white-box tests that exercise model-side helpers (e.g. MoveVoltageLevelFeederBays.getTerminal).
    org.gridsuite.modification.model.MoveFeederBayModel map(org.gridsuite.modification.dto.MoveFeederBayInfos infos);

    @Named("activeModificationModels")
    default List<ModificationModel> activeModificationModels(List<ModificationInfos> modifications) {
        if (modifications == null) {
            return null;
        }
        return modifications.stream()
                .filter(modification -> Boolean.TRUE.equals(modification.getActivated())
                        && Boolean.FALSE.equals(modification.getStashed()))
                .map(this::toModel)
                .toList();
    }

    // Explicit AttributeModification converters, reused by the @AfterMapping hooks below.
    org.gridsuite.modification.model.AttributeModification<Double> mapDoubleAttribute(org.gridsuite.modification.dto.AttributeModification<Double> source);

    org.gridsuite.modification.model.AttributeModification<Boolean> mapBooleanAttribute(org.gridsuite.modification.dto.AttributeModification<Boolean> source);

    // The following fields are skipped by MapStruct's default property matching because of the JavaBeans/Lombok
    // accessor-naming quirk for single-lowercase-letter prefixes (pMeasurement.., qPercent) and "is"-prefixed
    // boolean fields. They are copied explicitly here.
    @AfterMapping
    default void mapInjectionMeasurements(org.gridsuite.modification.dto.InjectionModificationInfos source,
            @MappingTarget org.gridsuite.modification.model.InjectionModificationModel target) {
        target.setPMeasurementValue(mapDoubleAttribute(source.getPMeasurementValue()));
        target.setPMeasurementValidity(mapBooleanAttribute(source.getPMeasurementValidity()));
        target.setQMeasurementValue(mapDoubleAttribute(source.getQMeasurementValue()));
        target.setQMeasurementValidity(mapBooleanAttribute(source.getQMeasurementValidity()));
    }

    @AfterMapping
    default void mapGeneratorModificationQPercent(org.gridsuite.modification.dto.GeneratorModificationInfos source,
            @MappingTarget org.gridsuite.modification.model.GeneratorModificationModel target) {
        target.setQPercent(mapDoubleAttribute(source.getQPercent()));
    }

    @AfterMapping
    default void mapGeneratorCreationQPercent(org.gridsuite.modification.dto.GeneratorCreationInfos source,
            @MappingTarget org.gridsuite.modification.model.GeneratorCreationModel target) {
        target.setQPercent(source.getQPercent());
    }

    @AfterMapping
    default void mapVoltageLevelSectionFlags(org.gridsuite.modification.dto.CreateVoltageLevelSectionInfos source,
            @MappingTarget org.gridsuite.modification.model.CreateVoltageLevelSectionModel target) {
        target.setAfterBusbarSectionId(source.isAfterBusbarSectionId());
        target.setAllBusbars(source.isAllBusbars());
        target.setSwitchOpen(source.isSwitchOpen());
    }
}
