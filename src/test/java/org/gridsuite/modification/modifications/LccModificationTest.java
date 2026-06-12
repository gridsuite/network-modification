/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.computation.ComputationManager;
import com.powsybl.computation.local.LocalComputationManager;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

class LccModificationTest extends AbstractInjectionModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createWithLcc(networkUuid);
    }

    @Override
    protected ModificationModel buildModification() {
        return LccModificationModel.builder()
            .equipmentId("hvdcLine")
            .equipmentName(new AttributeModification<>("newV1Lcc", OperationType.SET))
            .nominalV(new AttributeModification<>(40., OperationType.SET))
            .maxP(new AttributeModification<>(50., OperationType.SET))
            .r(new AttributeModification<>(5., OperationType.SET))
            .activePowerSetpoint(new AttributeModification<>(5., OperationType.SET))
            .convertersMode(new AttributeModification<>(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, OperationType.SET))
            .converterStation1(buildLccConverterStationModificationInfos1())
            .converterStation2(buildLccConverterStationModificationInfos2())
            .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    private static LccConverterStationModificationModel buildLccConverterStationModificationInfos1() {

        LccShuntCompensatorModificationModel filter1 = new LccShuntCompensatorModificationModel("ShuntStation1Id1",
            "ShuntStation1Name1", 0.1, true, false);

        LccShuntCompensatorModificationModel filter2 = new LccShuntCompensatorModificationModel("ShuntStation1Id2",
            "ShuntStation1Name2", 0.1, false, false);

        return LccConverterStationModificationModel.builder()
            .equipmentId("v1lcc")
            .equipmentName(new AttributeModification<>("lcc1Station1Name", OperationType.SET))
            .lossFactor(new AttributeModification<>(40.f, OperationType.SET))
            .powerFactor(new AttributeModification<>(1.f, OperationType.SET))
            .shuntCompensatorsOnSide(List.of(filter1, filter2))
            .build();
    }

    private static LccConverterStationModificationModel buildLccConverterStationModificationInfos2() {
        return LccConverterStationModificationModel.builder()
            .equipmentId("v2lcc")
            .equipmentName(new AttributeModification<>("lcc2Station2Name", OperationType.SET))
            .lossFactor(new AttributeModification<>(40.f, OperationType.SET))
            .powerFactor(new AttributeModification<>(1.f, OperationType.SET))
            .shuntCompensatorsOnSide(List.of())
            .build();
    }

    private LccConverterStationModificationModel buildLccConverterStationModificationInfos2WithNullInfos() {
        return LccConverterStationModificationModel.builder()
            .equipmentId("v2lcc")
            .equipmentName(new AttributeModification<>("newV2lcc", OperationType.SET))
            .lossFactor(null)
            .powerFactor(null)
            .shuntCompensatorsOnSide(List.of())
            .build();
    }

    public LccConverterStationCreationModel buildLccConverterStationCreationInfos1() {
        LccShuntCompensatorModel filter1 = new LccShuntCompensatorModel("shuntId1", "shuntName1", 110.0, true);
        LccShuntCompensatorModel filter2 = new LccShuntCompensatorModel("shuntId2", "shuntName2", 100.0, false);

        return LccConverterStationCreationModel.builder()
            .equipmentId("stationId1")
            .equipmentName("stationName1")
            .lossFactor(40F)
            .powerFactor(1F)
            .voltageLevelId("v1")
            .busOrBusbarSectionId("1.1")
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .shuntCompensatorsOnSide(List.of(filter1, filter2))
            .build();
    }

    public LccConverterStationModificationModel buildLccConverterStationModificationInfosWithShunts(List<LccShuntCompensatorModificationModel> shuntInfos) {
        return LccConverterStationModificationModel.builder()
            .equipmentId("stationId1")
            .equipmentName(null)
            .lossFactor(null)
            .powerFactor(null)
            .shuntCompensatorsOnSide(shuntInfos)
            .build();
    }

    public LccConverterStationCreationModel buildLccConverterStationCreationInfos2() {
        LccShuntCompensatorModel filter1 = new LccShuntCompensatorModel("shunt2Id1", "shunt2Name1", 90.0, true);
        LccShuntCompensatorModel filter2 = new LccShuntCompensatorModel("shunt2Id2", "shunt2Name2", 100.0, false);

        return LccConverterStationCreationModel.builder()
            .equipmentId("stationId2")
            .equipmentName("stationName2")
            .lossFactor(40F)
            .powerFactor(1F)
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1A")
            .connectionName("bottom")
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .shuntCompensatorsOnSide(List.of(filter1, filter2))
            .build();
    }

    private void buildAndApplyLccCreationWithShuntCompensator(Network network) {
        LccCreationModel creationInfos = LccCreationModel.builder()
            .equipmentId("lcc1")
            .equipmentName("lcc1Name")
            .nominalV(39.)
            .r(4.)
            .maxP(56.)
            .convertersMode(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER)
            .activePowerSetpoint(5.)
            .converterStation1(buildLccConverterStationCreationInfos1())
            .converterStation2(buildLccConverterStationCreationInfos2())
            .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();

        LccCreation lccCreation = new LccCreation(creationInfos);
        lccCreation.apply(network);
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getHvdcLine("hvdcLine"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getLccConverterStationStream()
            .filter(converterStation -> converterStation.getId().equals("v1lcc")).count());
        HvdcLine hvdcLine = getNetwork().getHvdcLine("hvdcLine");
        assertEquals(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, hvdcLine.getConvertersMode());
        assertEquals(40., hvdcLine.getNominalV(), 0);
        assertEquals(5., hvdcLine.getR(), 0);
        assertEquals(5., hvdcLine.getActivePowerSetpoint(), 0);
        assertEquals(50., hvdcLine.getMaxP(), 0);
        LccConverterStation lccConverterStation1 = (LccConverterStation) hvdcLine.getConverterStation1();
        assertNotNull(lccConverterStation1);
        assertEquals(40.f, lccConverterStation1.getLossFactor(), 0);
        assertEquals(1.f, lccConverterStation1.getPowerFactor(), 0);
        assertEquals("v1", lccConverterStation1.getTerminal().getVoltageLevel().getId());
    }

    @Test
    void testModificationWithNullValues() throws Exception {
        var networkUuid = UUID.randomUUID();
        Network networkWithoutExt = NetworkCreation.createWithLcc(networkUuid);
        LccModificationModel modificationInfos = (LccModificationModel) buildModification();
        modificationInfos.setEquipmentName(null);
        modificationInfos.setNominalV(null);
        modificationInfos.setR(null);
        modificationInfos.setMaxP(null);
        modificationInfos.setConvertersMode(null);
        modificationInfos.setActivePowerSetpoint(null);
        modificationInfos.setConverterStation1(null);
        modificationInfos.setConverterStation2(null);

        LccConverterStationModificationModel converterStationModificationInfos = buildLccConverterStationModificationInfos2WithNullInfos();
        modificationInfos.setConverterStation2(converterStationModificationInfos);

        LccModification lccModification = new LccModification(modificationInfos);
        assertEquals("LCC_Modification", lccModification.getName());
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        assertDoesNotThrow(() -> lccModification.check(networkWithoutExt));
        lccModification.apply(networkWithoutExt, true, computationManager, subReporter);

        assertEquals(1, getNetwork().getVoltageLevel("v1").getLccConverterStationStream()
            .filter(converterStation -> converterStation.getId().equals("v1lcc")).count());
        HvdcLine hvdcLine = networkWithoutExt.getHvdcLine("hvdcLine");
        assertEquals(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, hvdcLine.getConvertersMode());
        assertEquals(225., hvdcLine.getNominalV(), 0);
        assertEquals(1., hvdcLine.getR(), 0);
        assertEquals(100., hvdcLine.getMaxP(), 0);
        assertEquals(500., hvdcLine.getActivePowerSetpoint(), 0);

        LccConverterStation lccConverterStation1 = (LccConverterStation) hvdcLine.getConverterStation1();
        assertNotNull(lccConverterStation1);
        assertEquals(0.f, lccConverterStation1.getLossFactor(), 0);
        assertEquals(0.f, lccConverterStation1.getPowerFactor(), 0);
        assertEquals("v1", lccConverterStation1.getTerminal().getVoltageLevel().getId());

        LccConverterStation lccConverterStation2 = (LccConverterStation) hvdcLine.getConverterStation2();
        assertNotNull(lccConverterStation2);
        assertEquals("v1lcc", lccConverterStation1.getOptionalName().orElse(""));
        assertEquals(0.f, lccConverterStation2.getLossFactor(), 0);
        assertEquals(0.f, lccConverterStation2.getPowerFactor(), 0);
    }

    @Test
    void testModificationWithShuntCompensatorsOnSide() {
        var networkUuid = UUID.randomUUID();
        Network networkWithoutExt = NetworkCreation.createWithLcc(networkUuid);
        buildAndApplyLccCreationWithShuntCompensator(networkWithoutExt);

        LccModificationModel modificationInfos = (LccModificationModel) buildModification();

        LccShuntCompensatorModificationModel shuntInfos = new LccShuntCompensatorModificationModel("shuntId1",
            "newName", 50.0, false, false);
        LccShuntCompensatorModificationModel shuntInfos2 = new LccShuntCompensatorModificationModel("shuntId2",
            null, null, null, true);

        // does not exist : will do nothing
        LccShuntCompensatorModificationModel shuntInfos3 = new LccShuntCompensatorModificationModel("shuntId3",
            "newName2", 60.0, null, false);

        LccConverterStationModificationModel converterStationModificationInfos =
            buildLccConverterStationModificationInfosWithShunts(List.of(shuntInfos, shuntInfos2, shuntInfos3));

        modificationInfos.setConverterStation1(converterStationModificationInfos);
        LccModification lccModification = new LccModification(modificationInfos);
        lccModification.apply(networkWithoutExt);

        ShuntCompensator shuntCompensator1 = networkWithoutExt.getShuntCompensator("shuntId1");
        assertNotNull(shuntCompensator1);
        assertEquals("newName", shuntCompensator1.getOptionalName().orElse(""));

        VoltageLevel v1 = networkWithoutExt.getVoltageLevel("v1");
        assertEquals(50.0, shuntCompensator1.getModel(ShuntCompensatorLinearModel.class).getBPerSection() * Math.pow(v1.getNominalV(), 2));

        //check if shuntCompensator was deleted from network
        ShuntCompensator shuntCompensator2 = networkWithoutExt.getShuntCompensator("shuntId2");
        assertNull(shuntCompensator2);
    }

    @Override
    protected void checkModification() {
        // Nothing special to check
    }
}
