package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.LccConverterStation;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ValidationException;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.dto.LccModificationInfos;
import org.gridsuite.modification.dto.LccShuntCompensatorInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.utils.NetworkCreation;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

public class LccModificationTest extends AbstractInjectionModificationTest{
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LccModificationInfos.builder()
            .equipmentId("v1lcc")
            .equipmentName(new AttributeModification<>("newV1Lcc", OperationType.SET))
            .nominalV(new AttributeModification<>(40., OperationType.SET))
            .maxP(new AttributeModification<>(50., OperationType.SET))
            .r(new AttributeModification<>(5., OperationType.SET))
            .activePowerSetpoint(new AttributeModification<>(5., OperationType.SET) )
            .convertersMode(new AttributeModification<>(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, OperationType.SET))
            .converterStation1(buildLccConverterStationModificationInfos1())
            .converterStation2(buildLccConverterStationModificationInfos2())
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    private static LccConverterStationModificationInfos buildLccConverterStationModificationInfos1() {
        var filter1 = LccShuntCompensatorInfos.builder()
            .id("ShuntStation1Id1")
            .name("ShuntStation1Name1")
            .maxQAtNominalV(0.1)
            .connectedToHvdc(true)
            .build();

        var filter2 = LccShuntCompensatorInfos.builder()
            .id("ShuntStation1Id2")
            .name("ShuntStation1Name2")
            .maxQAtNominalV(0.1)
            .connectedToHvdc(false)
            .build();

        return LccConverterStationModificationInfos.builder()
            .equipmentId("idConverter")
            .equipmentName(new AttributeModification<>("lcc1Station1Name", OperationType.SET))
            .lossFactor(new AttributeModification<>(40.f, OperationType.SET))
            .powerFactor(new AttributeModification<>(1.f, OperationType.SET))
            .voltageLevelId(new AttributeModification<>("v1", OperationType.SET))
            .busOrBusbarSectionId(new AttributeModification<>("bus1", OperationType.SET))
            .connectionName(new AttributeModification<>("top", OperationType.SET))
            .connectionDirection(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
            .shuntCompensatorsOnSide(List.of(filter1, filter2))
            .build();
    }

    private static LccConverterStationModificationInfos buildLccConverterStationModificationInfos2()
    {
        return LccConverterStationModificationInfos.builder()
            .equipmentId("idConverter")
            .equipmentName(new AttributeModification<>("lcc2Station2Name", OperationType.SET))
            .lossFactor(new AttributeModification<>(40.f, OperationType.SET))
            .powerFactor(new AttributeModification<>(1.f, OperationType.SET))
            .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
            .busOrBusbarSectionId(new AttributeModification<>("1.1", OperationType.SET))
            .connectionName(new AttributeModification<>("top", OperationType.SET))
            .connectionDirection(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
            .shuntCompensatorsOnSide(List.of())
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getHvdcLine("idLcc"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getLccConverterStationStream()
            .filter(converterStation -> converterStation.getId().equals("lcc1Station1Id")).count());
        assertEquals(1, getNetwork().getVoltageLevel("v2").getLccConverterStationStream()
            .filter(converterStation -> converterStation.getId().equals("lcc2Station2Id")).count());
        HvdcLine hvdcLine = getNetwork().getHvdcLine("idLcc");
        assertEquals(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, hvdcLine.getConvertersMode());
        assertEquals(40., hvdcLine.getNominalV(), 0);
        assertEquals(5., hvdcLine.getR(), 0);
        assertEquals(5., hvdcLine.getActivePowerSetpoint(), 0);
        assertEquals(50., hvdcLine.getMaxP(), 0);
        assertEquals(PROPERTY_VALUE, hvdcLine.getProperty(PROPERTY_NAME));
        LccConverterStation lccConverterStation1 = (LccConverterStation) hvdcLine.getConverterStation1();
        assertNotNull(lccConverterStation1);
        assertEquals(40.f, lccConverterStation1.getLossFactor(), 0);
        assertEquals(1.f, lccConverterStation1.getPowerFactor(), 0);
        assertEquals("v1", lccConverterStation1.getTerminal().getVoltageLevel().getId());
        LccConverterStation lccConverterStation2 = (LccConverterStation) hvdcLine.getConverterStation2();
        assertNotNull(lccConverterStation2);
        assertEquals(40.f, lccConverterStation2.getLossFactor(), 0);
        assertEquals(1.f, lccConverterStation2.getPowerFactor(), 0);
        assertEquals("v2", lccConverterStation2.getTerminal().getVoltageLevel().getId());
    }

    @Override
    protected void checkModification() {
        LccModificationInfos modificationInfos = (LccModificationInfos) buildModification();

        // Unset an attribute that should not be null
        modificationInfos.setNominalV(new AttributeModification<>(null, OperationType.UNSET));

        ValidationException exception = assertThrows(ValidationException.class,
            () -> modificationInfos.toModification().apply(getNetwork()));
        assertEquals("Hvdc Lcc Line 'idLcc': Nominal Voltage is not set",
            exception.getMessage());


    }
}
