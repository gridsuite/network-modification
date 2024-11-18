/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.formula;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.byfilter.equipmentfield.TwoWindingsTransformerField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createTwoWindingsTransformer;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TwoWindingsTransformerByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String TWT_ID_1 = "twt1";
    private static final String TWT_ID_2 = "twt2";
    private static final String TWT_ID_3 = "twt3";
    private static final String TWT_ID_4 = "twt4";
    private static final String TWT_ID_5 = "twt5";
    private static final String TWT_ID_6 = "twt6";

    @Test
    void testModifyTwtWithError() throws Exception {
        // Test modifying ratio tab changer field when ratio tab changer is null
        IdentifiableAttributes identifiableAttributes1 = new IdentifiableAttributes(TWT_ID_4, getIdentifiableType(), 1.);
        IdentifiableAttributes identifiableAttributes2 = new IdentifiableAttributes(TWT_ID_6, getIdentifiableType(), 1.);
        FilterEquipments filter = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(identifiableAttributes1, identifiableAttributes2)).build();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(Map.of(FILTER_ID_4, filter));
        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filter4))
                .fieldOrValue2(ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.RATIO_TAP_POSITION.name()).build())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(1.).build())
                .editedField(TwoWindingsTransformerField.RATIO_TAP_POSITION.name())
                .operator(Operator.ADDITION)
                .build();

        ByFormulaModificationInfos modificationInfos = ByFormulaModificationInfos.builder()
                .identifiableType(getIdentifiableType())
                .formulaInfosList(List.of(formulaInfos))
                .stashed(false)
                .build();
        apply(modificationInfos);

        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_4).getRatioTapChanger());
        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_6).getRatioTapChanger());

        // Test modifying phase tab changer field when phase tab changer is null
        IdentifiableAttributes identifiableAttributes3 = new IdentifiableAttributes(TWT_ID_1, getIdentifiableType(), 1.);
        IdentifiableAttributes identifiableAttributes4 = new IdentifiableAttributes(TWT_ID_2, getIdentifiableType(), 1.);
        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(identifiableAttributes3, identifiableAttributes4)).build();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(Map.of(FILTER_ID_1, filter2));
        FormulaInfos formulaInfos2 = FormulaInfos.builder()
                .filters(List.of(filter1))
                .fieldOrValue2(ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.PHASE_TAP_POSITION.name()).build())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(1.).build())
                .editedField(TwoWindingsTransformerField.PHASE_TAP_POSITION.name())
                .operator(Operator.ADDITION)
                .build();

        ByFormulaModificationInfos modificationInfos2 = ByFormulaModificationInfos.builder()
                .identifiableType(getIdentifiableType())
                .formulaInfosList(List.of(formulaInfos2))
                .stashed(false)
                .build();
        apply(modificationInfos2);

        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_1).getPhaseTapChanger());
        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_2).getPhaseTapChanger());
    }

    @Test
    void testModifyTwtWithWarning() throws Exception {
        IdentifiableAttributes identifiableAttributes1 = new IdentifiableAttributes(TWT_ID_1, getIdentifiableType(), 1.);
        IdentifiableAttributes identifiableAttributes2 = new IdentifiableAttributes(TWT_ID_2, getIdentifiableType(), 1.);
        IdentifiableAttributes identifiableAttributes3 = new IdentifiableAttributes(TWT_ID_4, getIdentifiableType(), 1.);
        IdentifiableAttributes identifiableAttributes4 = new IdentifiableAttributes(TWT_ID_6, getIdentifiableType(), 1.);
        FilterEquipments filterTwt1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(identifiableAttributes1, identifiableAttributes2)).build();
        FilterEquipments filterTwt2 = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(identifiableAttributes3, identifiableAttributes4)).build();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(Map.of(FILTER_ID_1, filterTwt1, FILTER_ID_4, filterTwt2));

        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filter1, filter4))
                .fieldOrValue2(ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.RATIO_TAP_POSITION.name()).build())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(1.).build())
                .editedField(TwoWindingsTransformerField.RATIO_TAP_POSITION.name())
                .operator(Operator.ADDITION)
                .build();

        ByFormulaModificationInfos modificationInfos = ByFormulaModificationInfos.builder()
                .identifiableType(getIdentifiableType())
                .formulaInfosList(List.of(formulaInfos))
                .stashed(false)
                .build();
        apply(modificationInfos);

        assertNotNull(getNetwork().getTwoWindingsTransformer(TWT_ID_1).getRatioTapChanger());
        assertNotNull(getNetwork().getTwoWindingsTransformer(TWT_ID_2).getRatioTapChanger());
        assertEquals(2, getNetwork().getTwoWindingsTransformer(TWT_ID_1).getRatioTapChanger().getTapPosition());
        assertEquals(5, getNetwork().getTwoWindingsTransformer(TWT_ID_2).getRatioTapChanger().getTapPosition());
        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_4).getRatioTapChanger());
        assertNull(getNetwork().getTwoWindingsTransformer(TWT_ID_6).getRatioTapChanger());

    }

    @Override
    protected void createEquipments() {
        Substation s1 = getNetwork().getSubstation("s1");
        Substation s3 = getNetwork().getSubstation("s3");
        TwoWindingsTransformer twt1 = createTwoWindingsTransformer(s1, TWT_ID_1, TWT_ID_1, 30, 40, 50,
            60, 10, 20, 100, 100,
                "v1", "v2",
                "trf1", 11, ConnectablePosition.Direction.TOP,
                "trf1", 22, ConnectablePosition.Direction.BOTTOM);
        twt1.setRatedS(11);
        addRatioTapChangerSteps(twt1.newRatioTapChanger().setTargetV(50).setLowTapPosition(0).setTapPosition(1).setTargetDeadband(55));

        TwoWindingsTransformer twt2 = createTwoWindingsTransformer(s1, TWT_ID_2, TWT_ID_2, 35, 45, 55,
            65, 15, 25, 101, 100,
                "v1", "v4",
                "trf1", 33, ConnectablePosition.Direction.TOP,
                "trf1", 44, ConnectablePosition.Direction.BOTTOM);
        twt2.setRatedS(10);
        addRatioTapChangerSteps(twt2.newRatioTapChanger().setTargetV(53).setLowTapPosition(3).setTapPosition(4).setTargetDeadband(58));

        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(s1, TWT_ID_3, TWT_ID_3, 40, 50, 60,
            70, 20, 30, 101, 101,
                "v2", "v4",
                "trf1", 10, ConnectablePosition.Direction.TOP,
                "trf1", 20, ConnectablePosition.Direction.BOTTOM);
        twt3.setRatedS(25);
        addRatioTapChangerSteps(twt3.newRatioTapChanger().setTargetV(56).setLowTapPosition(0).setTapPosition(1).setTargetDeadband(61));

        TwoWindingsTransformer twt4 = createTwoWindingsTransformer(s3, TWT_ID_4, TWT_ID_4, 45, 55, 65,
            75, 25, 35, 100, 100,
                "v5", "v6",
                "trf1", 30, ConnectablePosition.Direction.TOP,
                "trf1", 40, ConnectablePosition.Direction.BOTTOM);
        twt4.setRatedS(15);
        addPhaseTapChangerSteps(twt4.newPhaseTapChanger().setRegulationValue(45).setLowTapPosition(1).setTapPosition(2).setTargetDeadband(34));

        TwoWindingsTransformer twt5 = createTwoWindingsTransformer(s3, TWT_ID_5, TWT_ID_5, 50, 60, 70,
            80, 30, 40, 101, 101,
                "v5", "v6",
                "trf1", 15, ConnectablePosition.Direction.TOP,
                "trf1", 26, ConnectablePosition.Direction.BOTTOM);
        twt5.setRatedS(30);
        addPhaseTapChangerSteps(twt5.newPhaseTapChanger().setRegulationValue(46).setLowTapPosition(2).setTapPosition(2).setTargetDeadband(35));

        TwoWindingsTransformer twt6 = createTwoWindingsTransformer(s3, TWT_ID_6, TWT_ID_6, 55, 65, 75, 85,
            35, 45, 102, 102,
                "v5", "v6",
                "trf1", 38, ConnectablePosition.Direction.TOP,
                "trf1", 49, ConnectablePosition.Direction.BOTTOM);
        twt6.setRatedS(20);
        addPhaseTapChangerSteps(twt6.newPhaseTapChanger().setRegulationValue(47).setLowTapPosition(1).setTapPosition(1).setTargetDeadband(36));
    }

    @Override
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
            new IdentifiableAttributes(TWT_ID_1, getIdentifiableType(), 1.0),
            new IdentifiableAttributes(TWT_ID_2, getIdentifiableType(), 2.0)
        )).build();

        FilterEquipments filter2 = FilterEquipments.builder().filterId(FILTER_ID_2).identifiableAttributes(List.of(
            new IdentifiableAttributes(TWT_ID_1, getIdentifiableType(), 1.0),
            new IdentifiableAttributes(TWT_ID_3, getIdentifiableType(), 2.0)
        )).build();

        FilterEquipments filter3 = FilterEquipments.builder().filterId(FILTER_ID_3).identifiableAttributes(List.of(
            new IdentifiableAttributes(TWT_ID_4, getIdentifiableType(), 5.0),
            new IdentifiableAttributes(TWT_ID_5, getIdentifiableType(), 6.0)
        )).build();

        FilterEquipments filter4 = FilterEquipments.builder().filterId(FILTER_ID_4).identifiableAttributes(List.of(
            new IdentifiableAttributes(TWT_ID_4, getIdentifiableType(), 5.0),
            new IdentifiableAttributes(TWT_ID_6, getIdentifiableType(), 7.0)
        )).build();

        return Map.of(FILTER_ID_1, filter1, FILTER_ID_2, filter2, FILTER_ID_3, filter3, FILTER_ID_4, filter4);
    }

    @Override
    protected List<FormulaInfos> getFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(TwoWindingsTransformerField.TARGET_V.name(),
                List.of(filter1),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(200.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.TARGET_V.name()).build());

        FormulaInfos formulaInfos2 = getFormulaInfo(TwoWindingsTransformerField.RATIO_TAP_POSITION.name(),
                List.of(filter2),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().value(4.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.RATIO_TAP_POSITION.name()).build());

        FormulaInfos formulaInfos3 = getFormulaInfo(TwoWindingsTransformerField.RATIO_LOW_TAP_POSITION.name(),
                List.of(filter2),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().value(1.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.RATIO_LOW_TAP_POSITION.name()).build());

        FormulaInfos formulaInfos4 = getFormulaInfo(TwoWindingsTransformerField.RATIO_TARGET_DEADBAND.name(),
                List.of(filter1),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.RATIO_TARGET_DEADBAND.name()).build(),
                ReferenceFieldOrValue.builder().value(5.).build());

        FormulaInfos formulaInfos5 = getFormulaInfo(TwoWindingsTransformerField.REGULATION_VALUE.name(),
                List.of(filter4),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(200.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.REGULATION_VALUE.name()).build());

        FormulaInfos formulaInfos6 = getFormulaInfo(TwoWindingsTransformerField.PHASE_TAP_POSITION.name(),
                List.of(filter3),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().value(2.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.PHASE_TAP_POSITION.name()).build());

        FormulaInfos formulaInfos7 = getFormulaInfo(TwoWindingsTransformerField.PHASE_LOW_TAP_POSITION.name(),
                List.of(filter3),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().value(2.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.PHASE_LOW_TAP_POSITION.name()).build());

        FormulaInfos formulaInfos8 = getFormulaInfo(TwoWindingsTransformerField.PHASE_TARGET_DEADBAND.name(),
                List.of(filter4),
                Operator.SUBTRACTION,
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.PHASE_TARGET_DEADBAND.name()).build(),
                ReferenceFieldOrValue.builder().value(10.).build());

        FormulaInfos formulaInfos9 = getFormulaInfo(TwoWindingsTransformerField.X.name(),
                List.of(filter1, filter4),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().value(20.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.X.name()).build());

        FormulaInfos formulaInfos10 = getFormulaInfo(TwoWindingsTransformerField.R.name(),
                List.of(filter2, filter3),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(200.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.R.name()).build());

        FormulaInfos formulaInfos11 = getFormulaInfo(TwoWindingsTransformerField.G.name(),
                List.of(filter4, filter2),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().value(25.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.G.name()).build());

        FormulaInfos formulaInfos12 = getFormulaInfo(TwoWindingsTransformerField.B.name(),
                List.of(filter1, filter3),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().value(2.5).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.B.name()).build());

        FormulaInfos formulaInfos13 = getFormulaInfo(TwoWindingsTransformerField.RATED_U1.name(),
                List.of(filter2),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().value(15.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.RATED_U1.name()).build());

        FormulaInfos formulaInfos14 = getFormulaInfo(TwoWindingsTransformerField.RATED_U2.name(),
                List.of(filter3, filter2),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(50.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.RATED_U2.name()).build());

        FormulaInfos formulaInfos15 = getFormulaInfo(TwoWindingsTransformerField.RATED_S.name(),
                List.of(filter1, filter2),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(200.).build(),
                ReferenceFieldOrValue.builder().equipmentField(TwoWindingsTransformerField.RATED_S.name()).build());

        return List.of(formulaInfos1,
                formulaInfos2,
                formulaInfos3,
                formulaInfos4,
                formulaInfos5,
                formulaInfos6,
                formulaInfos7,
                formulaInfos8,
                formulaInfos9,
                formulaInfos10,
                formulaInfos11,
                formulaInfos12,
                formulaInfos13,
                formulaInfos14,
                formulaInfos15);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.TWO_WINDINGS_TRANSFORMER;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.TWO_WINDINGS_TRANSFORMER;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        TwoWindingsTransformer twt1 = getNetwork().getTwoWindingsTransformer(TWT_ID_1);
        RatioTapChanger ratioTapChanger1 = twt1.getRatioTapChanger();
        assertNotNull(ratioTapChanger1);
        assertEquals(100, ratioTapChanger1.getTargetV(), 0);
        assertEquals(1, ratioTapChanger1.getLowTapPosition());
        assertEquals(4, ratioTapChanger1.getTapPosition());
        assertEquals(11, ratioTapChanger1.getTargetDeadband(), 0);
        assertEquals(60, twt1.getX(), 0);
        assertEquals(150, twt1.getB(), 0);
        assertEquals(60, twt1.getR(), 0);
        assertEquals(75, twt1.getG(), 0);
        assertEquals(25, twt1.getRatedU1(), 0);
        assertEquals(10, twt1.getRatedU2(), 0);
        assertEquals(44, twt1.getRatedS(), 0);

        TwoWindingsTransformer twt2 = getNetwork().getTwoWindingsTransformer(TWT_ID_2);
        RatioTapChanger ratioTapChanger2 = twt2.getRatioTapChanger();
        assertNotNull(ratioTapChanger2);
        assertEquals(106, ratioTapChanger2.getTargetV(), 0);
        assertEquals(3, ratioTapChanger2.getLowTapPosition());
        assertEquals(4, ratioTapChanger2.getTapPosition());
        assertEquals(11.6, ratioTapChanger2.getTargetDeadband(), 0);
        assertEquals(65, twt2.getX(), 0);
        assertEquals(162.5, twt2.getB(), 0);
        assertEquals(15, twt2.getRatedU1(), 0);
        assertEquals(20, twt2.getRatedS(), 0);

        TwoWindingsTransformer twt3 = getNetwork().getTwoWindingsTransformer(TWT_ID_3);
        RatioTapChanger ratioTapChanger3 = twt3.getRatioTapChanger();
        assertNotNull(ratioTapChanger3);
        assertEquals(1, ratioTapChanger3.getLowTapPosition());
        assertEquals(4, ratioTapChanger3.getTapPosition());
        assertEquals(80, twt3.getR(), 0);
        assertEquals(85, twt3.getG(), 0);
        assertEquals(35, twt3.getRatedU1(), 0);
        assertEquals(15, twt3.getRatedU2(), 0);
        assertEquals(50, twt3.getRatedS(), 0);

        TwoWindingsTransformer twt4 = getNetwork().getTwoWindingsTransformer(TWT_ID_4);
        PhaseTapChanger phaseTapChanger4 = twt4.getPhaseTapChanger();
        assertNotNull(phaseTapChanger4);
        assertEquals(90, phaseTapChanger4.getRegulationValue(), 0);
        assertEquals(2, phaseTapChanger4.getLowTapPosition());
        assertEquals(4, phaseTapChanger4.getTapPosition());
        assertEquals(24, phaseTapChanger4.getTargetDeadband(), 0);
        assertEquals(90, twt4.getR(), 0);
        assertEquals(75, twt4.getX(), 0);
        assertEquals(90, twt4.getG(), 0);
        assertEquals(187.5, twt4.getB(), 0);
        assertEquals(25, twt4.getRatedU1(), 0);
        assertEquals(17.5, twt4.getRatedU2(), 0);
        assertEquals(15, twt4.getRatedS(), 0);

        TwoWindingsTransformer twt5 = getNetwork().getTwoWindingsTransformer(TWT_ID_5);
        PhaseTapChanger phaseTapChanger5 = twt5.getPhaseTapChanger();
        assertNotNull(phaseTapChanger4);
        assertEquals(4, phaseTapChanger5.getLowTapPosition());
        assertEquals(4, phaseTapChanger5.getTapPosition());
        assertEquals(100, twt5.getR(), 0);
        assertEquals(200, twt5.getB(), 0);
        assertEquals(20, twt5.getRatedU2(), 0);

        TwoWindingsTransformer twt6 = getNetwork().getTwoWindingsTransformer(TWT_ID_6);
        PhaseTapChanger phaseTapChanger6 = twt6.getPhaseTapChanger();
        assertNotNull(phaseTapChanger4);
        assertEquals(94, phaseTapChanger6.getRegulationValue(), 0);
        assertEquals(26, phaseTapChanger6.getTargetDeadband(), 0);
        assertEquals(85, twt6.getX(), 0);
        assertEquals(100, twt6.getG(), 0);
    }

    private static void addRatioTapChangerSteps(RatioTapChangerAdder ratioTapChangerAdder) {
        ratioTapChangerAdder.beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .beginStep()
                .setR(39.78474)
                .setX(39.784726)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .beginStep()
                .setR(39.78474)
                .setX(39.784726)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(5.0)
                .endStep()
                .beginStep()
                .setR(39.78474)
                .setX(39.784726)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .endStep()
                .add();
    }

    private static void addPhaseTapChangerSteps(PhaseTapChangerAdder phaseTapChangerAdder) {
        phaseTapChangerAdder.beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.)
                .endStep()
                .beginStep()
                .setR(39.78475)
                .setX(39.784727)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.1)
                .endStep()
                .beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.)
                .endStep()
                .beginStep()
                .setR(39.78475)
                .setX(39.784727)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.1)
                .endStep()
                .beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.)
                .endStep()
                .beginStep()
                .setR(39.78475)
                .setX(39.784727)
                .setG(0.0)
                .setB(0.0)
                .setRho(15.0)
                .setAlpha(1.1)
                .endStep()
                .add();
    }
}
