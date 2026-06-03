/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability.SIDE1;
import static org.gridsuite.modification.model.OperationalLimitsGroupModel.Applicability.SIDE2;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class TwoWindingsTransformerCreationNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationModel buildModification() {
        return buildModification(null, null);
    }

    private ModificationModel buildModificationWithInvalidSelectedLimitGroups() {
        return buildModification("invalid1", "invalid2");
    }

    protected ModificationModel buildModification(String selectedLimitGroups1, String selectedLimitGroups2) {
        return TwoWindingsTransformerCreationModel.builder()
                .equipmentId("new2wt")
                .equipmentName("new2wt")
                .r(1.)
                .x(2.)
                .g(3.)
                .b(4.)
                .ratedU1(5.)
                .ratedU2(6.)
                .ratedS(1.)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .connected2(true)
                .operationalLimitsGroups(
                    List.of(
                        OperationalLimitsGroupModel.builder()
                            .currentLimits(
                                    CurrentLimitsModel.builder().permanentLimit(3.).temporaryLimits(List.of(CurrentTemporaryLimitCreationModel.builder().name("IT5").acceptableDuration(2147483647).value(671.).build())).build()
                            ).applicability(SIDE1).build(),
                        OperationalLimitsGroupModel.builder()
                            .currentLimits(
                                CurrentLimitsModel.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationModel.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build()
                            ).applicability(SIDE2).build()
                    )
                )
                .connectionName1("cn201")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connected1(true)
                .connectionName2("cn202")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connected2(true)
                .phaseTapChanger(PhaseTapChangerCreationModel.builder()
                        .lowTapPosition(1)
                        .tapPosition(2)
                        .terminalRefConnectableId("v1load")
                        .terminalRefConnectableVlId("v1")
                        .regulating(false)
                        .terminalRefConnectableType("LOAD")
                        .regulationMode(PhaseTapChanger.RegulationMode.CURRENT_LIMITER)
                        .steps(List.of(TapChangerStepCreationModel.builder()
                                        .index(1)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .alpha(0)
                                        .build(),
                                TapChangerStepCreationModel.builder()
                                        .index(2)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .alpha(0.)
                                        .build(),
                                TapChangerStepCreationModel.builder()
                                        .index(3)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .alpha(0.)
                                        .build()
                        )).build())
                .ratioTapChanger(RatioTapChangerCreationModel.builder()
                        .lowTapPosition(5)
                        .tapPosition(6)
                        .regulating(true)
                        .targetDeadband(1.)
                        .terminalRefConnectableId("v2load")
                        .terminalRefConnectableVlId("v2")
                        .terminalRefConnectableType("LOAD")
                        .loadTapChangingCapabilities(true)
                        .targetV(5.)
                        .steps(List.of(TapChangerStepCreationModel.builder()
                                        .index(5)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build(),
                                TapChangerStepCreationModel.builder()
                                        .index(6)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build(),
                                TapChangerStepCreationModel.builder()
                                        .index(7)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build(),
                                TapChangerStepCreationModel.builder()
                                        .index(8)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build()
                        ))
                        .build())
            .selectedOperationalLimitsGroupId1(selectedLimitGroups1)
            .selectedOperationalLimitsGroupId1(selectedLimitGroups2)
                .properties(List.of(FreePropertyModel.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertNotNull(getNetwork().getTwoWindingsTransformer("new2wt"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals(1, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals("v1", getNetwork().getTwoWindingsTransformer("new2wt").getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", getNetwork().getTwoWindingsTransformer("new2wt").getTerminal2().getVoltageLevel().getId());
        assertEquals(2., getNetwork().getTwoWindingsTransformer("new2wt").getX(), 0.1);
        assertEquals(5., getNetwork().getTwoWindingsTransformer("new2wt").getRatedU1(), 0.1);
        assertEquals(1, getNetwork().getTwoWindingsTransformer("new2wt").getRatedS(), 0.1);
        assertEquals(4, getNetwork().getTwoWindingsTransformer("new2wt").getRatioTapChanger().getStepCount());
        assertEquals(3, getNetwork().getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getStepCount());
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, getNetwork().getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getRegulationMode());
        assertEquals(PROPERTY_VALUE, getNetwork().getTwoWindingsTransformer("new2wt").getProperty(PROPERTY_NAME));
    }

    @Test
    void testCreateTwoWindingsTransformerWithRatioTapChangerInNodeBreaker() {
        // create new 2wt in voltage level with Node/breaker topology, having a RatioTapChanger
        RatioTapChangerCreationModel ratioTapChangerCreationModel = RatioTapChangerCreationModel.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .terminalRefConnectableVlId("v1")
                .terminalRefConnectableId("v1load")
                .terminalRefConnectableType("LOAD")
                .loadTapChangingCapabilities(true)
                .targetV(220.)
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel = TwoWindingsTransformerCreationModel.builder()
                .equipmentId("id2wt1WithRatioTapChanger")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connected1(true)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connected2(true)
                .ratioTapChanger(ratioTapChangerCreationModel)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationModel);
        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel2 = TwoWindingsTransformerCreationModel.builder()
                .equipmentId("id2wt1WithRatioTapChanger2")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
                .connected2(true)
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .ratioTapChanger(ratioTapChangerCreationModel)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationModel2);
        assertEquals(
            "TwoWindingsTransformerCreationModel(super=BranchCreationModel(super=EquipmentCreationModel(super=EquipmentModificationModel(equipmentId=id2wt1WithRatioTapChanger2, properties=null), equipmentName=2wtName), r=400.0, x=300.0, voltageLevelId1=v1, voltageLevelId2=v4, busOrBusbarSectionId1=1.1, busOrBusbarSectionId2=1.A, operationalLimitsGroups=null, selectedOperationalLimitsGroupId1=null, selectedOperationalLimitsGroupId2=null, connectionName1=null, connectionDirection1=TOP, connectionName2=null, connectionDirection2=TOP, connectionPosition1=null, connectionPosition2=null, connected1=true, connected2=true), g=100.0, b=200.0, ratedU1=1000.0, ratedU2=1010.0, ratedS=null, ratioTapChanger=RatioTapChangerCreationModel(super=TapChangerCreationModel(lowTapPosition=0, tapPosition=1, regulating=true, targetDeadband=null, terminalRefConnectableId=v1load, terminalRefConnectableType=LOAD, terminalRefConnectableVlId=v1, steps=[TapChangerStepCreationModel(index=0, rho=1.0, r=39.78473, x=39.784725, g=0.0, b=0.0, alpha=0.0), TapChangerStepCreationModel(index=0, rho=1.0, r=39.78474, x=39.784726, g=0.0, b=0.0, alpha=0.0), TapChangerStepCreationModel(index=0, rho=1.0, r=39.78475, x=39.784727, g=0.0, b=0.0, alpha=0.0)], loadTapChangingCapabilities=true), targetV=220.0), phaseTapChanger=null)",
            twoWindingsTransformerCreationModel2.toString()
        );

        // create twt with ratioTapChanger having a null targetV
        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel3 = TwoWindingsTransformerCreationModel.builder()
                .equipmentId("id2wt1WithRatioTapChanger3")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
                .connected2(true)
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .ratioTapChanger(RatioTapChangerCreationModel.builder()
                        .lowTapPosition(0)
                        .tapPosition(1)
                        .regulating(false)
                        .targetDeadband(null)
                        .terminalRefConnectableVlId("v1")
                        .terminalRefConnectableId("v1load")
                        .terminalRefConnectableType("LOAD")
                        .loadTapChangingCapabilities(true)
                        .targetV(null)
                        .steps(getTapChangerSteps())
                        .build())
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationModel3);

    }

    @Test
    void testApplySelectedLimitsGroupsNotExist() {
        ModificationModel modification = buildModificationWithInvalidSelectedLimitGroups();
        modification.toModification().apply(getNetwork());
        assertEquals("", getNetwork().getTwoWindingsTransformer("new2wt").getSelectedOperationalLimitsGroupId1().orElse(""));
        assertEquals("", getNetwork().getTwoWindingsTransformer("new2wt").getSelectedOperationalLimitsGroupId2().orElse(""));
    }

    @Test
    void testCreateTwoWindingsTransformerWithPhaseTapChangerInNodeBreaker() {
        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger with Load regulating
        PhaseTapChangerCreationModel phaseTapChangerLoadRegulatingCreationModel = PhaseTapChangerCreationModel.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .regulationValue(10.0)
                .terminalRefConnectableVlId("v1")
                .terminalRefConnectableId("v1load")
                .terminalRefConnectableType("LOAD")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel = TwoWindingsTransformerCreationModel.builder()
                .equipmentId("id2wt1WithPhaseTapChanger")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connected1(true)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connected2(true)
                .phaseTapChanger(phaseTapChangerLoadRegulatingCreationModel)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationModel);

        // create new 2wt in voltage level with Node/breaker topology, PhaseTapChanger with Battery regulating
        PhaseTapChangerCreationModel phaseTapChangerBatteryRegulatingCreationModel = PhaseTapChangerCreationModel.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .regulationValue(10.0)
                .terminalRefConnectableVlId("v3")
                .terminalRefConnectableId("v3Battery")
                .terminalRefConnectableType("BATTERY")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel2 = TwoWindingsTransformerCreationModel.builder()
                .equipmentId("id2wt1WithPhaseTapChanger2")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
                .connected2(true)
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(phaseTapChangerBatteryRegulatingCreationModel)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationModel2);

        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger with Shunt compensator regulating
        PhaseTapChangerCreationModel phaseTapChangerShuntCompensatorRegulatingCreationModel = PhaseTapChangerCreationModel.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .regulationValue(10.0)
                .terminalRefConnectableVlId("v3")
                .terminalRefConnectableId("v2shunt")
                .terminalRefConnectableType("SHUNT_COMPENSATOR")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel3 = TwoWindingsTransformerCreationModel.builder()
                .equipmentId("id2wt1WithPhaseTapChanger3")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
                .connected2(true)
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(phaseTapChangerShuntCompensatorRegulatingCreationModel)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationModel3);
    }

    @Override
    protected void checkModification() {
        // try to create an existing equipment
        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel = (TwoWindingsTransformerCreationModel) buildModification();
        twoWindingsTransformerCreationModel.setEquipmentId("trf1");
        NetworkModificationException exception = assertThrows(NetworkModificationException.class, () -> twoWindingsTransformerCreationModel.toModification().check(getNetwork()));
        assertEquals("TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS : trf1", exception.getMessage());
    }

    private void testCreateTwoWindingsTransformerInNodeBreaker(TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel) {
        final String transformerId = twoWindingsTransformerCreationModel.getEquipmentId();
        twoWindingsTransformerCreationModel.toModification().apply(getNetwork());
        TwoWindingsTransformer twoWindingsTransformer = getNetwork().getTwoWindingsTransformer(transformerId);
        assertNotNull(twoWindingsTransformer);  // transformer was created
    }

    private static List<TapChangerStepCreationModel> getTapChangerSteps() {
        return List.of(
                TapChangerStepCreationModel.builder()
                        .r(39.78473)
                        .x(39.784725)
                        .g(0.)
                        .b(0.)
                        .rho(1.)
                        .build(),
                TapChangerStepCreationModel.builder()
                        .r(39.78474)
                        .x(39.784726)
                        .g(0.)
                        .b(0.)
                        .rho(1.)
                        .build(),
                TapChangerStepCreationModel.builder()
                        .r(39.78475)
                        .x(39.784727)
                        .g(0.)
                        .b(0.)
                        .rho(1.)
                        .build()
        );
    }

    @Override
    protected void testCreationModificationMessage(ModificationModel modificationModel) throws Exception {
        assertEquals("TWO_WINDINGS_TRANSFORMER_CREATION", modificationModel.getType().toString());
        Map<String, String> createdValues = modificationModel.getMapMessageValues();
        assertEquals("new2wt", createdValues.get("equipmentId"));
    }

    @Test
    void testCreationInfoChecks() {
        Network network = getNetwork();
        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel = TwoWindingsTransformerCreationModel.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .r(-1d)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationModel.toModification();
        String message = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerCreation.check(network)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Two windings transformer 'twt3' : can not have a negative value for Resistance R", message);

        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel2 = TwoWindingsTransformerCreationModel.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .g(-2d)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation2 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationModel2.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerCreation2.check(network)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Two windings transformer 'twt3' : can not have a negative value for Conductance G", message);

        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel3 = TwoWindingsTransformerCreationModel.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .ratedU1(-1)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation3 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationModel3.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerCreation3.check(network)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Two windings transformer 'twt3' : can not have a negative value for Rated Voltage on side 1", message);

        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel4 = TwoWindingsTransformerCreationModel.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .ratedU2(-1)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation4 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationModel4.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerCreation4.check(network)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Two windings transformer 'twt3' : can not have a negative value for Rated Voltage on side 2", message);

        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel5 = TwoWindingsTransformerCreationModel.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .ratioTapChanger(RatioTapChangerCreationModel.builder()
                .targetV(-1d)
                .build())
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation5 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationModel5.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerCreation5.check(network)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Two windings transformer 'twt3' : can not have a negative value for Target voltage for ratio tap changer", message);

        TwoWindingsTransformerCreationModel twoWindingsTransformerCreationModel6 = TwoWindingsTransformerCreationModel.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .ratedS(-200d)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation6 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationModel6.toModification();
        message = assertThrows(NetworkModificationException.class,
            () -> twoWindingsTransformerCreation6.check(network)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Two windings transformer 'twt3' : can not have a negative value for Rated nominal power", message);
    }
}

