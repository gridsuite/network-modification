/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE1;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE2;
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
    protected ModificationInfos buildModification() {
        return buildModification(null, null);
    }

    private ModificationInfos buildModificationWithInvalidSelectedLimitGroups() {
        return buildModification("invalid1", "invalid2");
    }

    protected ModificationInfos buildModification(String selectedLimitGroups1, String selectedLimitGroups2) {
        return TwoWindingsTransformerCreationInfos.builder()
                .stashed(false)
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
                        OperationalLimitsGroupInfos.builder()
                            .currentLimits(
                                    CurrentLimitsInfos.builder().permanentLimit(3.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT5").acceptableDuration(2147483647).value(671.).build())).build()
                            ).applicability(SIDE1).build(),
                        OperationalLimitsGroupInfos.builder()
                            .currentLimits(
                                CurrentLimitsInfos.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build()
                            ).applicability(SIDE2).build()
                    )
                )
                .connectionName1("cn201")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connected1(true)
                .connectionName2("cn202")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connected2(true)
                .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                        .lowTapPosition(1)
                        .tapPosition(2)
                        .terminalRefConnectableId("v1load")
                        .terminalRefConnectableVlId("v1")
                        .regulating(false)
                        .terminalRefConnectableType("LOAD")
                        .regulationMode(PhaseTapChanger.RegulationMode.CURRENT_LIMITER)
                        .steps(List.of(TapChangerStepCreationInfos.builder()
                                        .index(1)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .alpha(0)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(2)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .alpha(0.)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(3)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .alpha(0.)
                                        .build()
                        )).build())
                .ratioTapChanger(RatioTapChangerCreationInfos.builder()
                        .lowTapPosition(5)
                        .tapPosition(6)
                        .regulating(true)
                        .targetDeadband(1.)
                        .terminalRefConnectableId("v2load")
                        .terminalRefConnectableVlId("v2")
                        .terminalRefConnectableType("LOAD")
                        .loadTapChangingCapabilities(true)
                        .targetV(5.)
                        .steps(List.of(TapChangerStepCreationInfos.builder()
                                        .index(5)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(6)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(7)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(8)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build()
                        ))
                        .build())
            .selectedOperationalLimitsGroup1(selectedLimitGroups1)
            .selectedOperationalLimitsGroup1(selectedLimitGroups2)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
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
    void testCreateTwoWindingsTransformerWithRatioTapChangerInNodeBreaker() throws Exception {
        // create new 2wt in voltage level with Node/breaker topology, having a RatioTapChanger
        RatioTapChangerCreationInfos ratioTapChangerCreationInfos = RatioTapChangerCreationInfos.builder()
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
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
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
                .ratioTapChanger(ratioTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos);
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
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
                .ratioTapChanger(ratioTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos2);
        assertEquals(
            "TwoWindingsTransformerCreationInfos(super=BranchCreationInfos(super=EquipmentCreationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=null, type=TWO_WINDINGS_TRANSFORMER_CREATION, date=null, stashed=false, messageType=null, messageValues=null, activated=true), equipmentId=id2wt1WithRatioTapChanger2, properties=null), equipmentName=2wtName), r=400.0, x=300.0, voltageLevelId1=v1, voltageLevelId2=v4, busOrBusbarSectionId1=1.1, busOrBusbarSectionId2=1.A, operationalLimitsGroups=null, selectedOperationalLimitsGroup1=null, selectedOperationalLimitsGroup2=null, connectionName1=null, connectionDirection1=TOP, connectionName2=null, connectionDirection2=TOP, connectionPosition1=null, connectionPosition2=null, connected1=true, connected2=true), g=100.0, b=200.0, ratedU1=1000.0, ratedU2=1010.0, ratedS=null, ratioTapChanger=RatioTapChangerCreationInfos(super=TapChangerCreationInfos(lowTapPosition=0, tapPosition=1, regulating=true, targetDeadband=null, terminalRefConnectableId=v1load, terminalRefConnectableType=LOAD, terminalRefConnectableVlId=v1, steps=[TapChangerStepCreationInfos(index=0, rho=1.0, r=39.78473, x=39.784725, g=0.0, b=0.0, alpha=0.0), TapChangerStepCreationInfos(index=0, rho=1.0, r=39.78474, x=39.784726, g=0.0, b=0.0, alpha=0.0), TapChangerStepCreationInfos(index=0, rho=1.0, r=39.78475, x=39.784727, g=0.0, b=0.0, alpha=0.0)], loadTapChangingCapabilities=true), targetV=220.0), phaseTapChanger=null)",
            twoWindingsTransformerCreationInfos2.toString()
        );

        // create twt with ratioTapChanger having a null targetV
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos3 = TwoWindingsTransformerCreationInfos.builder()
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
                .ratioTapChanger(RatioTapChangerCreationInfos.builder()
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
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos3);

    }

    @Test
    void testApplySelectedLimitsGroupsNotExist() {
        ModificationInfos modification = buildModificationWithInvalidSelectedLimitGroups();
        modification.toModification().apply(getNetwork());
        assertEquals("", getNetwork().getTwoWindingsTransformer("new2wt").getSelectedOperationalLimitsGroupId1().orElse(""));
        assertEquals("", getNetwork().getTwoWindingsTransformer("new2wt").getSelectedOperationalLimitsGroupId2().orElse(""));
    }

    @Test
    void testCreateTwoWindingsTransformerWithPhaseTapChangerInNodeBreaker() throws Exception {
        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger with Load regulating
        PhaseTapChangerCreationInfos phaseTapChangerLoadRegulatingCreationInfos = PhaseTapChangerCreationInfos.builder()
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
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
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
                .phaseTapChanger(phaseTapChangerLoadRegulatingCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos);

        // create new 2wt in voltage level with Node/breaker topology, PhaseTapChanger with Battery regulating
        PhaseTapChangerCreationInfos phaseTapChangerBatteryRegulatingCreationInfos = PhaseTapChangerCreationInfos.builder()
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
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
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
                .phaseTapChanger(phaseTapChangerBatteryRegulatingCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos2);

        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger with Shunt compensator regulating
        PhaseTapChangerCreationInfos phaseTapChangerShuntCompensatorRegulatingCreationInfos = PhaseTapChangerCreationInfos.builder()
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
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos3 = TwoWindingsTransformerCreationInfos.builder()
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
                .phaseTapChanger(phaseTapChangerShuntCompensatorRegulatingCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos3);
    }

    @Override
    protected void checkModification() {
        // try to create an existing equipment
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) buildModification();
        twoWindingsTransformerCreationInfos.setEquipmentId("trf1");
        NetworkModificationRunException exception = assertThrows(NetworkModificationRunException.class, () -> twoWindingsTransformerCreationInfos.toModification().check(getNetwork()));
        assertEquals("Two winding transformer already exists: trf1", exception.getMessage());
    }

    private void testCreateTwoWindingsTransformerInNodeBreaker(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) throws Exception {
        final String transformerId = twoWindingsTransformerCreationInfos.getEquipmentId();
        twoWindingsTransformerCreationInfos.toModification().apply(getNetwork());
        TwoWindingsTransformer twoWindingsTransformer = getNetwork().getTwoWindingsTransformer(transformerId);
        assertNotNull(twoWindingsTransformer);  // transformer was created
    }

    private static List<TapChangerStepCreationInfos> getTapChangerSteps() {
        return List.of(
                TapChangerStepCreationInfos.builder()
                        .r(39.78473)
                        .x(39.784725)
                        .g(0.)
                        .b(0.)
                        .rho(1.)
                        .build(),
                TapChangerStepCreationInfos.builder()
                        .r(39.78474)
                        .x(39.784726)
                        .g(0.)
                        .b(0.)
                        .rho(1.)
                        .build(),
                TapChangerStepCreationInfos.builder()
                        .r(39.78475)
                        .x(39.784727)
                        .g(0.)
                        .b(0.)
                        .rho(1.)
                        .build()
        );
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("TWO_WINDINGS_TRANSFORMER_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("new2wt", createdValues.get("equipmentId"));
    }

    @Test
    void testCreationInfoChecks() {
        Network network = getNetwork();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .r(-1d)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationInfos.toModification();
        String message = assertThrows(NetworkModificationRunException.class,
            () -> twoWindingsTransformerCreation.check(network)).getMessage();
        assertEquals("Two windings transformer 'twt3' : can not have a negative value for Resistance R", message);

        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .g(-2d)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation2 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationInfos2.toModification();
        message = assertThrows(NetworkModificationRunException.class,
            () -> twoWindingsTransformerCreation2.check(network)).getMessage();
        assertEquals("Two windings transformer 'twt3' : can not have a negative value for Conductance G", message);

        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos3 = TwoWindingsTransformerCreationInfos.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .ratedU1(-1)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation3 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationInfos3.toModification();
        message = assertThrows(NetworkModificationRunException.class,
            () -> twoWindingsTransformerCreation3.check(network)).getMessage();
        assertEquals("Two windings transformer 'twt3' : can not have a negative value for Rated Voltage on side 1", message);

        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos4 = TwoWindingsTransformerCreationInfos.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .ratedU2(-1)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation4 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationInfos4.toModification();
        message = assertThrows(NetworkModificationRunException.class,
            () -> twoWindingsTransformerCreation4.check(network)).getMessage();
        assertEquals("Two windings transformer 'twt3' : can not have a negative value for Rated Voltage on side 2", message);

        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos5 = TwoWindingsTransformerCreationInfos.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .ratioTapChanger(RatioTapChangerCreationInfos.builder()
                .targetV(-1d)
                .build())
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation5 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationInfos5.toModification();
        message = assertThrows(NetworkModificationRunException.class,
            () -> twoWindingsTransformerCreation5.check(network)).getMessage();
        assertEquals("Two windings transformer 'twt3' : can not have a negative value for Target voltage for ratio tap changer", message);

        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos6 = TwoWindingsTransformerCreationInfos.builder()
            .equipmentId("twt3")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .ratedS(-200d)
            .build();
        TwoWindingsTransformerCreation twoWindingsTransformerCreation6 = (TwoWindingsTransformerCreation) twoWindingsTransformerCreationInfos6.toModification();
        message = assertThrows(NetworkModificationRunException.class,
            () -> twoWindingsTransformerCreation6.check(network)).getMessage();
        assertEquals("Two windings transformer 'twt3' : can not have a negative value for Rated nominal power", message);
    }
}

