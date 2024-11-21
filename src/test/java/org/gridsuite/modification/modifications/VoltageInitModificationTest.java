/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.utils.NetworkUtil.createGenerator;
import static org.gridsuite.modification.utils.NetworkUtil.createSwitch;
import static org.gridsuite.modification.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
class VoltageInitModificationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);

        VoltageLevel v2 = network.getVoltageLevel("v2");
        createGenerator(v2, "newGen", 18, 45., 1.0, "feederNewGen", 50, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "dNewGen", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 19);
        createSwitch(v2, "brNewGen", null, SwitchKind.BREAKER, true, false, false, 18, 19);

        Generator newGen = network.getGenerator("newGen");
        newGen.setTargetV(224.);
        newGen.setVoltageRegulatorOn(true);

        ThreeWindingsTransformer transformer = network.getThreeWindingsTransformer("trf6");
        transformer.getLeg2().newRatioTapChanger()
            .setLowTapPosition(0)
            .setTapPosition(1)
            .setLoadTapChangingCapabilities(false)
            .setRegulating(true)
            .setTargetDeadband(1.0)
            .setTargetV(220.0)
            .beginStep()
            .setR(39.78473)
            .setX(39.784725)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .endStep()
            .beginStep()
            .setR(39.78474)
            .setX(39.784726)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .endStep()
            .beginStep()
            .setR(39.78475)
            .setX(39.784727)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .endStep()
            .add();

        network.getShuntCompensator("v2shunt").setSectionCount(1);
        network.getShuntCompensator("v5shunt").setSectionCount(0);
        network.getShuntCompensator("v6shunt").setSectionCount(0);

        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return VoltageInitModificationInfos.builder()
            .stashed(false)
            .generators(List.of(
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("idGenerator")
                    .targetQ(10.)
                    .build(),
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("newGen")
                    .targetV(226.)
                    .build()))
            .transformers(List.of(
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf1")
                    .ratioTapChangerPosition(2)
                    .ratioTapChangerTargetV(223.)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf2")
                    .ratioTapChangerPosition(2)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("2wtNotFound")
                    .ratioTapChangerPosition(2)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf6")
                    .ratioTapChangerPosition(2)
                    .ratioTapChangerTargetV(220.)
                    .legSide(ThreeSides.TWO)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("3wtNotFound")
                    .legSide(ThreeSides.THREE)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("3wtNotFound")
                    .ratioTapChangerPosition(1)
                    .legSide(ThreeSides.ONE)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf6")
                    .ratioTapChangerPosition(1)
                    .ratioTapChangerTargetV(220.)
                    .legSide(ThreeSides.ONE)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf6")
                    .ratioTapChangerPosition(null)
                    .ratioTapChangerTargetV(220.)
                    .legSide(ThreeSides.TWO)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf6")
                    .ratioTapChangerPosition(2)
                    .ratioTapChangerTargetV(null)
                    .legSide(ThreeSides.TWO)
                    .build()))
            .staticVarCompensators(List.of(
                VoltageInitStaticVarCompensatorModificationInfos.builder()
                    .staticVarCompensatorId("v5Compensator")
                    .reactivePowerSetpoint(50.)
                    .build(),
                VoltageInitStaticVarCompensatorModificationInfos.builder()
                    .staticVarCompensatorId("v6Compensator")
                    .voltageSetpoint(372.)
                    .build(),
                VoltageInitStaticVarCompensatorModificationInfos.builder()
                    .staticVarCompensatorId("svcNotFound")
                    .voltageSetpoint(230.)
                    .build()))
            .vscConverterStations(List.of(
                VoltageInitVscConverterStationModificationInfos.builder()
                    .vscConverterStationId("v2vsc")
                    .reactivePowerSetpoint(23.)
                    .build(),
                VoltageInitVscConverterStationModificationInfos.builder()
                    .vscConverterStationId("v2vsc")
                    .voltageSetpoint(560.)
                    .build(),
                VoltageInitVscConverterStationModificationInfos.builder()
                    .vscConverterStationId("vscNotFound")
                    .voltageSetpoint(218.)
                    .build()))
            .shuntCompensators(List.of(
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("v2shunt")
                    .sectionCount(1)
                    .connect(true)
                    .targetV(230.)
                    .build(),
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("v5shunt")
                    .sectionCount(0)
                    .connect(true)
                    .targetV(221.)
                    .build(),
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("v6shunt")
                    .sectionCount(1)
                    .connect(false)
                    .build(),
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("shuntNotFound")
                    .sectionCount(1)
                    .connect(false)
                    .build()))
            .buses(List.of(
                VoltageInitBusModificationInfos.builder()
                    .voltageLevelId("v2")
                    .busId("busNotFound")
                    .v(400.)
                    .angle(0.)
                    .build(),
                VoltageInitBusModificationInfos.builder()
                    .voltageLevelId("v1")
                    .busId("v1_0")
                    .v(230.)
                    .angle(0.5)
                    .build()))
            .build();
    }

    private ReportNode testVoltageInitShunt(String shuntCompensatorId, int currentSectionCount, Integer sectionCount, Boolean connect) throws Exception {
        setNetwork(createNetwork(getNetworkId()));
        getNetwork().getShuntCompensator(shuntCompensatorId).setSectionCount(currentSectionCount);

        VoltageInitModificationInfos modification = VoltageInitModificationInfos.builder()
            .stashed(false)
            .generators(List.of())
            .transformers(List.of())
            .staticVarCompensators(List.of())
            .vscConverterStations(List.of())
            .buses(List.of())
            .shuntCompensators(List.of(
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId(shuntCompensatorId)
                    .sectionCount(sectionCount)
                    .connect(connect)
                    .build()))
            .build();
        ReportNode report = modification.createSubReportNode(ReportNode.newRootReportNode().withMessageTemplate("", "").build());
        modification.toModification().apply(getNetwork(), report);
        return report;
    }

    @Test
    void testVoltageInitConnectedSectionCountNull() throws Exception {
        ReportNode report = testVoltageInitShunt("v2shunt", 0, null, false);
        assertLogMessage("Section count value is undefined", "shuntCompensatorSectionCountUndefined", report);
    }

    @Test
    void testVoltageInitConnectedCurrentSection0Section0() throws Exception {
        ReportNode report = testVoltageInitShunt("v2shunt", 0, 0, false);
        assertLogMessage("Shunt compensator disconnected", "shuntCompensatorDisconnected", report);
    }

    @Test
    void testVoltageInitDisconnectedConnectNull() throws Exception {
        ReportNode report = testVoltageInitShunt("v5shunt", 0, 0, null);
        assertLogMessage("Connect value is undefined", "shuntCompensatorConnectUndefined", report);
    }

    @Test
    void testVoltageInitDisconnectedCurrentSection0Section2() throws Exception {
        ReportNode report = testVoltageInitShunt("v5shunt", 0, 2, true);
        assertLogMessage("Shunt compensator reconnected", "shuntCompensatorReconnected", report);
        assertLogMessage("Section count : 0 → 2", "modification-indent1", report);
        assertEquals(2, getNetwork().getShuntCompensator("v5shunt").getSectionCount());
    }

    @Test
    void testVoltageInitDisconnectedCurrentSection1Section1() throws Exception {
        ReportNode report = testVoltageInitShunt("v5shunt", 1, 1, true);
        assertLogMessage("Shunt compensator reconnected", "shuntCompensatorReconnected", report);
        assertEquals(1, getNetwork().getShuntCompensator("v5shunt").getSectionCount());
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        assertEquals(10., getNetwork().getGenerator("idGenerator").getTargetQ(), 0.001);
        assertEquals(226., getNetwork().getGenerator("newGen").getTargetV(), 0.001);
        assertEquals(2, getNetwork().getTwoWindingsTransformer("trf1").getRatioTapChanger().getTapPosition());
        assertEquals(223, getNetwork().getTwoWindingsTransformer("trf1").getRatioTapChanger().getTargetV(), 0.001);
        assertEquals(2, getNetwork().getThreeWindingsTransformer("trf6").getLeg2().getRatioTapChanger().getTapPosition());
        assertEquals(220., getNetwork().getThreeWindingsTransformer("trf6").getLeg2().getRatioTapChanger().getTargetV(), 0.001);
        assertEquals(50., getNetwork().getStaticVarCompensator("v5Compensator").getReactivePowerSetpoint(), 0.001);
        assertEquals(372., getNetwork().getStaticVarCompensator("v6Compensator").getVoltageSetpoint(), 0.001);
        assertEquals(23., getNetwork().getVscConverterStation("v2vsc").getReactivePowerSetpoint(), 0.001);
        assertEquals(560., getNetwork().getVscConverterStation("v2vsc").getVoltageSetpoint(), 0.001);
        assertEquals(1, getNetwork().getShuntCompensator("v2shunt").getSectionCount());
        assertEquals(230., getNetwork().getShuntCompensator("v2shunt").getTargetV(), 0.001);
        assertEquals(0, getNetwork().getShuntCompensator("v5shunt").getSectionCount());
        assertEquals(221., getNetwork().getShuntCompensator("v5shunt").getTargetV(), 0.001);
        assertEquals(1, getNetwork().getShuntCompensator("v6shunt").getSectionCount());
        assertEquals(230., getNetwork().getBusView().getBus("v1_0").getV(), 0.001);
        assertEquals(0.5, getNetwork().getBusView().getBus("v1_0").getAngle(), 0.001);
    }

    @Override
    protected void checkModification() {
    }
}
