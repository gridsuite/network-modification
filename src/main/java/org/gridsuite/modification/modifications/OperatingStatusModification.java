/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.tripping.BranchTripping;
import com.powsybl.iidm.modification.tripping.BusbarSectionTripping;
import com.powsybl.iidm.modification.tripping.HvdcLineTripping;
import com.powsybl.iidm.modification.tripping.ThreeWindingsTransformerTripping;
import com.powsybl.iidm.modification.tripping.Tripping;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import com.powsybl.iidm.network.extensions.OperatingStatusAdder;
import com.powsybl.iidm.network.util.SwitchPredicates;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Objects;
import java.util.function.Predicate;

import static org.gridsuite.modification.utils.ModificationUtils.distinctByKey;

/**
 * @author Ghazwa REHILI <ghazwa.rehili at rte-france.com>
 */
public class OperatingStatusModification extends AbstractModification {

    private final OperatingStatusModificationInfos modificationInfos;
    private static final Logger LOGGER = LoggerFactory.getLogger(OperatingStatusModification.class);

    private static final String EQUIPMENT_TYPE = "equipmentType";

    public OperatingStatusModification(OperatingStatusModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) {
        String equipmentId = modificationInfos.getEquipmentId();
        Identifiable<?> equipment = network.getIdentifiable(equipmentId);
        if (equipment == null) {
            throw new NetworkModificationException("Equipment not found: " + equipmentId);
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        String equipmentId = modificationInfos.getEquipmentId();
        Identifiable<?> equipment = network.getIdentifiable(equipmentId);
        if (equipment == null) {
            throw new NetworkModificationException("Equipment not found: " + equipmentId);
        }

        String equipmentType = String.valueOf(equipment.getType());
        switch (modificationInfos.getAction()) {
            case LOCKOUT -> applyLockoutEquipment(subReportNode, equipment, equipmentType);
            case TRIP -> applyTripEquipment(subReportNode, equipment, equipmentType, network);
            case SWITCH_ON -> applySwitchOnEquipment(subReportNode, equipment, equipmentType);
            case ENERGISE_END_ONE ->
                    applyEnergiseEquipmentEnd(subReportNode, equipment, equipmentType, TwoSides.ONE);
            case ENERGISE_END_TWO ->
                    applyEnergiseEquipmentEnd(subReportNode, equipment, equipmentType, TwoSides.TWO);
            default ->
                    throw new NetworkModificationException("The operating action type : " + modificationInfos.getAction() + " is unsupported");
        }
    }

    @Override
    public String getName() {
        return "OperatingStatusModification";
    }

    private void applyLockoutEquipment(ReportNode subReportNode, Identifiable<?> equipment, String equipmentType) {
        if (disconnectAllTerminals(equipment)) {
            equipment.newExtension(OperatingStatusAdder.class).withStatus(OperatingStatus.Status.PLANNED_OUTAGE).add();
        } else {
            throw new NetworkModificationException("Unable to disconnect all equipment ends");
        }
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.lockout.equipment.Applied")
                .withUntypedValue(EQUIPMENT_TYPE, equipmentType)
                .withUntypedValue("id", equipment.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void applyTripEquipment(ReportNode subReportNode, Identifiable<?> equipment, String equipmentType, Network network) {
        var switchesToDisconnect = new HashSet<Switch>();
        var terminalsToDisconnect = new HashSet<Terminal>();
        var traversedTerminals = new HashSet<Terminal>();
        getTrippingFromIdentifiable(equipment).traverse(network, switchesToDisconnect, terminalsToDisconnect, traversedTerminals);

        LOGGER.info("Apply Trip on {} {}, switchesToDisconnect: {} terminalsToDisconnect: {} traversedTerminals: {}",
                equipmentType, equipment.getId(),
                switchesToDisconnect.stream().map(Identifiable::getId).toList(),
                terminalsToDisconnect.stream().map(Terminal::getConnectable).map(Identifiable::getId).toList(),
                traversedTerminals.stream().map(Terminal::getConnectable).map(Identifiable::getId).toList());

        switchesToDisconnect.forEach(sw -> sw.setOpen(true));
        terminalsToDisconnect.forEach(Terminal::disconnect);

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.trip.equipment.Applied")
                .withUntypedValue(EQUIPMENT_TYPE, equipmentType)
                .withUntypedValue("id", equipment.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        traversedTerminals.stream()
                .map(t -> network.getIdentifiable(t.getConnectable().getId()))
                .filter(Objects::nonNull)
                .filter(distinctByKey(Identifiable::getId))
                .forEach(b -> equipment.newExtension(OperatingStatusAdder.class)
                        .withStatus(OperatingStatus.Status.FORCED_OUTAGE)
                        .add());
    }

    private void applySwitchOnEquipment(ReportNode subReportNode, Identifiable<?> equipment, String equipmentType) {
        if (!(equipment instanceof Branch<?>)) {
            throw new NetworkModificationException("The equipment type : " + equipment.getClass().getSimpleName() + " is not supported");
        }

        if (connectAllTerminals(equipment)) {
            equipment.newExtension(OperatingStatusAdder.class).withStatus(OperatingStatus.Status.IN_OPERATION).add();
        } else {
            throw new NetworkModificationException("Unable to connect all equipment ends");
        }

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.switchOn.equipment.Applied")
                .withUntypedValue(EQUIPMENT_TYPE, equipmentType)
                .withUntypedValue("id", equipment.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void applyEnergiseEquipmentEnd(ReportNode subReportNode, Identifiable<?> equipment, String equipmentType, TwoSides side) {
        if (!(equipment instanceof Branch<?> branch)) {
            throw new NetworkModificationException("The equipment type : " + equipment.getClass().getSimpleName() + " is not supported");
        }

        TwoSides oppositeSide = side == TwoSides.ONE ? TwoSides.TWO : TwoSides.ONE;
        if (connectOneTerminal(branch.getTerminal(side)) && disconnectOneTerminal(branch.getTerminal(oppositeSide), SwitchPredicates.IS_CLOSED_BREAKER)) {
            branch.newExtension(OperatingStatusAdder.class).withStatus(OperatingStatus.Status.IN_OPERATION).add();
        } else {
            throw new NetworkModificationException("Unable to energise equipment end");
        }

        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.energise.equipment.EndApplied")
                .withUntypedValue(EQUIPMENT_TYPE, equipmentType)
                .withUntypedValue("id", equipment.getId())
                .withUntypedValue("side", side.name())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private boolean disconnectAllTerminals(Identifiable<?> equipment) {
        return ModificationUtils.getInstance().getTerminalsFromIdentifiable(equipment).stream().allMatch(terminal -> disconnectOneTerminal(terminal, SwitchPredicates.IS_NON_NULL));
    }

    private boolean disconnectOneTerminal(Terminal terminal, Predicate<Switch> switchPredicates) {
        return !terminal.isConnected() || terminal.disconnect(switchPredicates);
    }

    private boolean connectAllTerminals(Identifiable<?> equipment) {
        return ModificationUtils.getInstance().getTerminalsFromIdentifiable(equipment).stream().allMatch(this::connectOneTerminal);
    }

    private boolean connectOneTerminal(Terminal terminal) {
        return terminal.isConnected() || terminal.connect();
    }

    public Tripping getTrippingFromIdentifiable(Identifiable<?> identifiable) {
        if (identifiable instanceof Branch<?> branch) {
            return new BranchTripping(branch.getId());
        } else if (identifiable instanceof BusbarSection bbs) {
            return new BusbarSectionTripping(bbs.getId());
        } else if (identifiable instanceof ThreeWindingsTransformer w3t) {
            return new ThreeWindingsTransformerTripping(w3t.getId());
        } else if (identifiable instanceof HvdcLine hvdcLine) {
            return new HvdcLineTripping(hvdcLine.getId());
        }
        throw new NetworkModificationException("The equipment type : " + identifiable.getClass().getSimpleName() + " is not supported");
    }
}
