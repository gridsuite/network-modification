/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import com.powsybl.iidm.network.extensions.OperatingStatusAdder;
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;
import static org.gridsuite.modification.NetworkModificationException.Type.WRONG_EQUIPMENT_TYPE;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Getter
@Setter
public class EquipmentAttributeModification extends AbstractEquipmentBase {

    private String equipmentAttributeName;
    private Object equipmentAttributeValue;
    private IdentifiableType equipmentType;

    @Builder
    public EquipmentAttributeModification(String equipmentId, List<FreePropertyInfos> properties,
                                          String equipmentAttributeName, Object equipmentAttributeValue,
                                          IdentifiableType equipmentType) {
        super(equipmentId, properties);
        this.equipmentAttributeName = equipmentAttributeName;
        this.equipmentAttributeValue = equipmentAttributeValue;
        this.equipmentType = equipmentType;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Identifiable<?> identifiable = network.getIdentifiable(equipmentId);
        if (identifiable == null) {
            throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, equipmentId);
        }
        if (identifiable.getType() != equipmentType) {
            throw new NetworkModificationException(WRONG_EQUIPMENT_TYPE, String.format("Type of '%s' is not %s but %s", equipmentId, equipmentType,
                    identifiable.getType()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Identifiable<?> identifiable = network.getIdentifiable(equipmentId);
        if (identifiable instanceof Switch) {
            changeSwitchAttribute((Switch) identifiable, equipmentAttributeName, equipmentAttributeValue, subReportNode);
        } else if (identifiable instanceof Injection) {
            if (identifiable instanceof Generator) {
                changeGeneratorAttribute((Generator) identifiable, equipmentAttributeName, equipmentAttributeValue, subReportNode);
            }
        } else if (identifiable instanceof Branch) {
            if (identifiable instanceof Line) {
                changeLineAttribute((Line) identifiable, equipmentAttributeName, equipmentAttributeValue, subReportNode);
            } else if (identifiable instanceof TwoWindingsTransformer) {
                changeTwoWindingsTransformerAttribute((TwoWindingsTransformer) identifiable, equipmentAttributeName, equipmentAttributeValue,
                        subReportNode);
            }
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            changeThreeWindingsTransformerAttribute((ThreeWindingsTransformer) identifiable, equipmentAttributeName, equipmentAttributeValue,
                    subReportNode);
        } else if (identifiable instanceof HvdcLine) {
            // no hvdc line modifications yet
        }
    }

    @Override
    public String getName() {
        return "EquipmentAttributeModification";
    }

    private void changeSwitchAttribute(Switch aSwitch, String attributeName, Object attributeValue, ReportNode reportNode) {
        if ("open".equals(attributeName)) {
            if (Boolean.TRUE.equals(aSwitch.isOpen() != (Boolean) attributeValue)) {
                aSwitch.setOpen((Boolean) attributeValue);
                reportNode.newReportNode()
                    .withMessageTemplate("network.modification.switchChanged")
                    .withUntypedValue("id", aSwitch.getId())
                    .withUntypedValue("operation", Boolean.TRUE.equals(attributeValue) ? "Opening" : "Closing")
                    .withUntypedValue("voltageLevelId", aSwitch.getVoltageLevel().getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            }
        } else {
            throw NetworkModificationException.createEquipementAttributeNotEditable(aSwitch.getType(), attributeName);
        }
    }

    // TODO remove only for switch
    private void changeGeneratorAttribute(Generator generator, String attributeName, Object attributeValue, ReportNode reportNode) {
        if ("targetP".equals(attributeName)) {
            generator.setTargetP((Double) attributeValue);
            reportNode.newReportNode()
                .withMessageTemplate("network.modification.generatorChanged")
                .withUntypedValue("id", generator.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        } else {
            throw NetworkModificationException.createEquipementAttributeNotEditable(generator.getType(), attributeName);
        }
    }

    // TODO remove only for switch
    private void changeLineAttribute(Line line, String attributeName, Object attributeValue, ReportNode reportNode) {
        if ("operatingStatus".equals(attributeName)) {
            line.newExtension(OperatingStatusAdder.class).withStatus(OperatingStatus.Status.valueOf((String) attributeValue)).add();
            reportNode.newReportNode()
                .withMessageTemplate("network.modification.lineStatusChanged")
                .withUntypedValue("id", line.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        } else {
            throw NetworkModificationException.createEquipementAttributeNotEditable(line.getType(), attributeName);
        }
    }

    // TODO remove only for switch
    private void changeTwoWindingsTransformerAttribute(TwoWindingsTransformer transformer, String attributeName, Object attributeValue, ReportNode reportNode) {
        String reportKey;

        switch (attributeName) {
            case "ratioTapChanger.tapPosition":
                transformer.getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
                reportKey = "network.modification.ratioTapPositionChanged";
                break;
            case "phaseTapChanger.tapPosition":
                reportKey = "network.modification.phaseTapPositionChanged";
                break;
            default:
                throw NetworkModificationException.createEquipementAttributeNotEditable(transformer.getType(), attributeName);
        }

        reportNode.newReportNode()
            .withMessageTemplate(reportKey)
            .withUntypedValue("id", transformer.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
    }

    // TODO remove only for switch
    private void changeThreeWindingsTransformerAttribute(ThreeWindingsTransformer transformer, String attributeName, Object attributeValue, ReportNode reportNode) {
        String reportKey;

        switch (attributeName) {
            case "ratioTapChanger1.tapPosition":
                transformer.getLeg1().getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
                reportKey = "network.modification.ratioTapChanger1.tapPosition";
                break;
            case "ratioTapChanger2.tapPosition":
                transformer.getLeg2().getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
                reportKey = "network.modification.ratioTapChanger2.tapPosition";
                break;
            case "ratioTapChanger3.tapPosition":
                transformer.getLeg3().getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
                reportKey = "network.modification.ratioTapChanger3.tapPosition";
                break;
            case "phaseTapChanger1.tapPosition":
                transformer.getLeg1().getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
                reportKey = "network.modification.phaseTapChanger1.tapPosition";
                break;
            case "phaseTapChanger2.tapPosition":
                transformer.getLeg2().getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
                reportKey = "network.modification.phaseTapChanger2.tapPosition";
                break;
            case "phaseTapChanger3.tapPosition":
                transformer.getLeg3().getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
                reportKey = "network.modification.phaseTapChanger3.tapPosition";
                break;
            default:
                throw NetworkModificationException.createEquipementAttributeNotEditable(transformer.getType(), attributeName);
        }

        reportNode.newReportNode()
            .withMessageTemplate(reportKey)
            .withUntypedValue("id", transformer.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
    }
}
