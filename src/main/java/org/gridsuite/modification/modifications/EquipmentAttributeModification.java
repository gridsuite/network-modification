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
import org.gridsuite.modification.error.NetworkModificationRunException;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class EquipmentAttributeModification extends AbstractModification {

    private final EquipmentAttributeModificationInfos modificationInfos;

    public EquipmentAttributeModification(EquipmentAttributeModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) {
        Identifiable<?> identifiable = network.getIdentifiable(modificationInfos.getEquipmentId());
        if (identifiable == null) {
            throw new NetworkModificationRunException("Equipment not found: " + modificationInfos.getEquipmentId());
        }
        if (identifiable.getType() != modificationInfos.getEquipmentType()) {
            throw new NetworkModificationRunException("Wrong equipment type: " + String.format("Type of '%s' is not %s but %s", modificationInfos.getEquipmentId(), modificationInfos.getEquipmentType(), identifiable.getType()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Identifiable<?> identifiable = network.getIdentifiable(modificationInfos.getEquipmentId());
        if (identifiable instanceof Switch) {
            changeSwitchAttribute((Switch) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReportNode);
        } else if (identifiable instanceof Injection) {
            if (identifiable instanceof Generator) {
                changeGeneratorAttribute((Generator) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReportNode);
            }
        } else if (identifiable instanceof Branch) {
            if (identifiable instanceof Line) {
                changeLineAttribute((Line) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReportNode);
            } else if (identifiable instanceof TwoWindingsTransformer) {
                changeTwoWindingsTransformerAttribute((TwoWindingsTransformer) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReportNode);
            }
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            changeThreeWindingsTransformerAttribute((ThreeWindingsTransformer) identifiable, modificationInfos.getEquipmentAttributeName(), modificationInfos.getEquipmentAttributeValue(), subReportNode);
        } else if (identifiable instanceof HvdcLine) {
            // no hvdc line modifications yet
        }
    }

    @Override
    public String getName() {
        return "EquipmentAttributeModification";
    }

    private void changeSwitchAttribute(Switch aSwitch, String attributeName, Object attributeValue, ReportNode reportNode) {
        if (attributeName.equals("open")) {
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
            throw new NetworkModificationRunException(aSwitch.getType().name() + " attribute '" + attributeName + "' not editable");
        }
    }

    // TODO remove only for switch
    private void changeGeneratorAttribute(Generator generator, String attributeName, Object attributeValue, ReportNode reportNode) {
        if (attributeName.equals("targetP")) {
            generator.setTargetP((Double) attributeValue);
            reportNode.newReportNode()
                .withMessageTemplate("network.modification.generatorChanged")
                .withUntypedValue("id", generator.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        } else {
            throw new NetworkModificationRunException(generator.getType().name() + " attribute '" + attributeName + "' not editable");
        }
    }

    // TODO remove only for switch
    private void changeLineAttribute(Line line, String attributeName, Object attributeValue, ReportNode reportNode) {
        if (attributeName.equals("operatingStatus")) {
            line.newExtension(OperatingStatusAdder.class).withStatus(OperatingStatus.Status.valueOf((String) attributeValue)).add();
            reportNode.newReportNode()
                .withMessageTemplate("network.modification.lineStatusChanged")
                .withUntypedValue("id", line.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        } else {
            throw new NetworkModificationRunException(line.getType().name() + " attribute '" + attributeName + "' not editable");
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
                throw new NetworkModificationRunException(transformer.getType().name() + " attribute '" + attributeName + "' not editable");
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
                throw new NetworkModificationRunException(transformer.getType().name() + " attribute '" + attributeName + "' not editable");
        }

        reportNode.newReportNode()
            .withMessageTemplate(reportKey)
            .withUntypedValue("id", transformer.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
    }
}
