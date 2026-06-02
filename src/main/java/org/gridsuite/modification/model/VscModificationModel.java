package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.HvdcLine;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.VscModification;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonTypeName("VSC_MODIFICATION")
@ModificationErrorTypeName("MODIFY_VSC_ERROR")
public class VscModificationModel extends BasicEquipmentModificationModel {
    private AttributeModification<Double> nominalV;

    private AttributeModification<Double> r;

    private AttributeModification<Double> maxP;

    private AttributeModification<Float> operatorActivePowerLimitFromSide1ToSide2;

    private AttributeModification<Float> operatorActivePowerLimitFromSide2ToSide1;

    private AttributeModification<HvdcLine.ConvertersMode> convertersMode;

    private AttributeModification<Double> activePowerSetpoint;

    private AttributeModification<Boolean> angleDroopActivePowerControl;

    private AttributeModification<Float> p0;

    private AttributeModification<Float> droop;

    private ConverterStationModificationModel converterStation1;

    private ConverterStationModificationModel converterStation2;

    @Override
    public AbstractModification toModification() {
        return new VscModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate("network.modification.vsc.modification")
                .withUntypedValue("vscId", getEquipmentId())
                .add();
    }
}
