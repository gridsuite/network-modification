package org.gridsuite.modification.modifications.data;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;

import java.util.List;

@Getter
@Setter
@SuperBuilder
public class VscConverterStationModification extends AbstractInjectionModification {

    private AttributeModification<Float> lossFactor;
    private AttributeModification<Double> reactivePowerSetpoint;
    private AttributeModification<Boolean> voltageRegulationOn;
    private AttributeModification<Double> voltageSetpoint;
    private AttributeModification<Boolean> reactiveCapabilityCurve;
    private AttributeModification<Double> minQ;
    private AttributeModification<Double> maxQ;
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;
}
