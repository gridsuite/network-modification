package org.gridsuite.modification.modifications.data;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.dto.ReactiveLimitsHolderInfos;

import java.util.List;

@Getter
@Setter
@SuperBuilder
public class VscConverterStationCreation extends AbstractInjectionCreation implements ReactiveLimitsHolderInfos {

    private Float lossFactor;
    private Double reactivePowerSetpoint;
    private Boolean voltageRegulationOn;
    private Double voltageSetpoint;
    private Boolean reactiveCapabilityCurve;
    private Double minQ;
    private Double maxQ;
    private List<ReactiveCapabilityCurvePointsInfos> reactiveCapabilityCurvePoints;
}
