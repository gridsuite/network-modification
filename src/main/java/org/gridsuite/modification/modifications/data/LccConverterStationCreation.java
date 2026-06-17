package org.gridsuite.modification.modifications.data;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.LccShuntCompensatorInfos;

import java.util.List;

@Getter
@Setter
@SuperBuilder
public class LccConverterStationCreation extends AbstractInjectionCreation {

    private Float lossFactor;
    private Float powerFactor;
    private List<LccShuntCompensatorInfos> shuntCompensatorsOnSide;
}
