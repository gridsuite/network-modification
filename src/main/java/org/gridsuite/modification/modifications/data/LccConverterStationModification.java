package org.gridsuite.modification.modifications.data;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.LccShuntCompensatorModificationInfos;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Getter
@Setter
@SuperBuilder
public class LccConverterStationModification extends AbstractInjectionModification {

    private AttributeModification<Float> lossFactor;
    private AttributeModification<Float> powerFactor;
    private List<LccShuntCompensatorModificationInfos> shuntCompensatorsOnSide;

    public boolean hasModifications() {
        return getEquipmentName() != null || lossFactor != null || powerFactor != null || !CollectionUtils.isEmpty(shuntCompensatorsOnSide);
    }
}
