package org.gridsuite.modification.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@Getter
@Setter
@NoArgsConstructor

public class LccShuntCompensatorModificationInfos extends AbstractLccShuntCompensatorInfos {
    private boolean deletionMark = false;

    public LccShuntCompensatorModificationInfos(String id, String name, Double maxQAtNominalV, Boolean connectedToHvdc, Boolean deletionMark) {
        super(id, name, maxQAtNominalV, connectedToHvdc);
        this.deletionMark = deletionMark;
    }
}
