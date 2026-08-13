package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import lombok.*;
import lombok.experimental.SuperBuilder;

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "type"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = LccShuntCompensatorInfos.class, name = "LCC_SHUNT_CREATION"),
    @JsonSubTypes.Type(value = LccShuntCompensatorModificationInfos.class, name = "LCC_SHUNT_MODIFICATION")
})
@Getter
@Setter
@SuperBuilder
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
public abstract class AbstractLccShuntCompensatorInfos {
    private String id;
    private String name;
    private Double maxQAtNominalV;
    private Boolean connectedToHvdc;
}
