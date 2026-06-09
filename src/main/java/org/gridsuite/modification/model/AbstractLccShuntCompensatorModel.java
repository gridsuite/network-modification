package org.gridsuite.modification.model;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "type"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = LccShuntCompensatorModel.class, name = "LCC_SHUNT_CREATION"),
    @JsonSubTypes.Type(value = LccShuntCompensatorModificationModel.class, name = "LCC_SHUNT_MODIFICATION")
})

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter

public abstract class AbstractLccShuntCompensatorModel {
    private String id;
    private String name;
    private Double maxQAtNominalV;
    private Boolean connectedToHvdc;
}
