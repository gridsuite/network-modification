package org.gridsuite.modification.modifications.data;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.modifications.InjectionModification;

import java.util.List;

@Getter
@Setter
@SuperBuilder
public abstract class AbstractInjectionModification implements InjectionModification {

    private String equipmentId;
    private List<FreePropertyInfos> properties;
    private AttributeModification<String> equipmentName;
    private AttributeModification<String> voltageLevelId;
    private AttributeModification<String> busOrBusbarSectionId;
    private AttributeModification<String> connectionName;
    private AttributeModification<ConnectablePosition.Direction> connectionDirection;
    private AttributeModification<Integer> connectionPosition;
    private AttributeModification<Boolean> terminalConnected;
    private AttributeModification<Double> pMeasurementValue;
    private AttributeModification<Boolean> pMeasurementValidity;
    private AttributeModification<Double> qMeasurementValue;
    private AttributeModification<Boolean> qMeasurementValidity;
}
