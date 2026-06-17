package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;

import java.util.List;

public interface InjectionModification {

    String getEquipmentId();

    List<FreePropertyInfos> getProperties();

    AttributeModification<String> getEquipmentName();

    AttributeModification<String> getVoltageLevelId();

    AttributeModification<String> getBusOrBusbarSectionId();

    AttributeModification<String> getConnectionName();

    AttributeModification<ConnectablePosition.Direction> getConnectionDirection();

    AttributeModification<Integer> getConnectionPosition();

    AttributeModification<Boolean> getTerminalConnected();

    AttributeModification<Double> getPMeasurementValue();

    AttributeModification<Boolean> getPMeasurementValidity();

    AttributeModification<Double> getQMeasurementValue();

    AttributeModification<Boolean> getQMeasurementValidity();
}
