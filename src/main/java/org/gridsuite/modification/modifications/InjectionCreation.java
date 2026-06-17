package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.dto.FreePropertyInfos;

import java.util.List;

public interface InjectionCreation {

    String getEquipmentId();

    List<FreePropertyInfos> getProperties();

    String getEquipmentName();

    String getVoltageLevelId();

    String getBusOrBusbarSectionId();

    String getConnectionName();

    ConnectablePosition.Direction getConnectionDirection();

    Integer getConnectionPosition();

    boolean isTerminalConnected();
}
