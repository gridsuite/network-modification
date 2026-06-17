package org.gridsuite.modification.modifications.data;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.modifications.InjectionCreation;

import java.util.List;

@Getter
@Setter
@SuperBuilder
public abstract class AbstractInjectionCreation implements InjectionCreation {

    private String equipmentId;
    private List<FreePropertyInfos> properties;
    private String equipmentName;
    private String voltageLevelId;
    private String busOrBusbarSectionId;
    private String connectionName;
    private ConnectablePosition.Direction connectionDirection;
    private Integer connectionPosition;
    private boolean terminalConnected;
}
