package org.gridsuite.modification.utils;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;

@Getter
public record FeederData(String connectionName, Integer order, ConnectablePosition.Direction direction) {
}
