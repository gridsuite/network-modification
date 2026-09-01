/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.error;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.NonNull;
import org.gridsuite.modification.dto.OperatingStatusModificationInfos;

import java.util.Objects;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class NetworkModificationException extends PowsyblException {

    public NetworkModificationException(NetworkModificationExceptionType exceptionType) {
        super(Objects.requireNonNull(exceptionType.getMessage()));
    }

    public NetworkModificationException(NetworkModificationExceptionType exceptionType, Exception cause) {
        super(Objects.requireNonNull(exceptionType.getMessage()) + " : " + ((cause.getMessage() == null) ? cause.getClass().getName() : cause.getMessage()), cause);
    }

    public NetworkModificationException(NetworkModificationExceptionType exceptionType, String message) {
        super(Objects.requireNonNull(exceptionType.getMessage()) + " : " + Objects.requireNonNull(message));
    }

    public static NetworkModificationException createEquipmentTypeUnknown(@NonNull String type) {
        return new NetworkModificationException(NetworkModificationExceptionType.UNKNOWN_EQUIPMENT_TYPE, "The equipment type : " + type + " is unknown");
    }

    public static NetworkModificationException createEquipmentTypeNotSupported(@NonNull String type) {
        return new NetworkModificationException(NetworkModificationExceptionType.EQUIPMENT_TYPE_UNSUPPORTED, "The equipment type : " + type + " is not supported");
    }

    public static NetworkModificationException createOperatingActionTypeUnsupported(@NonNull OperatingStatusModificationInfos.ActionType type) {
        return new NetworkModificationException(NetworkModificationExceptionType.OPERATING_ACTION_TYPE_UNSUPPORTED, "The operating action type : " + type + " is unsupported");
    }

    public static NetworkModificationException createEquipementAttributeNotEditable(@NonNull IdentifiableType equipmentType, @NonNull String attributeName) {
        throw new NetworkModificationException(NetworkModificationExceptionType.ATTRIBUTE_NOT_EDITABLE, equipmentType.name() + " attribute '" + attributeName + "' not editable");
    }

    public static NetworkModificationException createHybridHvdcUnsupported(String hvdcId) {
        Objects.requireNonNull(hvdcId);
        return new NetworkModificationException(NetworkModificationExceptionType.UNSUPPORTED_HYBRID_HVDC, String.format("The hybrid Hvdc line %s is unsupported", hvdcId));
    }
}
