/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.Getter;
import lombok.NonNull;

import org.gridsuite.modification.dto.OperatingStatusModificationInfos;

import java.util.Objects;

import static org.gridsuite.modification.NetworkModificationException.Type.ATTRIBUTE_NOT_EDITABLE;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Getter
public class NetworkModificationException extends PowsyblException {
    public enum Type {
        GROOVY_SCRIPT_EMPTY("Empty script"),
        GROOVY_SCRIPT_ERROR,
        NETWORK_NOT_FOUND,
        VARIANT_NOT_FOUND,
        NOTHING_TO_DELETE,
        MODIFICATION_DELETION_ERROR,
        MODIFICATION_GROUP_NOT_FOUND,
        MODIFICATION_NOT_FOUND,
        SWITCH_NOT_FOUND,
        LINE_NOT_FOUND,
        LOAD_NOT_FOUND,
        BATTERY_NOT_FOUND,
        GENERATOR_NOT_FOUND,
        TWO_WINDINGS_TRANSFORMER_NOT_FOUND,
        UNKNOWN_MODIFICATION_TYPE,
        UNKNOWN_EQUIPMENT_TYPE,
        WRONG_EQUIPMENT_TYPE,
        MODIFICATION_ERROR,
        VOLTAGE_LEVEL_NOT_FOUND,
        CREATE_LOAD_ERROR,
        MODIFY_LOAD_ERROR,
        BUSBAR_SECTION_NOT_FOUND,
        BUS_NOT_FOUND,
        CREATE_BATTERY_ERROR,
        CREATE_GENERATOR_ERROR,
        CREATE_SHUNT_COMPENSATOR_ERROR,
        MODIFY_SHUNT_COMPENSATOR_ERROR,
        CREATE_STATIC_VAR_COMPENSATOR_ERROR,
        DELETE_EQUIPMENT_ERROR,
        BY_FILTER_DELETION_ERROR,
        EQUIPMENT_NOT_FOUND,
        ATTRIBUTE_NOT_EDITABLE,
        CREATE_LINE_ERROR,
        MODIFY_LINE_ERROR,
        CREATE_TWO_WINDINGS_TRANSFORMER_ERROR,
        MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR,
        CREATE_SUBSTATION_ERROR,
        MODIFY_SUBSTATION_ERROR,
        CREATE_VOLTAGE_LEVEL_ERROR,
        MODIFY_VOLTAGE_LEVEL_ERROR,
        SUBSTATION_NOT_FOUND,
        BATTERY_ALREADY_EXISTS,
        LOAD_ALREADY_EXISTS,
        VOLTAGE_LEVEL_ALREADY_EXISTS,
        BUSBAR_SECTION_ALREADY_EXISTS,
        BUSBAR_SECTION_NOT_DEFINED,
        GENERATOR_ALREADY_EXISTS,
        SHUNT_COMPENSATOR_ALREADY_EXISTS,
        SHUNT_COMPENSATOR_NOT_FOUND,
        STATIC_VAR_COMPENSATOR_ALREADY_EXISTS,
        STATIC_VAR_COMPENSATOR_NOT_FOUND,
        LINE_ALREADY_EXISTS,
        TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS,
        TWO_WINDINGS_TRANSFORMER_CREATION_ERROR,
        BRANCH_MODIFICATION_ERROR,
        INJECTION_MODIFICATION_ERROR,
        MODIFY_BATTERY_ERROR,
        OPERATING_STATUS_MODIFICATION_ERROR,
        OPERATING_ACTION_TYPE_EMPTY("Empty operating action type"),
        OPERATING_ACTION_TYPE_UNKNOWN,
        OPERATING_ACTION_TYPE_UNSUPPORTED,
        EQUIPMENT_TYPE_UNSUPPORTED,
        LINE_SPLIT_ERROR,
        LINE_SPLIT_NOT_FOUND,
        LINE_ATTACH_ERROR,
        LINE_ATTACH_DESCRIPTION_ERROR,
        LINE_ATTACH_NOT_FOUND,
        MODIFY_GENERATOR_ERROR,
        TYPE_MISMATCH,
        MISSING_MODIFICATION_DESCRIPTION,
        MODIFICATION_OUT_OF_RANGE,
        DELETE_VOLTAGE_LEVEL_ON_LINE_ERROR,
        DELETE_VOLTAGE_LEVEL_ON_LINE_NOT_FOUND,
        EQUIPMENT_ATTRIBUTE_NAME_ERROR,
        EQUIPMENT_ATTRIBUTE_VALUE_ERROR,
        MOVE_MODIFICATION_ERROR,
        GENERATOR_SCALING_ERROR,
        LOAD_SCALING_ERROR,
        DELETE_ATTACHING_LINE_ERROR,
        DELETE_ATTACHING_LINE_NOT_FOUND,
        FILTERS_NOT_FOUND,
        LOAD_FLOW_PARAMETERS_NOT_FOUND,
        LOAD_FLOW_PARAMETERS_FETCH_ERROR,
        GENERATION_DISPATCH_ERROR,
        BALANCES_ADJUSTMENT_MODIFICATION_ERROR,
        VOLTAGE_INIT_MODIFICATION_ERROR,
        TABULAR_MODIFICATION_ERROR,
        TABULAR_CREATION_ERROR,
        CREATE_VSC_ERROR,
        MODIFY_VSC_ERROR,
        CREATE_LCC_ERROR,
        MODIFY_LCC_ERROR,
        HVDC_LINE_ALREADY_EXISTS,
        VSC_CONVERTER_STATION_NOT_FOUND,
        LCC_CONVERTER_STATION_NOT_FOUND,
        CREATE_CONVERTER_STATION_ERROR,
        MODIFY_CONVERTER_STATION_ERROR,
        LCC_CREATE_CONVERTER_STATION_ERROR,
        LCC_MODIFY_CONVERTER_STATION_ERROR,
        BY_FORMULA_MODIFICATION_ERROR,
        MODIFICATION_BY_ASSIGNMENT_ERROR,
        HVDC_LINE_NOT_FOUND,
        COMPOSITE_MODIFICATION_ERROR,
        WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL,
        UNSUPPORTED_HYBRID_HVDC,
        MODIFY_VOLTAGE_LEVEL_TOPOLOGY_ERROR,
        CREATE_COUPLING_DEVICE_ERROR,
        CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR,
        CREATE_VOLTAGE_LEVEL_SECTION_ERROR,
        MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR,;

        private final String message;

        Type() {
            this(null);
        }

        Type(String message) {
            this.message = message;
        }
    }

    private final Type type;

    public NetworkModificationException(Type type) {
        super(Objects.requireNonNull(type.name()) + ((type.message == null) ? "" : " : " + type.message));
        this.type = type;
    }

    public NetworkModificationException(Type type, Exception cause) {
        super(Objects.requireNonNull(type.name()) + " : " + ((cause.getMessage() == null) ? cause.getClass().getName() : cause.getMessage()), cause);
        this.type = type;
    }

    public NetworkModificationException(Type type, String message) {
        super(Objects.requireNonNull(type.name()) + " : " + Objects.requireNonNull(message));
        this.type = type;
    }

    public static NetworkModificationException createEquipmentTypeUnknown(@NonNull String type) {
        return new NetworkModificationException(Type.UNKNOWN_EQUIPMENT_TYPE, "The equipment type : " + type + " is unknown");
    }

    public static NetworkModificationException createEquipmentTypeNotSupported(@NonNull String type) {
        return new NetworkModificationException(Type.EQUIPMENT_TYPE_UNSUPPORTED, "The equipment type : " + type + " is not supported");
    }

    public static NetworkModificationException createOperatingActionTypeUnsupported(@NonNull OperatingStatusModificationInfos.ActionType type) {
        return new NetworkModificationException(Type.OPERATING_ACTION_TYPE_UNSUPPORTED, "The operating action type : " + type + " is unsupported");
    }

    public static NetworkModificationException createEquipementAttributeNotEditable(@NonNull IdentifiableType equipmentType, @NonNull String attributeName) {
        throw new NetworkModificationException(ATTRIBUTE_NOT_EDITABLE, equipmentType.name() + " attribute '" + attributeName + "' not editable");
    }

    public static NetworkModificationException createHybridHvdcUnsupported(String hvdcId) {
        Objects.requireNonNull(hvdcId);
        return new NetworkModificationException(Type.UNSUPPORTED_HYBRID_HVDC, String.format("The hybrid Hvdc line %s is unsupported", hvdcId));
    }
}
