/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.error;

import java.util.Objects;

/**
 * @author Ghiles Abdellah {@literal <ghiles.abdellah at rte-france.com>}
 */
public enum NetworkModificationExceptionType {
    GROOVY_SCRIPT_EMPTY("The groovy script is empty"),
    LINE_NOT_FOUND("The line could not be found"),
    LOAD_NOT_FOUND("The load could not be found"),
    BATTERY_NOT_FOUND("The battery could not be found"),
    GENERATOR_NOT_FOUND("The generator could not be found"),
    TWO_WINDINGS_TRANSFORMER_NOT_FOUND("The two windings transformer could not be found"),
    UNKNOWN_EQUIPMENT_TYPE("The equipment type is unknown"),
    WRONG_EQUIPMENT_TYPE("The equipment type does not match the expected type"),
    MODIFICATION_ERROR("An error occurred while applying the modification"),
    VOLTAGE_LEVEL_NOT_FOUND("The voltage level could not be found"),
    BUSBAR_SECTION_NOT_FOUND("The busbar section could not be found"),
    BUS_NOT_FOUND("The bus could not be found"),
    CREATE_BATTERY_ERROR("An error occurred while creating the battery"),
    CREATE_GENERATOR_ERROR("An error occurred while creating the generator"),
    CREATE_SHUNT_COMPENSATOR_ERROR("An error occurred while creating the shunt compensator"),
    MODIFY_SHUNT_COMPENSATOR_ERROR("An error occurred while modifying the shunt compensator"),
    CREATE_STATIC_VAR_COMPENSATOR_ERROR("An error occurred while creating the static var compensator"),
    EQUIPMENT_NOT_FOUND("The equipment could not be found"),
    ATTRIBUTE_NOT_EDITABLE("The equipment attribute is not editable"),
    CREATE_LINE_ERROR("An error occurred while creating the line"),
    MODIFY_LINE_ERROR("An error occurred while modifying the line"),
    CREATE_TWO_WINDINGS_TRANSFORMER_ERROR("An error occurred while creating the two windings transformer"),
    MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR("An error occurred while modifying the two windings transformer"),
    CREATE_VOLTAGE_LEVEL_ERROR("An error occurred while creating the voltage level"),
    MODIFY_VOLTAGE_LEVEL_ERROR("An error occurred while modifying the voltage level"),
    SUBSTATION_NOT_FOUND("The substation could not be found"),
    BATTERY_ALREADY_EXISTS("A battery with this identifier already exists"),
    LOAD_ALREADY_EXISTS("A load with this identifier already exists"),
    VOLTAGE_LEVEL_ALREADY_EXISTS("A voltage level with this identifier already exists"),
    GENERATOR_ALREADY_EXISTS("A generator with this identifier already exists"),
    SHUNT_COMPENSATOR_ALREADY_EXISTS("A shunt compensator with this identifier already exists"),
    SHUNT_COMPENSATOR_NOT_FOUND("The shunt compensator could not be found"),
    STATIC_VAR_COMPENSATOR_ALREADY_EXISTS("A static var compensator with this identifier already exists"),
    STATIC_VAR_COMPENSATOR_NOT_FOUND("The static var compensator could not be found"),
    LINE_ALREADY_EXISTS("A line with this identifier already exists"),
    TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS("A two windings transformer with this identifier already exists"),
    TWO_WINDINGS_TRANSFORMER_CREATION_ERROR("An error occurred while creating the two windings transformer"),
    BRANCH_MODIFICATION_ERROR("An error occurred while modifying the branch"),
    INJECTION_MODIFICATION_ERROR("An error occurred while modifying the injection"),
    MODIFY_BATTERY_ERROR("An error occurred while modifying the battery"),
    OPERATING_STATUS_MODIFICATION_ERROR("An error occurred while modifying the operating status"),
    OPERATING_ACTION_TYPE_EMPTY("The operating action type is empty"),
    OPERATING_ACTION_TYPE_UNSUPPORTED("The operating action type is not supported"),
    EQUIPMENT_TYPE_UNSUPPORTED("The equipment type is not supported"),
    MODIFY_GENERATOR_ERROR("An error occurred while modifying the generator"),
    EQUIPMENT_ATTRIBUTE_NAME_ERROR("The equipment attribute name is invalid"),
    EQUIPMENT_ATTRIBUTE_VALUE_ERROR("The equipment attribute value is invalid"),
    GENERATOR_SCALING_ERROR("An error occurred while scaling the generators"),
    LOAD_SCALING_ERROR("An error occurred while scaling the loads"),
    GENERATION_DISPATCH_ERROR("An error occurred while dispatching the generation"),
    VOLTAGE_INIT_MODIFICATION_ERROR("An error occurred while applying the voltage init modification"),
    TABULAR_MODIFICATION_ERROR("An error occurred while applying the tabular modification"),
    TABULAR_CREATION_ERROR("An error occurred while applying the tabular creation"),
    CREATE_VSC_ERROR("An error occurred while creating the VSC converter station"),
    MODIFY_VSC_ERROR("An error occurred while modifying the VSC converter station"),
    CREATE_LCC_ERROR("An error occurred while creating the LCC converter station"),
    MODIFY_LCC_ERROR("An error occurred while modifying the LCC converter station"),
    HVDC_LINE_ALREADY_EXISTS("An HVDC line with this identifier already exists"),
    VSC_CONVERTER_STATION_NOT_FOUND("The VSC converter station could not be found"),
    LCC_CONVERTER_STATION_NOT_FOUND("The LCC converter station could not be found"),
    BY_FORMULA_MODIFICATION_ERROR("An error occurred while applying the modification by formula"),
    MODIFICATION_BY_ASSIGNMENT_ERROR("An error occurred while applying the modification by assignment"),
    HVDC_LINE_NOT_FOUND("The HVDC line could not be found"),
    WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL("The HVDC angle droop active power control configuration is invalid"),
    UNSUPPORTED_HYBRID_HVDC("The hybrid HVDC line is not supported"),
    MODIFY_VOLTAGE_LEVEL_TOPOLOGY_ERROR("An error occurred while modifying the voltage level topology"),
    CREATE_VOLTAGE_LEVEL_TOPOLOGY_ERROR("An error occurred while creating the voltage level topology"),
    MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR("An error occurred while moving the voltage level feeder bays");

    private final String message;

    NetworkModificationExceptionType(String message) {
        this.message = Objects.requireNonNull(message);
    }

    public String getMessage() {
        return message;
    }
}
