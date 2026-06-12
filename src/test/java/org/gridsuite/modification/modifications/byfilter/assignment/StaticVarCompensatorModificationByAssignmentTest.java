/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.BooleanAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.StaticVarCompensatorField;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.utils.MeasurementUtils.getExistingMeasurement;
import static org.gridsuite.modification.utils.NetworkUtil.createStaticVarCompensator;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
class StaticVarCompensatorModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
    private static final String SVC_ID_1 = "svc1";
    private static final String SVC_ID_2 = "svc2";
    private static final Double REACTIVE_POWER_MEASUREMENT_VALUE_FOR_SVC1 = 25.;
    private static final boolean REACTIVE_POWER_MEASUREMENT_VALIDITY_FOR_SVC2 = false;

    @Override
    protected void createEquipments() {
        createStaticVarCompensator(getNetwork().getVoltageLevel("v1"), SVC_ID_1, SVC_ID_1, 8, StaticVarCompensator.RegulationMode.VOLTAGE, 100.0, -50., 80., 120.);
        createStaticVarCompensator(getNetwork().getVoltageLevel("v3"), SVC_ID_2, SVC_ID_2, 10, StaticVarCompensator.RegulationMode.VOLTAGE, 100.0, -50., 80., 120.);
    }

    @Override
    protected Map<UUID, FilterEquipments> getTestFilters() {
        FilterEquipments filter1 = FilterEquipments.builder().filterId(FILTER_ID_1).identifiableAttributes(List.of(
            new IdentifiableAttributes(SVC_ID_1, IdentifiableType.STATIC_VAR_COMPENSATOR, 1.0),
            new IdentifiableAttributes(SVC_ID_2, IdentifiableType.STATIC_VAR_COMPENSATOR, 2.0)))
            .build();
        return Map.of(FILTER_ID_1, filter1);
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .editedField(StaticVarCompensatorField.REACTIVE_POWER_MEASUREMENT_VALUE.name())
                .value(REACTIVE_POWER_MEASUREMENT_VALUE_FOR_SVC1)
                .filters(List.of(filter1))
                .build();

        BooleanAssignmentInfos assignmentInfos2 = BooleanAssignmentInfos.builder()
                .editedField(StaticVarCompensatorField.REACTIVE_POWER_MEASUREMENT_VALIDITY.name())
                .value(REACTIVE_POWER_MEASUREMENT_VALIDITY_FOR_SVC2)
                .filters(List.of(filter1))
                .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1, assignmentInfos2));

        return infosList;
    }

    @Override
    protected void assertAfterNetworkModificationApplication() {
        StaticVarCompensator svc1 = getNetwork().getStaticVarCompensator(SVC_ID_1);
        StaticVarCompensator svc2 = getNetwork().getStaticVarCompensator(SVC_ID_2);
        Measurements<?> ms1 = (Measurements<?>) svc1.getExtension(Measurements.class);
        assertNotNull(ms1);
        Measurement svc1Measurement = getExistingMeasurement(ms1, Measurement.Type.REACTIVE_POWER);
        assertNotNull(svc1Measurement);
        Measurements<?> ms2 = (Measurements<?>) svc2.getExtension(Measurements.class);
        assertNotNull(ms2);
        Measurement svc2Measurement = getExistingMeasurement(ms2, Measurement.Type.REACTIVE_POWER);
        assertNotNull(svc2Measurement);

        assertThat(svc1Measurement.getValue()).isEqualTo(REACTIVE_POWER_MEASUREMENT_VALUE_FOR_SVC1);
        assertThat(svc2Measurement.isValid()).isEqualTo(REACTIVE_POWER_MEASUREMENT_VALIDITY_FOR_SVC2);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.STATIC_VAR_COMPENSATOR;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.STATIC_VAR_COMPENSATOR;
    }

}
