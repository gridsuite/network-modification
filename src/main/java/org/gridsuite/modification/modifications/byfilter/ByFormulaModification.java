/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.byfilter;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import jakarta.annotation.Nonnull;
import lombok.*;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.error.NetworkModificationExceptionType;
import org.gridsuite.modification.modifications.data.assignment.AbstractAssignmentData;
import org.gridsuite.modification.modifications.data.assignment.FormulaAssignmentData;
import org.gridsuite.modification.modifications.data.assignment.Operator;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.error.NetworkModificationExceptionType.BY_FORMULA_MODIFICATION_ERROR;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ByFormulaModification extends AbstractModificationByAssignment {
    private IdentifiableType identifiableType;
    private List<FormulaAssignmentData> formulaAssignments;

    @Builder
    public ByFormulaModification(IdentifiableType identifiableType, List<FormulaAssignmentData> formulaAssignments) {
        this.identifiableType = identifiableType;
        this.formulaAssignments = formulaAssignments;
    }

    @Override
    public String getModificationTypeLabel() {
        return "formula";
    }

    @Override
    public NetworkModificationExceptionType getExceptionType() {
        return BY_FORMULA_MODIFICATION_ERROR;
    }

    @Override
    @JsonIgnore
    public IdentifiableType getEquipmentType() {
        return identifiableType;
    }

    @Override
    @JsonIgnore
    public List<AbstractAssignmentData> getAssignments() {
        return Collections.unmodifiableList(formulaAssignments);
    }

    @Override
    protected boolean preCheckValue(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData, ReportNode reportNode) {
        FormulaAssignmentData formulaAssignmentData = (FormulaAssignmentData) abstractAssignmentData;
        Double value1 = formulaAssignmentData.getFieldOrValue1().getRefOrValue(equipment);
        Double value2 = formulaAssignmentData.getFieldOrValue2().getRefOrValue(equipment);
        // value 1 and value 2 cannot be null because getRefOrValue returns NaN if value is null
        if (Double.isNaN(value1) || Double.isNaN(value2)) {
            return reportErrorOnEquipment(equipment, REPORT_KEY_EQUIPMENT_MODIFIED_ERROR_MISSING, reportNode);
        }

        if (value2 == 0 && formulaAssignmentData.getOperator() == Operator.DIVISION) {
            return reportErrorOnEquipment(equipment, REPORT_KEY_EQUIPMENT_MODIFIED_ERROR_ZERO, reportNode);
        }
        if (equipment.getType() == IdentifiableType.GENERATOR) {
            return checkGeneratorsPowerValues(equipment, abstractAssignmentData, reportNode);
        }
        return true;
    }

    private boolean reportErrorOnEquipment(Identifiable<?> equipment, String reportKey, ReportNode reportNode) {
        reportNode.newReportNode()
                .withMessageTemplate(reportKey)
                .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                .withSeverity(TypedValue.WARN_SEVERITY)
                .add();
        return false;
    }

    @Override
    protected String getNewValue(Identifiable<?> equipment, AbstractAssignmentData abstractAssignmentData) {
        FormulaAssignmentData formulaAssignmentData = (FormulaAssignmentData) abstractAssignmentData;
        Double value1 = formulaAssignmentData.getFieldOrValue1().getRefOrValue(equipment);
        Double value2 = formulaAssignmentData.getFieldOrValue2().getRefOrValue(equipment);
        return applyOperation(formulaAssignmentData.getOperator(), value1, value2).toString();
    }

    static final int MAX_SCALE = 10;

    private Double applyOperation(Operator operator, @Nonnull Double value1, @Nonnull Double value2) {
        BigDecimal bValue1 = BigDecimal.valueOf(value1);
        BigDecimal bValue2 = BigDecimal.valueOf(value2);

        return switch (operator) {
            case ADDITION -> bValue1.add(bValue2).doubleValue();
            case SUBTRACTION -> bValue1.subtract(bValue2).doubleValue();
            case MULTIPLICATION -> bValue1.multiply(bValue2).doubleValue();
            case DIVISION -> bValue1.divide(bValue2, MAX_SCALE, RoundingMode.HALF_EVEN).doubleValue();
            case PERCENTAGE -> bValue1.divide(BigDecimal.valueOf(100.0), MAX_SCALE, RoundingMode.HALF_EVEN)
                .multiply(bValue2).doubleValue();
        };
    }

    @Override
    public String getName() {
        return ModificationType.BY_FORMULA_MODIFICATION.name();
    }
}
