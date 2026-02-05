/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.byfilter.AbstractAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;

import javax.annotation.Nonnull;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class ByFormulaModification extends AbstractModificationByAssignment {
    private final ByFormulaModificationInfos modificationInfos;

    public ByFormulaModification(ByFormulaModificationInfos modificationInfos) {
        super();
        this.modificationInfos = modificationInfos;
    }

    @Override
    public String getModificationTypeLabel() {
        return "formula";
    }

    @Override
    public ModificationInfos getModificationInfos() {
        return modificationInfos;
    }

    @Override
    public IdentifiableType getEquipmentType() {
        return modificationInfos.getIdentifiableType();
    }

    @Override
    public List<AbstractAssignmentInfos> getAssignmentInfosList() {
        return Collections.unmodifiableList(modificationInfos.getFormulaInfosList());
    }

    @Override
    public NetworkModificationException.Type getExceptionType() {
        return BY_FORMULA_MODIFICATION_ERROR;
    }

    @Override
    protected boolean preCheckValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos, List<ReportNode> reports, List<String> notEditableEquipments) {
        FormulaInfos formulaInfos = (FormulaInfos) abstractAssignmentInfos;
        Double value1 = formulaInfos.getFieldOrValue1().getRefOrValue(equipment);
        Double value2 = formulaInfos.getFieldOrValue2().getRefOrValue(equipment);
        if (value1 == null || Double.isNaN(value1) || value2 == null || Double.isNaN(value2)) {
            equipmentNotModifiedCount += 1;
            notEditableEquipments.add(equipment.getId());
            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_EQUIPMENT_MODIFIED_ERROR_EMPTY)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build());
            return false;
        }

        if (value2 == 0 && formulaInfos.getOperator() == Operator.DIVISION) {
            equipmentNotModifiedCount += 1;
            notEditableEquipments.add(equipment.getId());
            reports.add(ReportNode.newRootReportNode()
                    .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
                    .withMessageTemplate(REPORT_KEY_EQUIPMENT_MODIFIED_ERROR_ZERO)
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                    .withSeverity(TypedValue.DETAIL_SEVERITY)
                    .build());
            return false;
        }
        return true;
    }

    @Override
    protected String getNewValue(Identifiable<?> equipment, AbstractAssignmentInfos abstractAssignmentInfos) {
        FormulaInfos formulaInfos = (FormulaInfos) abstractAssignmentInfos;
        Double value1 = formulaInfos.getFieldOrValue1().getRefOrValue(equipment);
        Double value2 = formulaInfos.getFieldOrValue2().getRefOrValue(equipment);
        return applyOperation(formulaInfos.getOperator(), value1, value2).toString();
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
        return "ByFormulaModification";
    }
}
