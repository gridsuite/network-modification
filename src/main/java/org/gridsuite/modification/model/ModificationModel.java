/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.model;

import com.powsybl.commons.report.ReportNode;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.model.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.modifications.AbstractModification;

import java.util.Map;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public interface ModificationModel {

    default ReportNode createSubReportNode(ReportNode reportNode) {
        throw new UnsupportedOperationException("Method createSubReportNode must be implemented in subclass " + this.getClass().getSimpleName());
    }

    default AbstractModification toModification() {
        throw new UnsupportedOperationException("Method toModification must be implemented in subclass " + this.getClass().getSimpleName());
    }

    default NetworkModificationException.Type getErrorType() {
        Class<?> currentClass = this.getClass();
        while (currentClass != null) {
            ModificationErrorTypeName annotation = currentClass.getAnnotation(ModificationErrorTypeName.class);
            if (annotation != null) {
                return NetworkModificationException.Type.valueOf(annotation.value());
            }
            currentClass = currentClass.getSuperclass();
        }
        throw new IllegalStateException("No ModificationErrorTypeName annotation found for " + this.getClass().getSimpleName());
    }

    ModificationType getType();

    default Map<String, String> getMapMessageValues() {
        return Map.of();
    }

    default void check() {
        // To check input DTO before hypothesis creation. Nothing to check here
    }
}
