package org.gridsuite.modification.dto;

public enum AllOperationalLimitsGroupsModificationType {
    // Modification types for Tabular modifications :
    MODIFY, // standard mode : the olg modifications are applied. The unspecified olg are not changed at all
    // Modification type for simple form modifications :
    REPLACE, // All the olg are removed, then the olg modification/add etc are applied
}
