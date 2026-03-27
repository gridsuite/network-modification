package org.gridsuite.modification.dto;

/**
 * @author El Cheikh Bassel <bassel.el-cheikh_externe at rte-france.com>
 */

public record LineSegmentInfos(String segmentTypeId,
                               Integer segmentDistanceValue,
                               String area,
                               String temperature,
                               Double shapeFactor) {
}
