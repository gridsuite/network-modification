/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

/**
 * A modification whose content is lazily loaded: it carries how deep that content goes, to allow or prevent depth
 * sensitive operations. A composite, or a reference standing for the composite it points to.
 *
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
public interface MaxDepthHolderInfos {
    Integer getMaxDepth();

    void setMaxDepth(Integer maxDepth);
}
