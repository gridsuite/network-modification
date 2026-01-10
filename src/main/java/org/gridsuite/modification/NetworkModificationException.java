/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification;

import com.powsybl.commons.PowsyblException;

/**
 * @author Mohamed Benrejeb <mohamed.benrejeb at rte-france.com>
 */
public class NetworkModificationException extends PowsyblException {

    public NetworkModificationException(String message) {
        super(message);
    }

    public NetworkModificationException(String message, Throwable cause) {
        super(message, cause);
    }
}
