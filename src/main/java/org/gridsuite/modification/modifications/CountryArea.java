/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.modifications;

import com.powsybl.iidm.network.*;

import java.util.List;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public class CountryArea {
    private final List<Load> loadsCache;
    private final List<Generator> generatorsCache;

    public CountryArea(Network network, List<Country> countries) {
        loadsCache = network.getLoadStream()
            .filter(load -> isInCountry(load, countries))
            .toList();
        generatorsCache = network.getGeneratorStream()
            .filter(generator -> isInCountry(generator, countries))
            .toList();
    }

    public double getNetPosition() {
        return generatorsCache.stream().mapToDouble(Generator::getTargetP).sum() - loadsCache.stream().mapToDouble(Load::getP0).sum();
    }

    private static boolean isInCountry(Injection<?> injection, List<Country> countries) {
        return injection.getTerminal().getVoltageLevel().getSubstation().flatMap(Substation::getCountry).map(countries::contains).orElse(false);
    }
}
