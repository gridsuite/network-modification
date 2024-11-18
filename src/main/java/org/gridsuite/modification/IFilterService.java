/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Stream;

import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.modification.dto.FilterEquipments;

import com.powsybl.iidm.network.Network;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public interface IFilterService {
    List<AbstractFilter> getFilters(List<UUID> filtersUuids);

    Stream<org.gridsuite.filter.identifierlistfilter.FilterEquipments> exportFilters(List<UUID> filtersUuids, Network network);

    Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(Network network, Map<UUID, String> filters);
}
