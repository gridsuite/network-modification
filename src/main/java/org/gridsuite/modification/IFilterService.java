package org.gridsuite.modification;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Stream;

import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.modification.dto.FilterEquipments;

import com.powsybl.iidm.network.Network;

public interface IFilterService {
    List<AbstractFilter> getFilters(List<UUID> filtersUuids);

    Stream<org.gridsuite.filter.identifierlistfilter.FilterEquipments> exportFilters(List<UUID> filtersUuids, Network network);

    Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(Network network, Map<UUID, String> filters);
}
