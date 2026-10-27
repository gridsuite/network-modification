/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.reducer.IdentifierNetworkPredicate;
import com.powsybl.iidm.reducer.NetworkReducer;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.ModificationType;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;

@Getter
@Setter
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class NetworkReduction extends AbstractModification {
    private List<Filter> filters;

    @Builder
    public NetworkReduction(List<Filter> filters) {
        this.filters = filters;
    }

    @Override
    public void apply(Network network, ReportNode reportNode) {
        NetworkReducer reducer = NetworkReducer.builder()
                .withBoundaryLines(true)
                .withNetworkPredicate(new IdentifierNetworkPredicate(getEquipmentsFromFilters(network).stream()
                        .map(Identifiable::getId)
                        .toList()))
                .build();
        reducer.reduce(network);
    }

    @Override
    public String getName() {
        return ModificationType.NETWORK_REDUCTION.name();
    }

    private Collection<Identifiable<?>> getEquipmentsFromFilters(Network network) {
        Collection<Identifiable<?>> evaluatedEquipments = new LinkedHashSet<>();
        for (Filter filter : filters) {
            evaluatedEquipments.addAll(filter.evaluate(network));
        }
        return evaluatedEquipments;
    }
}
