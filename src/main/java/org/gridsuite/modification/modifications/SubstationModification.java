/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import lombok.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.List;

import static org.gridsuite.modification.NetworkModificationException.Type.SUBSTATION_NOT_FOUND;

/*
 * @author David Braquart <david.braquart at rte-france.com>
 */
@Getter
@Setter
public class SubstationModification extends AbstractEquipmentModification {

    private AttributeModification<Country> country;

    @Builder
    public SubstationModification(String equipmentId, List<FreePropertyInfos> properties,
                                  AttributeModification<String> equipmentName, AttributeModification<Country> country) {
        super(equipmentId, properties, equipmentName);
        this.country = country;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Substation station = network.getSubstation(equipmentId);
        if (station == null) {
            throw new NetworkModificationException(SUBSTATION_NOT_FOUND,
                    "Substation " + equipmentId + " does not exist in network");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Substation station = network.getSubstation(equipmentId);

        // modify the substation in the network
        subReportNode.newReportNode()
                .withMessageTemplate("network.modification.substationModification")
                .withUntypedValue("id", equipmentId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        // name and country
        ModificationUtils.getInstance().applyElementaryModifications(station::setName, () -> station.getOptionalName().orElse("No value"), equipmentName, subReportNode, "Name");
        ModificationUtils.getInstance().applyElementaryModifications(station::setCountry, station::getNullableCountry, country, subReportNode, "Country");
        // properties
        PropertiesUtils.applyProperties(station, subReportNode, properties, "network.modification.SubstationProperties");
    }

    @Override
    public String getName() {
        return "SubstationModification";
    }
}
