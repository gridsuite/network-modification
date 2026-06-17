package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import lombok.*;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.List;

@Getter
@Setter
public class SubstationCreation extends AbstractEquipmentCreation {

    private Country country;

    @Builder
    public SubstationCreation(String equipmentId, List<FreePropertyInfos> properties, String equipmentName,
                              Country country) {
        super(equipmentId, properties, equipmentName);
        this.country = country;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ModificationUtils.getInstance().createSubstation(this, subReportNode, network);
        Substation substation = network.getSubstation(equipmentId);
        // properties
        PropertiesUtils.applyProperties(substation, subReportNode, properties, "network.modification.SubstationProperties");
    }

    @Override
    public String getName() {
        return "SubstationCreation";
    }

}
