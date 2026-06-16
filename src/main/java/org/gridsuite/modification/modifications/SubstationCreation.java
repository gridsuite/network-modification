package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import java.util.List;

@NoArgsConstructor
@Getter
@AllArgsConstructor
@Builder
public class SubstationCreation extends AbstractModification {

    private String equipmentId;
    private List<FreePropertyInfos> properties;
    private String equipmentName;
    private Country country;

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ModificationUtils.getInstance().createSubstation(equipmentId, equipmentName, country, subReportNode, network);
        Substation substation = network.getSubstation(equipmentId);
        // properties
        PropertiesUtils.applyProperties(substation, subReportNode, properties, "network.modification.SubstationProperties");
    }

    @Override
    public String getName() {
        return "SubstationCreation";
    }

}
