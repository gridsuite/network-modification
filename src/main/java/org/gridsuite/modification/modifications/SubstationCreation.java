package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import org.gridsuite.modification.model.SubstationCreationModel;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

public class SubstationCreation extends AbstractModification {

    private final SubstationCreationModel modificationInfos;

    public SubstationCreation(SubstationCreationModel modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ModificationUtils.getInstance().createSubstation(modificationInfos, subReportNode, network);
        Substation substation = network.getSubstation(modificationInfos.getEquipmentId());
        // properties
        PropertiesUtils.applyProperties(substation, subReportNode, modificationInfos.getProperties(), "network.modification.SubstationProperties");
    }

    @Override
    public String getName() {
        return "SubstationCreation";
    }
}
