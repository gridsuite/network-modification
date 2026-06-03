package org.gridsuite.modification.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import org.gridsuite.modification.model.SubstationCreationModel;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

public class SubstationCreation extends AbstractModification {

    private final SubstationCreationModel modificationModel;

    public SubstationCreation(SubstationCreationModel modificationModel) {
        this.modificationModel = modificationModel;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ModificationUtils.getInstance().createSubstation(modificationModel, subReportNode, network);
        Substation substation = network.getSubstation(modificationModel.getEquipmentId());
        // properties
        PropertiesUtils.applyProperties(substation, subReportNode, modificationModel.getProperties(), "network.modification.SubstationProperties");
    }

    @Override
    public String getName() {
        return "SubstationCreation";
    }
}
