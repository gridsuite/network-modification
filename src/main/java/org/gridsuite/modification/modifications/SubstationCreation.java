package org.gridsuite.modification.modifications;

import org.gridsuite.modification.dto.SubstationCreationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.gridsuite.modification.utils.PropertiesUtils;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;

public class SubstationCreation extends AbstractModification {

    private final SubstationCreationInfos modificationInfos;

    public SubstationCreation(SubstationCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Substation substation = network.newSubstation()
                .setId(modificationInfos.getEquipmentId())
                .setName(modificationInfos.getEquipmentName())
                .setCountry(modificationInfos.getCountry())
                .add();

        subReportNode.newReportNode()
                .withMessageTemplate("substationCreated", "New substation with id=${id} created")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        // name and country
        if (modificationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, modificationInfos.getEquipmentName(), "Name");
        }
        if (modificationInfos.getCountry() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, modificationInfos.getCountry(), "Country");
        }
        // properties
        PropertiesUtils.applyProperties(substation, subReportNode, modificationInfos.getProperties(), "SubstationProperties");
    }

    @Override
    public String getName() {
        return "SubstationCreation";
    }
}
