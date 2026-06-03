/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.formula;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.model.ByFormulaModificationModel;
import org.gridsuite.modification.model.FilterEquipments;
import org.gridsuite.modification.model.FilterModel;
import org.gridsuite.modification.model.ModificationModel;
import org.gridsuite.modification.model.byfilter.formula.FormulaModel;
import org.gridsuite.modification.model.byfilter.formula.Operator;
import org.gridsuite.modification.model.byfilter.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.report.NetworkModificationReportResourceBundle;
import org.gridsuite.modification.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.*;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
abstract class AbstractByFormulaModificationTest extends AbstractNetworkModificationTest {
    protected static final UUID FILTER_ID_1 = UUID.randomUUID();
    protected static final UUID FILTER_ID_2 = UUID.randomUUID();
    protected static final UUID FILTER_ID_3 = UUID.randomUUID();
    protected static final UUID FILTER_ID_4 = UUID.randomUUID();
    protected static final UUID FILTER_ID_5 = UUID.randomUUID();
    protected static final UUID FILTER_ID_6 = UUID.randomUUID();
    protected static final UUID FILTER_ID_7 = UUID.randomUUID();
    protected static final UUID FILTER_WITH_ALL_WRONG_IDS = UUID.randomUUID();
    protected static final UUID FILTER_WITH_ONE_WRONG_ID = UUID.randomUUID();
    protected final FilterModel filter1 = new FilterModel(FILTER_ID_1, "filter1");
    protected final FilterModel filter2 = new FilterModel(FILTER_ID_2, "filter2");
    protected final FilterModel filter3 = new FilterModel(FILTER_ID_3, "filter3");
    protected final FilterModel filter4 = new FilterModel(FILTER_ID_4, "filter4");
    protected final FilterModel filter5 = new FilterModel(FILTER_ID_5, "filter5");
    protected final FilterModel filter6 = new FilterModel(FILTER_ID_6, "filter6");
    protected final FilterModel filter7 = new FilterModel(FILTER_ID_7, "filter7");
    protected final FilterModel filterWithOneWrongId = new FilterModel(FILTER_WITH_ONE_WRONG_ID, "filterWithOneWrongId");
    protected final ReportNode reportNode = ReportNode.newRootReportNode()
            .withResourceBundles(NetworkModificationReportResourceBundle.BASE_NAME)
            .withMessageTemplate("test")
            .build();

    @Mock
    protected IFilterService filterService;

    @BeforeEach
    void specificSetUp() {
        MockitoAnnotations.openMocks(this);
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        createEquipments();
    }

    @Test
    @Override
    public void testApply() throws Exception {
        ModificationModel modificationInfo = buildModification();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(getTestFilters());
        AbstractModification modification = modificationInfo.toModification();
        modification.initApplicationContext(filterService, null);
        modification.apply(getNetwork(), reportNode);
        assertAfterNetworkModificationApplication();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ByFormulaModificationModel buildModification() {
        return ByFormulaModificationModel.builder()
                .identifiableType(getIdentifiableType())
                .formulaModelList(getFormulaModel())
                .stashed(false)
                .build();
    }

    @Override
    protected void checkModification() {
    }

    protected void apply(ByFormulaModificationModel modificationModel) {
        AbstractModification modification = modificationModel.toModification();
        modification.initApplicationContext(filterService, null);
        modification.apply(getNetwork());
    }

    protected FormulaModel getFormulaInfo(String editedField,
                                List<FilterModel> filters,
                                Operator operator,
                                ReferenceFieldOrValue fieldOrValue1,
                                ReferenceFieldOrValue fieldOrValue2) {
        return FormulaModel.builder()
                .editedField(editedField)
                .filters(filters)
                .operator(operator)
                .fieldOrValue1(fieldOrValue1)
                .fieldOrValue2(fieldOrValue2)
                .build();
    }

    protected abstract void createEquipments();

    protected abstract Map<UUID, FilterEquipments> getTestFilters();

    protected abstract List<FormulaModel> getFormulaModel();

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract EquipmentType getEquipmentType();
}
