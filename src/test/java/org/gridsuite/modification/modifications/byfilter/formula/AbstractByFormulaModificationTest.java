/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.modifications.byfilter.formula;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.AbstractNetworkModificationTest;
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
    protected final FilterInfos filter1 = new FilterInfos(FILTER_ID_1, "filter1");
    protected final FilterInfos filter2 = new FilterInfos(FILTER_ID_2, "filter2");
    protected final FilterInfos filter3 = new FilterInfos(FILTER_ID_3, "filter3");
    protected final FilterInfos filter4 = new FilterInfos(FILTER_ID_4, "filter4");
    protected final FilterInfos filter5 = new FilterInfos(FILTER_ID_5, "filter5");
    protected final FilterInfos filter6 = new FilterInfos(FILTER_ID_6, "filter6");
    protected final FilterInfos filter7 = new FilterInfos(FILTER_ID_7, "filter7");
    protected final FilterInfos filterWithOneWrongId = new FilterInfos(FILTER_WITH_ONE_WRONG_ID, "filterWithOneWrongId");

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
        ModificationInfos modificationInfo = buildModification();
        when(filterService.getUuidFilterEquipmentsMap(any(), any())).thenReturn(getTestFilters());
        AbstractModification modification = modificationInfo.toModification();
        modification.initApplicationContext(filterService);
        modification.apply(getNetwork());
        assertAfterNetworkModificationApplication();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ByFormulaModificationInfos buildModification() {
        return ByFormulaModificationInfos.builder()
                .identifiableType(getIdentifiableType())
                .formulaInfosList(getFormulaInfos())
                .stashed(false)
                .build();
    }

    @Override
    protected void checkModification() {
    }

    protected void apply(ByFormulaModificationInfos modificationInfos) {
        AbstractModification modification = modificationInfos.toModification();
        modification.initApplicationContext(filterService);
        modification.apply(getNetwork());
    }

    protected FormulaInfos getFormulaInfo(String editedField,
                                List<FilterInfos> filters,
                                Operator operator,
                                ReferenceFieldOrValue fieldOrValue1,
                                ReferenceFieldOrValue fieldOrValue2) {
        return FormulaInfos.builder()
                .editedField(editedField)
                .filters(filters)
                .operator(operator)
                .fieldOrValue1(fieldOrValue1)
                .fieldOrValue2(fieldOrValue2)
                .build();
    }

    protected abstract void createEquipments();

    protected abstract Map<UUID, FilterEquipments> getTestFilters();

    protected abstract List<FormulaInfos> getFormulaInfos();

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract EquipmentType getEquipmentType();
}
