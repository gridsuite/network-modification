/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Country;
import com.powsybl.loadflow.LoadFlowParameters;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.modifications.BalancesAdjustmentModification;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Balances adjustment modification infos")
@JsonTypeName("BALANCES_ADJUSTMENT_MODIFICATION")
public class BalancesAdjustmentModificationInfos extends ModificationInfos {
    public static final int DEFAULT_MAX_NUMBER_ITERATIONS = 5;
    public static final double DEFAULT_THRESHOLD_NET_POSITION = 1;
    public static final List<Country> DEFAULT_COUNTRIES_TO_BALANCE = Collections.emptyList();
    public static final LoadFlowParameters.BalanceType DEFAULT_BALANCE_TYPE = LoadFlowParameters.BalanceType.PROPORTIONAL_TO_LOAD;
    public static final boolean DEFAULT_WITH_LOAD_FLOW = true;
    public static final boolean DEFAULT_WITH_RATIO_TAP_CHANGERS = false;
    public static final boolean DEFAULT_SUBTRACT_LOAD_FLOW_BALANCING = false;

    List<BalancesAdjustmentAreaInfos> areas;

    @Builder.Default
    private int maxNumberIterations = DEFAULT_MAX_NUMBER_ITERATIONS;

    @Builder.Default
    private double thresholdNetPosition = DEFAULT_THRESHOLD_NET_POSITION;

    @Builder.Default
    private List<Country> countriesToBalance = DEFAULT_COUNTRIES_TO_BALANCE;

    @Builder.Default
    private LoadFlowParameters.BalanceType balanceType = DEFAULT_BALANCE_TYPE;

    @Builder.Default
    private boolean withLoadFlow = DEFAULT_WITH_LOAD_FLOW;

    private UUID loadFlowParametersId;

    @Builder.Default
    private boolean withRatioTapChangers = DEFAULT_WITH_RATIO_TAP_CHANGERS;

    @Builder.Default
    private boolean subtractLoadFlowBalancing = DEFAULT_SUBTRACT_LOAD_FLOW_BALANCING;

    @Override
    public AbstractModification toModification(ModificationContext modificationContext) {
        LoadFlowParametersResolution loadFlowParameters = resolveLoadFlowParameters(modificationContext);
        return BalancesAdjustmentModification.builder()
                .areas(getAreas())
                .maxNumberIterations(getMaxNumberIterations())
                .thresholdNetPosition(getThresholdNetPosition())
                .countriesToBalance(getCountriesToBalance())
                .balanceType(getBalanceType())
                .withLoadFlow(isWithLoadFlow())
                .loadFlowParameters(loadFlowParameters.parameters())
                .defaultLoadFlowParametersReason(loadFlowParameters.defaultReason())
                .withRatioTapChangers(isWithRatioTapChangers())
                .subtractLoadFlowBalancing(isSubtractLoadFlowBalancing())
                .build();
    }

    private LoadFlowParametersResolution resolveLoadFlowParameters(ModificationContext modificationContext) {
        if (!isWithLoadFlow()) {
            return LoadFlowParametersResolution.NOT_NEEDED;
        }
        if (loadFlowParametersId == null) {
            return LoadFlowParametersResolution.useDefault("Load flow parameters ID is null");
        }
        return modificationContext.loadFlowParametersLoader().load(loadFlowParametersId)
                .map(parameters -> parameters.getProvider() == null
                        ? LoadFlowParametersResolution.useDefault("Load flow provider is null in parameters with id " + loadFlowParametersId)
                        : LoadFlowParametersResolution.found(parameters))
                .orElseGet(() -> LoadFlowParametersResolution.useDefault("Load flow parameters with id " + loadFlowParametersId + " not found"));
    }

    /**
     * Either the load flow parameters to use, or the reason why the default ones are used instead.
     */
    private record LoadFlowParametersResolution(LoadFlowParametersInfos parameters, String defaultReason) {
        static final LoadFlowParametersResolution NOT_NEEDED = new LoadFlowParametersResolution(null, null);

        static LoadFlowParametersResolution found(LoadFlowParametersInfos parameters) {
            return new LoadFlowParametersResolution(parameters, null);
        }

        static LoadFlowParametersResolution useDefault(String reason) {
            return new LoadFlowParametersResolution(null, reason);
        }
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate("network.modification.balancesAdjustment").add();
    }
}
