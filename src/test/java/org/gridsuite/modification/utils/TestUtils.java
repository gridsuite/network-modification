/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.utils;

import com.google.common.io.ByteStreams;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import com.powsybl.iidm.network.extensions.OperatingStatusAdder;
import org.apache.commons.text.StringSubstitutor;
import org.junit.platform.commons.util.StringUtils;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public final class TestUtils {

    private TestUtils() {
        throw new IllegalCallerException("Utility class");
    }

    @SuppressWarnings("unchecked")
    public static void assertOperatingStatus(Network network, String identifiableName, OperatingStatus.Status status) {
        assertNotNull(network);
        Identifiable<?> identifiable = network.getIdentifiable(identifiableName);
        assertNotNull(identifiable);
        OperatingStatus operatingStatus = identifiable.getExtensionByName("operatingStatus");
        assertNotNull(operatingStatus);
        assertEquals(status, operatingStatus.getStatus());
    }

    @SuppressWarnings("unchecked")
    public static void setOperatingStatus(Network network, String identifiableName, OperatingStatus.Status status) {
        Identifiable<?> identifiable = network.getIdentifiable(identifiableName);
        assertNotNull(identifiable);
        identifiable.newExtension(OperatingStatusAdder.class).withStatus(status).add();
    }

    public static String resourceToString(String resource) throws IOException {
        InputStream inputStream = Objects.requireNonNull(TestUtils.class.getResourceAsStream(resource));
        String content = new String(ByteStreams.toByteArray(inputStream), StandardCharsets.UTF_8);
        return StringUtils.replaceWhitespaceCharacters(content, "");
    }

    public static void assertLogNthMessage(String expectedMessage, String reportKey, ReportNode reportNode, int rank) {
        Optional<String> message = getMessageFromReporter(reportKey, reportNode, rank);
        assertTrue(message.isPresent());
        assertEquals(expectedMessage, message.get().trim());
    }

    public static void assertLogMessageAtDepth(String expectedMessage, String reportKey, ReportNode reportNode, int depth) {
        Optional<String> message = getMessageFromReporterAtDepth(reportKey, reportNode, 0, depth);
        assertTrue(message.isPresent());
        assertEquals(expectedMessage, message.get().trim());
    }

    public static void assertLogMessage(String expectedMessage, String reportKey, ReportNode reportNode) {
        assertLogNthMessage(expectedMessage, reportKey, reportNode, 1);
    }

    public static void assertLogMessageWithoutRank(String expectedMessage, String reportKey, ReportNode reportNode) {
        assertNotNull(reportNode);
        assertTrue(assertMessageFoundFromReporter(expectedMessage, reportKey, reportNode));
    }

    private static boolean assertMessageFoundFromReporter(String expectedMessage, String reportKey, ReportNode reporterModel) {
        for (ReportNode report : reporterModel.getChildren()) {
            if (report.getMessageKey().equals(reportKey)) {
                String message = formatReportMessage(report, reporterModel);
                if (message.trim().equals(expectedMessage)) {
                    return true;
                }
            }
        }

        boolean foundInSubReporters = false;
        Iterator<ReportNode> reportersIterator = reporterModel.getChildren().iterator();
        while (!foundInSubReporters && reportersIterator.hasNext()) {
            foundInSubReporters = assertMessageFoundFromReporter(expectedMessage, reportKey, reportersIterator.next());
        }
        return foundInSubReporters;
    }

    private static Optional<String> getMessageFromReporterAtDepth(String reportKey, ReportNode reporterModel, int currentDepth, int expectedDepth) {
        Optional<String> message = Optional.empty();

        Iterator<ReportNode> reportersIterator = reporterModel.getChildren().iterator();
        while (message.isEmpty() && reportersIterator.hasNext() && currentDepth < expectedDepth) {
            message = getMessageFromReporterAtDepth(reportKey, reportersIterator.next(), currentDepth + 1, expectedDepth);
        }

        Iterator<ReportNode> reportsIterator = reporterModel.getChildren().iterator();
        while (message.isEmpty() && reportsIterator.hasNext()) {
            ReportNode report = reportsIterator.next();
            if (currentDepth == expectedDepth) {
                if (report.getMessageKey().equals(reportKey)) {
                    message = Optional.of(formatReportMessage(report, reporterModel));
                }
            }
        }

        return message;
    }

    /**
     * @param rank order position inside reporterModel
     */
    private static Optional<String> getMessageFromReporter(String reportKey, ReportNode reporterModel, int rank) {
        Optional<String> message = Optional.empty();

        Iterator<ReportNode> reportsIterator = reporterModel.getChildren().iterator();
        int nbTimes = 0;
        while (message.isEmpty() && reportsIterator.hasNext()) {
            ReportNode report = reportsIterator.next();
            if (report.getMessageKey().equals(reportKey)) {
                nbTimes++;
                if (nbTimes == rank) {
                    message = Optional.of(formatReportMessage(report, reporterModel));
                }
            }
        }

        Iterator<ReportNode> reportersIterator = reporterModel.getChildren().iterator();
        while (message.isEmpty() && reportersIterator.hasNext()) {
            message = getMessageFromReporter(reportKey, reportersIterator.next(), rank);
        }

        return message;
    }

    private static String formatReportMessage(ReportNode report, ReportNode reporterModel) {
        return new StringSubstitutor(reporterModel.getValues()).replace(new StringSubstitutor(report.getValues()).replace(report.getMessageTemplate()));
    }

}
