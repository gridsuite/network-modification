# GridSuite Network Modification

[![Actions Status](https://github.com/gridsuite/network-modification/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/gridsuite/network-modification/actions)
[![Coverage Status](https://sonarcloud.io/api/project_badges/measure?project=org.gridsuite%3Anetwork-modification&metric=coverage)](https://sonarcloud.io/component_measures?id=org.gridsuite%3Anetwork-modification&metric=coverage)
[![MPL-2.0 License](https://img.shields.io/badge/license-MPL_2.0-blue.svg)](https://www.mozilla.org/en-US/MPL/2.0/)

## Overview

`gridsuite-network-modification` is a Java library designed to apply structural, operational, and topological modifications to electrical power networks. As a core component of the [GridSuite](http://www.gridsuite.org/) platform, it is built on top of [PowSyBl](https://www.powsybl.org/) (Power System Blocks).

The library provides a clean separation of concerns:
- **Data Transfer Objects (DTOs)**: Model modification requests, rules, and configurations (e.g., equipment creation, deletion, parameter adjustments, filter/formula-based modifications, and tabular changes).
- **Modification Implementations**: Execute the business logic that mutates a PowSyBl `Network` model.
- **Reporting & Validation**: Integrated i18n reporting (PowSyBl `ReportNode`) and constraint validation.

## Technology Stack

- **Language**: Java 25 (configured in `pom.xml`)
- **Build & Dependency Management**: Apache Maven
- **Core Power System Framework**: [PowSyBl](https://www.powsybl.org/) (IIDM API/Implementation, Load-Flow API, Open Load-Flow, Balances Adjustment)
- **Serialization & Validation**: Jackson, Swagger/OpenAPI v3 annotations, Jakarta Validation API
- **Scripting & Expressions**: Apache Groovy
- **Code Generation**: Project Lombok
- **Testing**: JUnit 5, Spring Boot Test, PowSyBl Config Test, JaCoCo

## Requirements

- **JDK**: Java 25 or higher
- **Maven**: Version 3.8.x or higher

## Setup and Installation

### Adding as a Maven Dependency

Add the following dependency to your project's `pom.xml`:

```xml
<dependency>
    <groupId>org.gridsuite</groupId>
    <artifactId>gridsuite-network-modification</artifactId>
    <version>1.8.0-SNAPSHOT</version>
</dependency>
```

### Building Locally

To build and install the library into your local Maven repository (`~/.m2/repository`):

```bash
mvn clean install
```

## Build and Scripts Commands

Common Maven commands for development and CI:

- **Compile sources**:
  ```bash
  mvn clean compile
  ```
- **Package JAR**:
  ```bash
  mvn package
  ```
- **Run Checkstyle validation**:
  ```bash
  mvn checkstyle:check
  ```
- **Generate JaCoCo coverage report**:
  ```bash
  mvn test jacoco:report
  ```

## Environment Variables

This project is a library and does not require standalone runtime environment variables. However, standard build/tooling configuration applies:

| Variable | Description | Default / Example |
|---|---|---|
| `JAVA_HOME` | Path to JDK 25 installation directory | `/path/to/jdk-25` |
| `MAVEN_OPTS` | JVM options passed to Maven builds | `-Xmx2048m` |
| `SONAR_TOKEN` | SonarCloud authentication token (used in CI) | Secret / CI only |
| `REPO_ACCESS_TOKEN` | Repository access token (used in CI release workflows) | Secret / CI only |

## Tests

The project includes unit and integration tests covering modifications, DTO serialization, formula evaluation, and report generation.

To run tests:
```bash
mvn test
```

To run a specific test class:
```bash
mvn test -Dtest=ModificationTest
```

## Project Structure

```
gridsuite-network-modification
├── .github/workflows/          # GitHub Actions CI/CD workflows
├── docs/                       # Detailed architectural and API documentation
│   ├── API.md                  # API reference
│   └── ARCHITECTURE.md         # Architecture documentation
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── org/gridsuite/modification/
│   │   │       ├── dto/            # Data Transfer Objects (deserialization, models)
│   │   │       ├── error/          # Exception types and error handling
│   │   │       ├── modifications/  # Concrete modification business logic
│   │   │       ├── report/         # Report bundles and i18n support
│   │   │       ├── utils/          # Utility classes and helpers
│   │   │       ├── IFilterService.java
│   │   │       └── ILoadFlowService.java
│   │   ├── java-templates/         # Version templates processed at build time
│   │   └── resources/              # Internationalization and message bundles
│   └── test/
│       ├── java/                   # Unit and integration test suites
│       └── resources/              # Test configurations and network fixtures
├── pom.xml                     # Maven project configuration
├── LICENSE                     # Mozilla Public License 2.0
└── README.md                   # Repository documentation
```

## Documentation

- [Architecture Overview](docs/ARCHITECTURE.md)
- [API Reference](docs/API.md)

## License

This project is licensed under the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/) (MPL-2.0). See the [LICENSE](LICENSE) file for details.
