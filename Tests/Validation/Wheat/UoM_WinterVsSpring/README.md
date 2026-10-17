# UoM Winter vs. Spring Wheat Validation Pipeline

## 🌾 Project Overview
This repository manages the data processing, validation pipelines, and quality control (QC) reporting for the **Winter vs. Spring Wheat Cultivars** project (University of Melbourne / BSI-PFR validation framework). 

The system orchestrates multi-regional APSIM-X simulations across multiple years and locations using the R `{targets}` package, performing automated date imputation, Haun stage scaling, mass balance auditing, and secure packaging.

---

## 📁 Repository Architecture

### Core Directories
* **`Dookie2024/`, `Dookie2025/`, `Fords2025/`, `Gnarwarre2024/`, `Gnarwarre2025/`, `GrassPatch2024/`, `GrassPatch2025/`, `Turretfield2024/`, `WaggaWagga2024/`, `WaggaWagga2025/`**: Regional project folders containing individual `{targets}` pipelines, raw Excel data, and local configuration scripts.
* **`targets_MasterScripts/`**: Universal R utility functions and custom scripts sourced globally across all regional pipelines.
* **`Inputs/`**: Centralized input tables, mapping CSVs, and germination/phenology control parameters.
* **`Met/`**: Standardized APSIM `.met` weather files.
* **`Observed/`**: Finalized, cleaned observation Excel spreadsheets ready for APSIM consumption (includes injected Quarto reports).
* **`renv/`**: Environment dependency management files (`renv.lock`).

### Key Scripts & Reports
* **`RunAll.R`**: The Master Control Script that executes all regional `{targets}` pipelines, generates the Quarto QC report, and builds the encrypted data package.
* **`QC_Report.qmd` / `QC_Report.html`**: Quarto quality control dashboard providing visual audits, phenology timelines, and pipeline execution logs.
* **`secret_pass.txt`**: Password credential file used for encrypting final output archives.
* **`Observed.zip`**: Secure, password-protected archive containing finalized observed datasets and rendered QC reports.

---

## 🚀 Execution & Reproducibility

The project utilizes a strict reproducibility framework managed by `{targets}` and `renv`.

### 1. Initialize the Environment
Ensure your R environment matches the project specifications by restoring the library lockfile:
```r
renv::restore()