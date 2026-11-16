# UoM Winter vs. Spring Wheat Validation Pipeline

## 🌾 Project Overview
This repository manages the data processing, validation pipelines, and quality control (QC) reporting for the **Winter vs. Spring Wheat Cultivars** project (University of Melbourne / BSI-PFR validation framework). 

The system orchestrates multi-regional APSIM-X simulations across multiple years and locations using the R `{targets}` package, performing automated date imputation, Haun stage scaling, mass balance auditing, and secure packaging.

## 📁 Repository Architecture

### Core Directories
* **`Dookie2024/`, `Dookie2025/`, `Turretfield2024/` etc.**: Regional project folders containing individual `{targets}` pipelines, raw Excel data, and local configuration scripts.
* **`[Project Folder]/R/`**: Localized script directories within each regional folder holding tailored conversions and bug-fixes for raw data that are too project-specific to be applied universally.
* **`targets_MasterScripts/`**: Universal R utility functions and custom scripts sourced globally across all regional pipelines.
* **`Inputs/`**: Centralized input tables, mapping CSVs, and germination/phenology control parameters.
* **`Met/`**: Standardized APSIM `.met` weather files.
* **`Observed/`**: Finalized, cleaned observation Excel spreadsheets ready for APSIM consumption.
* **`renv/`**: Environment dependency management files (`renv.lock`).

### Key Scripts, Logs & Reports
* **`RunAll.R`**: The Master Control Script that executes all regional `{targets}` pipelines, generates the Quarto QC report, builds the encrypted data package, and triggers the integration export.
* **`CopyFilesForMasterMerge.R`**: Utility script triggered by `RunAll.R` to isolate core integration files into a pristine directory for AppSync merging.
* **`pipeline_log.txt`**: Generated inside each project folder during execution, this log explicitly captures the outputs of pipeline runs, documenting all automated assumptions, biological fixes, and data conversions applied.
* **`QC_Report.qmd` / `QC_Report.html`**: Quarto quality control dashboard providing visual audits, phenology timelines, and pipeline execution logs.
* **`secret_pass.txt`**: Password credential file used for encrypting final output archives.
* **`Observed.zip`**: Secure, password-protected archive containing finalized observed datasets and rendered QC reports.

## 🚀 Execution & Reproducibility

The project utilizes a strict reproducibility framework managed by `{targets}` and `renv`.

### 1. Initialize the Environment
Ensure your R environment matches the project specifications by restoring the library lockfile:
```r
renv::restore()
```

### 2. Configure & Run the Master Pipeline
The entire architecture is triggered via `RunAll.R`. Before executing, open the script to set your build preference:
* **`clean_build <- TRUE`**: Destroys the `{targets}` cache in every project folder and rebuilds the data from scratch.
* **`clean_build <- FALSE`**: Updates outdated or missing nodes only.

Execute the script. Standard output and errors for each regional dataset will route directly to their respective `pipeline_log.txt` files for auditing.

### 3. Generate Reports & Export Data
Once the regional pipelines complete, `RunAll.R` automatically processes the following downstream actions:
* **QC Dashboard:** Renders `QC_Report.qmd` into an HTML dashboard using Quarto.
* **Secure Packaging:** Duplicates the HTML report into the `Observed` folder and compresses it into `Observed.zip` using the encryption key inside `secret_pass.txt`.
* **Pristine Integration Export:** Scans for explicitly defined targets (`*.apsimx`, `Met`, `Inputs`, `Observed.zip`) and copies them into a pristine, isolated directory for the AppSync master merge, leaving all analytical workflows safely behind. 

To access the finalized datasets, collaborators must use the key from `secret_pass.txt` to locally decrypt `Observed.zip`.