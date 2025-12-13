## Project Description

This repository contains a course project for PSY 8960 (Fall 2025), which is about creating and managing reproducible research projects. 
The project explores how perceived interactivity in advertising relates to donation intentions using simulated data. 

## Directory Structure

```text
PSY8960-Fall25-Zhanming/
├─ Archives/                                            # Non active materials kept for posterity
│  ├─ archived_data_testing/                            # Non active materials for data simulation
│  └─ archived_report_drafts/                           # Non active materials for report generation
├─ Code/                                                # Code/scripts for data simulating, cleaning, analysis
├─ Data/                                                # All project data and data dictionary
├─ Reports/                                             # Manuscripts and knitted reports
│  └─ interactive_ads_report_papaja_20251212_files/     # Figures generated when knitting
├─ Procedure/                                           # Course practice files (now empty)
├─ PSY8960-Fall25-Zhanming.Rproj                        # RStudio project entry
└─ README.md                                            # Project overview (this file)
```

## Quick guides for reproducibility check 

### Open the project

* Clone or download this repository. 
* Open `PSY8960-Fall25-Zhanming.Rproj` in RStudio.

### Data Dictionary

* Dictionary: `/Data/interactive_ads_data_dictionary_20251110.csv`

### Data Simulation

* Script: `/Code/interactive_ads_data_simulation_sample.R`
* Notes: 
  * Run this script line by line to generate simulated data 
  * (Optional) Install packages
  * (Optional) Change the output file name by editing the last line: `write.csv(dat_sim, "./Data/interactive_ads_data_simulated_sample_20251125.csv", row.names = FALSE)`
  * Current simulated data file: `/Data/interactive_ads_data_simulated_sample_20251125.csv` 

### Report Generation

* R Markdown file: `/Reports/interactive_ads_report_papaja_20251212.Rmd`
* Notes: 
  * Make sure the required packages are installed 
  * Installed XQuartz package if “summaryTools” is not working
  * Click Knit 
  * Current output report: `/Reports/interactive_ads_report_papaja_20251212.pdf `
