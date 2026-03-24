# ml-paper

All up-to-date data, graphs, and word drafts for this project can be found here: https://ethz-my.sharepoint.com/:f:/r/personal/bscherrer_ethz_ch/Documents/MLPaper?csf=1&web=1&e=MyMruQ

## Overview
This repository contains the code and resources for the ML-Paper project, which focuses on machine learning models applied to demographic and health survey (DHS) data. The project includes data preprocessing, model training, and evaluation for various countries. Results are visualized in various ways: Boxplots, Scatterplots, Variable Importance Plots, Performance Tables, and Country Maps.

## Repository Structure
- **census_merging.R**: Script for merging census data.
- **dhs_functions.R**: Utility functions for processing data, running models and creating tables and figures
- **figures_tables.R**: Script for generating figures and tables.
- **main.R**: Main script to orchestrate the workflow.
- **run_countries.R**: Script to process and clean the DHS data 
- **run_models.R**: Script to execute model training, tuning and evaluation.
- **short_model_mapping.R**: Script for creating all maps.
- **Old_files/**: Contains archived scripts and older versions of the project files.

## Key Features
- Preprocessing and merging of DHS data.
- Implementation of machine learning models, including XGBoost.
- Country-specific modeling and evaluation.
- Automated generation of figures and tables for analysis.

## Getting Started
1. Clone the repository:
   ```bash
   git clone https://gitlab.ethz.ch/bscherrer/ml-paper.git
   ```
2. Open the R scripts in your preferred R environment (e.g., RStudio).
3. Follow the workflow in `main.R` to execute the project.

## Run the models on ETH's Euler cluster
Please have a look at the documentation file **summary_euler.txt** if you wish to run the models via eth's euler cluster. I highly advise you do that as it significantly improves runtime and 

## Contributing
Contributions are welcome! Please fork the repository and submit a pull request with your changes.

## Contact
For questions or collaboration, please contact [beat.scherrer@unil.ch].