# ml-paper

All up-to-date data, graphs, and word drafts for this project can be found here: https://ethz-my.sharepoint.com/:f:/r/personal/bscherrer_ethz_ch/Documents/MLPaper?csf=1&web=1&e=MyMruQ

## Overview
This repository contains the code and resources for the ML-Paper project, which focuses on machine learning models applied to demographic and health survey (DHS) data. The project includes data preprocessing, model training, and evaluation for various countries. Results are visualized in various ways: Boxplots, Scatterplots, Variable Importance Plots, Performance Tables, and Country Maps.

## Repository Structure
- **main.R**: Main script to orchestrate the workflow.
-   **dhs_functions.R**: Utility functions for processing data, running models and creating tables and figures
-   **figures_tables.R**: Script for generating figures and tables.
-   **run_countries.R**: Script to process and clean the DHS data 
-   **run_models.R**: Script to execute model training, tuning and evaluation.
-   **short_model_mapping.R**: Script for creating all maps.
-   **Old_files/**: Contains archived scripts and older versions of the project files.

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
3. Follow the workflow in `main.R` to execute the project. (uncomment only the scripts you want to be run, the rest should be commented out)

## Run the models on ETH's Euler cluster
Please have a look at the documentation file **summary_euler.txt** if you wish to run the models via eth's euler cluster. I highly advise you do that as it significantly improves runtime and 

## Contact
For questions or collaboration, please contact [beat.scherrer@unil.ch].

## Appendix: Run the models on ETH's Euler cluster

Find infos on: https://scicomp.ethz.ch/wiki/Getting_started_with_clusters

### 0.) Initialize connection:

#### a) If you work with VSCode:
1. Click on Bottom left button ("Open a remote window") 
2. Click "Connect to host"
3. Enter your Euler url "username@euler.ethz.ch"
4. Enter your PW

#### b) If you don't work with VSCode

**Setup: MobaXterm:**
- Open MobaXterm as Administrator (get spadmin from ETH IT) and open "Session". Go on "SSH". 
- Type for Remote host: "euler.ethz.ch". 
- Specify username: ethzusername. 
- Port you can leave as it is.
- At "bookmark settings" you can define at "session name" the name of the session. 
- Type your password. You get an email and type the access code.

**With Mac:**
- Open terminal. Type: `ssh ethzusername@euler.ethz.ch`
- Type your password. You get an email and type the access code. 
- Type yes to exit

**Setup FileZilla:**
- Open FileZilla. Go to "open site manager". Go to folder "My sites". Click on "new server". Give the server a name. 
- On "General" select for "protocol": "SFTP - SSH File Transfer Protocol" and for "Host": "euler.ethz.ch". 
- "Connection type": "ask for password". You can leave Port empty.  

**Connect and transfer files**
- Open FileZilla and connect with the server which you created before under "open site manager"
- Load files in folder in FileZilla which will be used under home/ethzusername
- Open MobaXterm and connect with Euler: Double-click on euler.ethz.ch and then sign in with your credentials

### 1.) Install R and packages
If you need R, packages need to be installed first. For this, load the program first:
```bash
module spider r/4.4.0
module load gcc/12.2.0
module load r/4.4.0
R
```
Check if R works. Then install packages on R:
```r
install.packages(c("..."))
```
Type yes when it asks to create a new directory for the R packages.
```r
q()
```
To leave R environment.

### 2.) Useful commands

```bash
ls --help                    # Overview of options
module avail                 # Overview of all programs
module spider r              # Overview of R versions installed 
module avail r               # Overview of R versions available
module load r/               # (then click twice tab, to show all available R sessions) 
lquota                       # Shows used RAM
```

### 3.) Switch to new system (if needed)

Should you not have the newest version of the programs, you are probably in an old version of the system. To switch to the new one type the following:
See: https://scicomp.ethz.ch/wiki/New_SPACK_software_stack_on_Euler

```bash
module --version                # Shows version of system to check
module load eth_proxy           # Switches system to the ETH proxy server
```

### 4.) Run scripts

Change to directory and make script executable:
```bash
cd test_euler # name od your root directory
chmod u+x euler_job.sh  
ls -a                           # See all files in current directory
ls -l                           # See all files as list (x = executable)
./euler_job.sh                  # Run the script
squeue -l                       # Shows running jobs
scancel jobID                   # Deletes running job
```

### 5.) Euler rules

- Maximum 48 cores on the same node per user
- Maximum 120 GB RAM on the same node per user
- Maximum runtime must be defined. Depending on the time, your job will be put on a different server with different priority.
  - Levels: 24h, 120h, 15 days
  - 15 days in sh file: `15-00:00:00`
  - 24 hours: `24:00:00`
  - 10 minutes: `00:10:00`

### 6.) Common errors

**Error: "/bin/bash^M: bad interpreter: No such file or directory"**

Solution 1:
```bash
nano xxx.sh                  # Opens text editor (xxx.sh is the filename)
```
Then copy the code from the original file in the new file without the first line "#!/bin/bash", then write the first line by hand in the new file "#!/bin/bash", then Ctrl+O and Enter to save, then Ctrl+X and Enter to close.
```bash
chmod u+x xxx.sh
./xxx.sh
```

Solution 2:
```bash
sed -i -e 's/\r$//' xxx.sh
./xxx.sh
```

### 7.) Special setup for R-package "sf"

You need to run the following command in your terminal:
```bash
module load stack/2024-06 r/4.4.0 cmake/3.27.7 udunits/2.2.28 openssl/3.1.3-zhfub4o gdal/3.4.3 geos/3.9.1 proj/9.2.1 sqlite/3.43.2
```