# Analysis of Motor Unit Excitability Using Linear Mixed-Effects Models

## Overview
This repository contains the R script and data for analyzing motor unit data from our study on the influence of different protocols of isometric handgrip remote contractions on tibialis anterior motoneuron excitability. The results contribute to a deeper understanding of neuromodulatory mechanisms (e.g., persistent inward currents - PICs) in motor control.
Generated figures have been included in this repository for verification and visualization purposes.

## Repository Structure
- **`Script_PICs_remote_contraction.R`**: Main script for alayzing motor units data previously processed using Linear Mixed-Effects Models.
- **`Data/`**: Contains the input Excel file (`Data_pics_remote_contraction.xlsx`) with preprocessed data used in the analysis.
- **`Figures/`**: Includes output PNG images of *delta F*, *brace height* and *attenuation slope*, providing visual representations of the results.

## How to Run
1. Download and install [RStudio](https://posit.co/download/rstudio-desktop/) (version 2023.06.0+421 or newer).
2. Clone or download this repository and extract the files.
3. Place the `Data/` folder in your working directory or set your working directory to the extracted repository folder using:
   ```R
   setwd("path_to_repository")
4. Open `Script_PICs_remote_contraction.R` in RStudio and execute the script line by line.
5. Ensure all dependencies (listed below) are installed in your R environment.

## Dependencies

This script requires the following R packages, which are already loaded in the script:  
 `readxl`, `naniar`, `visdat`, `dplyr`, `tidyverse`, `ggplot2`, `lmerTest`, `emmeans`, `janitor`, `cowplot`, `lme4`, `car`, `rmcorr`, `psycho`, `sjstats`, `pwr`, `viridis`, `MuMIn`, `merTools`, `broom.mixed`, `ggpp`, `writexl`, `optimx`, `misty`, `magrittr`.

## Contact

For questions or feedback, contact:  
Lucas Campos Ugliara  
PhD candidate, Universidade de Brasília  
📧 Email: lucasugliaraunb@gmail.com  
