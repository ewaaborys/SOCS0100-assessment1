# SOCS0100-assessment1
This folder contains all materials to reproduce the SOCS0100 Assignment 1 report on COVID-19 cases and deaths. The analysis covers data wrangling, functional programming and visualisation tasks using R and Quarto.

## Folder Structure
- `data/`: the raw dataset used in the analysis
- `SOCS0100-report.qmd`: the main Quarto report (source file)
- `SOCS0100-report.html`: the rendered HTML report output
- `SOCS0100_code.R`: the full R script containing all code (no prose)
- `SOCS0100-assessment1.Rproj`: the RStudio project file
- `SOCS0100-report_files/`: automatically generated folder containing report dependencies 
- `README.md`: this documentation file

## Reproducing the Analysis
1. Open RStudio and set the working directory to the project folder  
   (you can simply open the `.Rproj` file).
2. Ensure the dataset is present in the `data` folder
3. Open `SOCS0100-report.qmd` in RStudio and click the "Render" button to generate the report 


##Software Requirements
- R (version 4.0 or higher recommended)
- R packages: tidyverse, lubridate, skimr, glue, gridExtra, scales
library(groundhog)
pkgs <- c("tidyverse", "lubridate", "skimr", "glue", "gridExtra", "scales")
groundhog.library(pkgs, "2025-11-04")

