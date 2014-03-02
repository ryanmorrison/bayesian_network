The required scripts are:

- 1-functions.R
- 2-hydrology_processing.R
- 3-scenarios_processing.R
- 4-bayesian_network_construct.R
- 5a-implement_existing.R
- 5b-implement_scenario1.R
- 5c-implement_scanrio2.R
- entropy_analysis.R
- site1_output_processing.R
- site2_output_processing.R
- site3_output_processing.R
- site4_output_processing.R
- site5_output_processing.R
- results_analysis.R

The input data used for these scripts are found in the "data" folder, including the hydrology data for existing conditions and each scenario.

Raw output from the model are contained in the "output" folder. The output data is in ".Rdata" format that can easily be loaded into R for post-processing. The data can be converted to ".txt" format if needed later.

The scripts should be run in the order they are numbers, i.e. run 1-functions.R followed by 2-hydrology_processing.R, and so forth.

Below is a brief description of each script. More information is provided as comments within each script.

## Scripts ##

### 1-functions.R ###

This script contains custom functions that I developed for the project. A description of each function is given as a comment for each.

### 2-hydrology_processing.R ###

This script is used to process the existing hydrology (Gila at Gila River USGS gage). Conditional probabilities for inundation and recession rates are calculated in this script.

### 3-scenario_processing.R ###

Each scenario is processed in this script, similar to the "2-hydrology_processing.R" script.

### 4-bayesian_network_construct.R ###

This script is used to build and Bayesian network used to infer recruitment potential. Conditional probabilities for each node are populated using results from the "2-hydrology_processing.R" and "3-scenario_processing.R" scripts.

### 5a-implement_existing.R ###

This script is used to implement the Bayesian network using existing conditions. The BN is implemented on a cell-by-cell basis and instantiated with evidence based on existing conditions.

### 5b-implement_scenario1.R ###

This script does the same thing as "5a-implement_existing.R" but for scenario 1.

### 5c-implement_scenario2.R ###

This script does the same thing as "5a-implement_existing.R" but for scenario 2.

### entropy_analysis.R ###

This script performs an entropy reduction analysis on the Bayesian network developed using "4-bayesian_network_construct.R".

### site*_output_processing.R ###

These scripts process the BN model output for each site, including data aggregation and sorting. Also, they contain the code used to assign the correct geographic projection to the results for spatial plotting.

### results_analysis.R ###

This script contains all the R-code used to analyze model results and create figures.