# Overview #

This documentation describes the sources of data and modeling processes used in the manuscript *Spatially implemented Bayesian network model to assess environmental impacts of water management* submitted to Water Resources Research. In addition, I've provided links to data sources and code scripts used for this study.

I've attempted to make this document as transparent as possible so other researchers can replicate my work, if needed.

## Study Abstract ##

Bayesian networks (BNs) have become popular methods of assessing environmental impacts of water management. However, spatial attributes that influence ecological processes are rarely included in BN models. We demonstrate the benefits of combining two-dimensional hydrodynamic and BN modeling frameworks to explicitly incorporate the spatial variability within a system. The impacts of two diversion scenarios on riparian vegetation recruitment at the Gila River, New Mexico, USA, were evaluated using a coupled modeling framework. We focused on five individual sites in the Upper Gila Basin. Recruitment potential decreased by more than 20% at some locations within each study site, with the largest impacts occurring along fluvial landforms, such as side channel and sand bars. Scenario 1, which restricts diversions when river flows are less than 4.25 cms (150 cfs) resulted in higher reductions in recruitment potential compared to scenario 2. Our unique approach allowed us to evaluate recruitment consequences of water management scenarios at a fine spatial scale, which not only helped differentiate impacts at distinct channel locations, but was useful for informing managers and stakeholders of possible ecological impacts. Our findings also demonstrate that minor changes to river flow may have large ecological implications.

## Modeling Framework ##

This study combined two-dimensional hydrodynamic and Bayesian network models to predict the impact of water diversion scenarios in riparian vegetation recruitment on the Gila River, New Mexico.

The thorough description of the model approach is described in the manuscript submitted to Water Resources Research.

## Data sources ##

### Hydrologic records ###

Hydrologic records from the [U.S. Geological Survey gage 09430500][USGS] were used to represent existing conditions.

Hydrological representations of Scenario 1 and scenario 2 were developed by The Nature Conservancy based on diversion stipulations of the [New Mexico Consumptive Use and Forebearance Agreement][CUFA] (CUFA). Broadly, CUFA stipulates that the maximum diversion from the Gila River by New Mexico is capped at 350 (cfs) at any given time and may not exceed 64,000 acre-feet in any given year or 140,000 acre-feet in any 10-year period.

Text files of the hydrologic time series used for existing conditions and each scenario can be found [here][gitdata] in the Github repository maintained by Ryan Morrison.

### Conditional probability tables ###

Conditional probability tables were developed for the Bayesian network based on information found in the literature, as well as through solicitation with experts during a workshop held December, 2013.

The conditional probability table for each node can be found here.

## R code ##

The Bayesian network was developed using [R][r] and the [bnlearn][bn] package. The scripts necessary for implementing the network are located [here][git] in a Github repository maintained by Ryan Morrison.

The following scripts are required:

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

**1-functions.R**

This script contains custom functions that I developed for the project. A description of each function is given as a comment for each.

**2-hydrology_processing.R**

This script is used to process the existing hydrology (Gila at Gila River USGS gage). Conditional probabilities for inundation and recession rates are calculated in this script.

**3-scenario_processing.R**

Each scenario is processed in this script, similar to the "2-hydrology_processing.R" script.

**4-bayesian_network_construct.R**

This script is used to build and Bayesian network used to infer recruitment potential. Conditional probabilities for each node are populated using results from the "2-hydrology_processing.R" and "3-scenario_processing.R" scripts.

**5a-implement_existing.R**

This script is used to implement the Bayesian network using existing conditions. The BN is implemented on a cell-by-cell basis and instantiated with evidence based on existing conditions.

**5b-implement_scenario1.R**

This script does the same thing as "5a-implement_existing.R" but for scenario 1.

**5c-implement_scenario2.R**

This script does the same thing as "5a-implement_existing.R" but for scenario 2.

**entropy_analysis.R**

This script performs an entropy reduction analysis on the Bayesian network developed using "4-bayesian_network_construct.R".

**site*_output_processing.R**

These scripts process the BN model output for each site, including data aggregation and sorting. Also, they contain the code used to assign the correct geographic projection to the results for spatial plotting.

**results_analysis.R**

This script contains all the R-code used to analyze model results and create figures.








[USGS]: http://waterdata.usgs.gov/usa/nwis/uv?site_no=09430500
[CUFA]: http://www.ose.state.nm.us/PDF/ISC/BasinsPrograms/GilaSanFrancisco/Final-CUFA-Oct27-2005.pdf
[gitdata]: https://github.com/munich1204/bayesian_network/tree/master/data
[bn]: http://www.bnlearn.com
[r]: http://www.r-project.org
[git]: https://github.com/munich1204/bayesian_network