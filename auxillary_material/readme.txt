Auxiliary Material for

Spatially implemented Bayesian network model to assess environmental impacts of water management

Ryan Morrison and Mark Stone

(Department of Civil Engineering, University of New Mexico, Albuquerque, New Mexico)

Water Resources Research

Introduction

Conditional probability tables used to construct the Bayesian network model in this study are included in this auxiliary material. The "timing" node did not contain any parent nodes, and therefore is represented by prior probabilities (ts03.xlsx). The conditional probability tables for the "inundation" (ts01.xlsx) and "recession rate" (ts02.xlsx) nodes were determined using the hydrologic time series for each scenario. Because these probabilities varied for each site, samples are provided from study site #1. Conditional probabilities for other nodes in the network were determined using literature and input from a workshop of riparian experts.

1. ts01.xlsx Conditional probability table for "inundation" node (example from site #1). Values calculated based on time series data for each scenario.

1.1 Column "Inundation", bin for discrete discharge range, 500 cfs discharge increments between 1,000 and 4,000 cfs

1.2 Column "TIMING = Apr-May", "Timing" node in discrete state April - May

1.2.1 Column "YES", conditional probability of "Inundation" state given "Timing" state

1.2.2 Column "NO", conditional probability of "Inundation" state given "Timing" state

1.3 Column "TIMING = Jun-Jul", "Timing" node in discrete state June - July

1.3.1 Column "YES", conditional probability of "Inundation" state given "Timing" state

1.3.2 Column "NO", conditional probability of "Inundation" state given "Timing" state

1.4 Column "TIMING = Aug-Sep", "Timing" node in discrete state August - September

1.4.1 Column "YES", conditional probability of "Inundation" state given "Timing" state

1.4.2 Column "NO", conditional probability of "Inundation" state given "Timing" state


2. ts02.xlsx Conditional probability table for "recession rate" node (example from site #2). Values calculated based on time series data for each scenario. Discrete states for "recession rate" were determined with guidance from Mahoney and Rood [1998], Amlin and Rood [2002], and Stella et al. [2010].

2.1 Column "Recession Rates (cm/day)", average recession rate following inundation event

2.2 Column "TIMING = Apr-May", "Timing" node in discrete state April - May

2.2.1 - 2.2.5 Columns "<0", "0-1", "1-3", "3-6", ">6", conditional probabilities of "Recession Rate" state given "Timing" state

2.3 Column "TIMING = Jun-Jul", "Timing" node in discrete state June - July

2.3.1 - 2.3.5 Columns "<0", "0-1", "1-3", "3-6", ">6", conditional probabilities of "Recession Rate" state given "Timing" state

2.4 Column "TIMING = Aug-Sep", "Timing" node in discrete state August - September

2.4.1 - 2.4.5 Columns "<0", "0-1", "1-3", "3-6", ">6", conditional probabilities of "Recession Rate" state given "Timing" state


3. ts03.xlsx Prior probabilities for "timing" node. Values represent seed availability during a given time range. Discrete states fore "timing" were determined with guidance from Mahoney and Rood [1998], Amlin and Rood [2002], and Shafroth et al. [1998], and well as from advice from workshop riparian experts.

3.1 Column "TIMING", discrete state for "timing" node

3.2 Column "Seed Availability", prior probability of riparian vegetation seed availability


4. ts04.xlsx Conditional probability table for "Hydrologic conditions" node given "recession rate" and "Inundation" nodes.

4.1 Rows "Recession Rate (cm/day)", discrete state of "recession rate" node

4.2 Column "FLOODING = YES", discrete state for "inundation" node

4.2.1 - 4.2.2 Columns "LOW", "HIGH", conditional probabilities of "hydrologic conditions" nodes given "recession rate" and "inundation" nodes

4.3 Column "FLOODING = NO", discrete state for "inundation" node

4.3.1 - 4.3.2 Columns "LOW", "HIGH", conditional probabilities of "hydrologic conditions" nodes given "recession rate" and "inundation" nodes


5. ts05.xlsx Conditional probability table for "Recruitment potential" node given "groundwater depth" and "hydrologic conditions" nodes. "Groundwater depth" states determined using Mahoney and Rood [1998].

5.1 Rows "Depth to Groundwater (cm)", discrete state of "groundwater depth" node

5.2 Column "HYDROLOGIC = LOW", discrete state for "hydrologic conditions" node

5.2.1 - 5.2.2 Columns "YES", "NO", conditional probabilities of "recruitment potential" nodes given "groundwater depth" and "hydrologic condition" nodes

5.3 Column "HYDROLOGIC = HIGH", discrete state for "hydrologic conditions" node

5.3.1 - 5.3.2 Columns "YES", "NO", conditional probabilities of "recruitment potential" nodes given "groundwater depth" and "hydrologic condition" nodes


References

Amlin, N. M., and S. B. Rood (2002), Comparative tolerances of riparian willows and cottonwoods to water-table decline, Wetlands, 22(2), 338–346, doi:10.1672/0277-5212(2002)022[0338:CTORWA]2.0.CO;2.

Mahoney, J. M., and S. B. Rood (1998), Streamflow requirements for cotton- wood seedling recruitment—an integrative model, Wetlands, 18(4), 634–645, doi: 10.1007/BF03161678.

Shafroth, P. B., G. T. Auble, J. C. Stromberg, and D. T. Patten (1998), Establishment of woody riparian vegetation in relation to annual patterns of streamflow, Bill Williams River, Arizona, Wetlands, 18(4), 577–590, doi:10.1007/BF03161674.

Stella, J. C., J. J. Battles, J. R. McBride, and B. K. Orr (2010), Riparian seedling mortality from simulated water table recession, and the design of sustainable flow regimes on regulated rivers, Restor. Ecol., 18(S2), 284–294, doi:10.1111/j.1526-100X.2010.00651.x.



